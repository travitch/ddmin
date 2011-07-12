{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.ByteString.Num
    (numCompare
    ) where

import qualified Data.ByteString as L
import Data.ByteString.Class
import Data.Bits
import Data.Binary (encode)
import Data.List (foldl', mapAccumL)
import Data.Word

-- Little Endian ByteStrings
instance Num L.ByteString where
    a + b = byteStrOp (+) a b
    a * b = L.take (max (L.length a) (L.length b)) (go (L.unpack a) (L.unpack b))
      where
      go as bs =
         let cs = zip [0..] as
             wordMult :: [[Word8]]
             wordMult = map ((\f -> f bs) . doMult) cs
         in foldl' (\a b -> a + L.pack b) (L.pack [0]) wordMult
      doMult :: (Int, Word8) -> [Word8] -> [Word8]
      doMult (i,a) b = replicate i 0 ++ byteMult a b 0
      byteMult :: Word8 -> [Word8] -> Int -> [Word8]
      byteMult _ [] c = [fromIntegral c]
      byteMult a (b:bs) c =
          let (q,r) = quotRem (fromIntegral a * fromIntegral b + c) 256
          in fromIntegral r : byteMult a bs q
    a - b = byteStrOp (-) a b
    negate a = L.replicate (L.length a) 0 - a
    abs a = a
    signum a = a `seq` L.pack [1]
    fromInteger i = L.pack $ go i []
      where
      go :: Integer -> [Word8] -> [Word8]
      go i acc | i < 0 = L.unpack $ (L.pack [0]) - (L.pack $ go (-i) acc)
      go 0 acc = reverse acc
      go i acc =
          let (c,r) = divMod i 256
          in go c (fromIntegral r:acc)

instance Integral L.ByteString where
    quot = asInteger2 quot
    rem = asInteger2 rem
    div = asInteger2 div
    mod = asInteger2 mod
    quotRem a b =
        let (x,y) = quotRem (fromIntegral a) (fromIntegral b :: Integer)
        in (fromIntegral x, fromIntegral y)
    divMod a b =
        let (x,y) = divMod (fromIntegral a) (fromIntegral b :: Integer)
        in (fromIntegral x, fromIntegral y)
    toInteger a = snd $ foldl' acc (0,0) (L.unpack a)
      where
      acc :: (Integer, Integer) -> Word8 -> (Integer, Integer)
      acc (i, tot) n = (i+1, tot + (fromIntegral n `shiftL` fromIntegral (i*8)))
      -- FIXME use of 'Int' in 'shiftL' causes a maxbound issue

instance Real L.ByteString where
    toRational = toRational . fromIntegral

instance Enum L.ByteString where
    succ a = if isMaxBound a then error "succ maxBound" else a + 1
    pred a = if isMinBound a then error "pred minBound" else a - 1
    toEnum i = toStrictByteString $ encode i
    fromEnum = fromIntegral
    enumFrom a = if isMaxBound a then [a] else a : (enumFrom (succ a))
    enumFromThen start cnt = normalized go start cnt
        where
        go s c =
          if s > (L.replicate (L.length c) 0xFF) - c
            then [s]
            else s : (enumFromThen (s+c) c)
    enumFromTo start end = normalized go start end
      where go s e | numCompare s e == GT = []
                   | otherwise =
                       if isMaxBound s then [s] else s : enumFromTo (succ s) e
    enumFromThenTo s c e =
        takeWhile (\x -> numCompare x e /= GT) (enumFromThen s c)

isMaxBound :: L.ByteString -> Bool
isMaxBound = L.all (== 0xFF)

isMinBound :: L.ByteString -> Bool
isMinBound = L.all (== 0x0)

-- instance Ord L.ByteString where
numCompare a b =
      let byteCmp = normalized (\x y -> reverse (L.zipWith compare x y)) a b
      in case dropWhile (== EQ) byteCmp of
           (LT:_) -> LT
           (GT:_) -> GT
           _      -> EQ

byteStrOp :: (Int -> Int -> Int) -> L.ByteString -> L.ByteString -> L.ByteString
byteStrOp op a b =
    let (c,ws) = mapAccumL (combWords op) 0 (L.zip a' b')
    in L.pack ws
  where
  (a',b') = normalize a b

combWords :: (Int -> Int -> Int) -> Int -> (Word8,Word8) -> (Int, Word8)
combWords op carry (a,b) = (c, fromIntegral r)
  where
  p :: Int
  p = (fromIntegral a `op` fromIntegral b) + carry
  (c,r) = quotRem p 256

normalized :: (L.ByteString -> L.ByteString -> a) ->  -- The op
              L.ByteString -> L.ByteString -> a   -- lps to normalize
normalized op a b = let (a', b') = normalize a b in op a' b'

normalize :: L.ByteString -> L.ByteString -> (L.ByteString, L.ByteString)
normalize a b = (a',b')
  where
  aPad = L.replicate (L.length b - L.length a) 0
  bPad = L.replicate (L.length a - L.length b) 0
  a' = L.append a aPad
  b' = L.append b bPad

asInteger :: (Integer -> Integer) -> L.ByteString -> L.ByteString
asInteger op = fromIntegral . op . fromIntegral

asInteger2 :: (Integer -> Integer -> Integer) ->
              L.ByteString -> L.ByteString -> L.ByteString
asInteger2 op a b = fromIntegral $ fromIntegral a `op` fromIntegral b

instance Bits L.ByteString where
    (.&.) = normalized (byteStrOp (.&.))
    (.|.) = normalized (byteStrOp (.|.))
    xor = normalized (byteStrOp xor)
    complement =  L.map complement
    a `shift` i = asInteger (`shift` i) a
    a `rotate` i = asInteger (`rotate` i) a
    bit i =
        let (d,m) = i `quotRem` 8
        in L.snoc (L.replicate (fromIntegral d) 0) (bit m)
    setBit a i = asInteger (`setBit` i) a
    clearBit a i = asInteger (`clearBit` i) a
    complementBit a i = asInteger (`complementBit` i) a
    testBit a i =
        let (d, m) = i `quotRem` 8
        in testBit (L.index a d) m
{-case L.unpack (L.drop (fromIntegral d) a) of
             []    -> False
             (w:_) -> testBit w m
-}
    bitSize a = 8 * fromIntegral (L.length a)
    isSigned _ = False
    shiftL = shift
    shiftR a i = shift a (-i)
    rotateL = rotate
    rotateR a i = rotate a (-i)
