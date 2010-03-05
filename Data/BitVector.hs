module Data.BitVector ( BitVector(..)
                      , makeBitVector
                      , toByteString
                      -- , clearBits
                      -- , setBits
                      -- , complementBits
                      ) where

import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString.Num
import Data.List (unfoldr, foldl', groupBy, sort)
import Data.Word

import Debug.Trace

debug = flip trace


data BitVector = BitVector BS.ByteString Int
               deriving (Eq)

instance Show BitVector where
  show = internalShow

instance Num BitVector where
  (BitVector v1 u1) + (BitVector v2 u2) = BitVector (v1 + v2) u1
  (BitVector v1 u1) - (BitVector v2 u2) = BitVector (v1 - v2) u1
  (BitVector v1 u1) * (BitVector v2 u2) = BitVector (v1 * v2) u1
  abs bv = bv
  signum bv = makeBitVector (bitSize bv) True [0]
  fromInteger i = BitVector (fromInteger i) 0

instance Bits BitVector where
  (.&.) = bitOp (.&.)
  (.|.) = bitOp (.|.)
  xor   = bitOp xor
  complement (BitVector v u) = BitVector (complement v) u
  -- Find out which bits are set, add or subtract n from each one, then filter out indices out of bounds.
  shift (BitVector v u) i = BitVector (shift v i) u
  rotate (BitVector v u) i = BitVector (rotate v i) u
  bitSize (BitVector v u) = 8 * (length $ BS.unpack v) - u
  isSigned bv = False

bitOp op (BitVector v1 un1) (BitVector v2 un2) = BitVector (op v1 v2) (min un1 un2)
  -- if (BS.length v1) == (BS.length v2) && (un1 == un2)
  --    then
  --    else error "Bytestrings not equal size"
  -- where newV = v1 `op` v2


internalShow bv0 = concatMap byteToString $ BS.unpack bv
  where bv = maskUnusedBits bv0
byteToString byte = concat $ map f [0..7]
  where f i = if testBit byte i then "1" else "0"

indexGroupToByte baseByte thisGroup = foldl' complementBit baseByte modGroup
  where modGroup = map (\x -> mod x 8) thisGroup

groupBitsByByte bitsSet = groupBy bitsGrouped sortedIndices
  where sortedIndices = sort bitsSet
        bitsGrouped a b = (a `div` 8) == (b `div` 8)


makeBitVector nBits initVal bitsSet =
  if nBits >= 0
    then BitVector bv (8-leftoverBits) -- `debug` (" rev " ++ show bv)
    else error "BitVectors cannot have negative length"
  where baseByte = if initVal then 0 else (-1) :: Word8
        (fullBytes, leftoverBits) = nBits `divMod` 8
        totalBytes = fullBytes + signum leftoverBits

        byteGroups = groupBitsByByte bitsSet
        bv = BS.pack $ reverse $ unfoldr byteAccum (0, byteGroups)
        indexOfGroup (firstIdx:rest) = firstIdx `div` 8
        byteAccum (idx, []) | idx < totalBytes = Just (baseByte, (idx+1, []))
        byteAccum (idx, grp:rest) = if idx == indexOfGroup grp
                                      then Just (indexGroupToByte baseByte grp, (idx+1, rest))
                                      else Just (baseByte, (idx+1, grp:rest))
        byteAccum (idx, _) | idx == totalBytes = Nothing

maskUnusedBits (BitVector v unusedBits) = v .&. bitmask'
  where bitmask = fromInteger $ toInteger $ round (2^unusedBits - 1)
        bitmask' = complement bitmask

-- Need to mask out the high unused bits
-- toByteString :: BitVector -> BS.ByteString
toByteString bv = maskUnusedBits bv
