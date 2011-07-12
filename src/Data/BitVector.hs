-- | This module defines a BitVector in terms of ByteStrings.  It uses
-- Num and Bits instances adapted from the NumLazyByteString pakage.
--
-- The implementations are currently fairly fragile and only behave
-- properly if given inputs of the same size.  Some operations are
-- only barely implemented.  It is basically only suitable for use in
-- this delta debugging package.
module Data.BitVector (
  -- * Types
  BitVector(..),
  -- * Constructors
  makeBitVector,
  toByteString
  ) where

import Data.Bits
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import Data.ByteString.Num ()
import Data.List ( unfoldr, foldl', groupBy, sort )
import Data.Word

-- | BitVectors use a 'ByteString' for their underlying
-- representation.  The extra Int tracks the number of bits used in
-- the last byte of the ByteString (not all bits are necessarily
-- used).
data BitVector = BitVector !ByteString !Int
               deriving (Eq)

instance Show BitVector where
  show = internalShow

-- | This instance is very sloppy and does not do the right thing if
-- the BitVectors are different lengths.
instance Num BitVector where
  (BitVector v1 u1) + (BitVector v2 _) = BitVector (v1 + v2) u1
  (BitVector v1 u1) - (BitVector v2 _) = BitVector (v1 - v2) u1
  (BitVector v1 u1) * (BitVector v2 _) = BitVector (v1 * v2) u1
  abs bv = bv
  signum bv = makeBitVector (bitSize bv) True [0]
  fromInteger i = BitVector (fromInteger i) 0

-- | This one is also sloppy
instance Bits BitVector where
  (.&.) = bitOp (.&.)
  (.|.) = bitOp (.|.)
  xor   = bitOp xor
  testBit (BitVector v _) = testBit v
  complement (BitVector v u) = BitVector (complement v) u
  -- Find out which bits are set, add or subtract n from each one, then filter out indices out of bounds.
  shift (BitVector v u) i = BitVector (shift v i) u
  rotate (BitVector v u) i = BitVector (rotate v i) u
  bitSize (BitVector v u) = 8 * length (BS.unpack v) - u
  isSigned _ = False

bitOp :: (ByteString -> ByteString -> ByteString) -> BitVector -> BitVector -> BitVector
bitOp op (BitVector v1 u1) (BitVector v2 u2) = BitVector newV newU
  where
    newV = v1 `op` v2
    newU = if BS.length v1 > BS.length v2 then u1 else u2

internalShow :: BitVector -> String
internalShow bv0 = concatMap byteToString $ BS.unpack bv
  where
    bv = maskUnusedBits bv0

byteToString :: Bits a => a -> String
byteToString byte = concatMap f [0..7]
  where
    f i = if testBit byte i then "1" else "0"

indexGroupToByte :: Bits a => a -> [Int] -> a
indexGroupToByte baseByte thisGroup = byte
  where
    modGroup = map (`mod` 8) thisGroup
    byte = foldl' complementBit baseByte modGroup

groupBitsByByte :: Integral a => [a] -> [[a]]
groupBitsByByte bitsSet = groupBy bitsGrouped sortedIndices
  where
    sortedIndices = sort bitsSet
    bitsGrouped a b = (a `div` 8) == (b `div` 8)

makeBitVector :: Int -> Bool -> [Int] -> BitVector
makeBitVector nBits initVal bitsSet =
  if nBits >= 0
    then BitVector bv (8-leftoverBits)
    else error "BitVectors cannot have negative length"
  where
    baseByte = if initVal then 0 else -1 :: Word8
    (fullBytes, leftoverBits) = nBits `divMod` 8
    totalBytes = fullBytes + signum leftoverBits

    byteGroups = groupBitsByByte bitsSet
    bv = BS.pack byteList
    indexOfGroup (firstIdx:_) = firstIdx `div` 8
    byteAccum (idx, []) | idx < totalBytes = Just (baseByte, (idx+1, []))
    byteAccum (idx, grp:rest) = case idx == indexOfGroup grp of
      True -> Just (indexGroupToByte baseByte grp, (idx+1, rest))
      False -> Just (baseByte, (idx+1, grp:rest))
    byteAccum (idx, _) | idx == totalBytes = Nothing
    byteList = unfoldr byteAccum (0, byteGroups)

maskUnusedBits :: BitVector -> ByteString
maskUnusedBits (BitVector v unusedBits) =
  case unusedBits == 0 of
    True -> v
    False -> v .&. bitmask
  where
    bitmask = BS.pack $ reverse $ indexGroupToByte 0 [0..(8-unusedBits-1)] : baseBS
    baseBS = replicate (BS.length v - 1) (-1)

-- Need to mask out the high unused bits
toByteString :: BitVector -> ByteString
toByteString = maskUnusedBits
