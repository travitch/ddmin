module Data.BitVector ( BitVector(..)
                      , makeBitVector
                      , toByteString
                      , clearBits
                      , setBits
                      -- , complementBits
                      ) where

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Vector as V
import Data.Word

import Debug.Trace

debug = flip trace

-- The Int is the number of UNUSED bits in the last byte.
-- It is easier to calculate this once.
data BitVector = BitVector (V.Vector Word8) Int
               deriving (Eq)

instance Show BitVector where
  show = internalShow

instance Num BitVector where
  (BitVector v1 un1) + (BitVector v2 un2) = error "+ on BitVectors not supported"
  (BitVector v1 un1) - (BitVector v2 un2) = error "- on BitVectors not supported"
  (BitVector v1 un1) * (BitVector v2 un2) = error "* on BitVectors not supported"
  abs bv = bv
  signum bv = makeBitVector (bitSize bv) True [0]
  fromInteger i = error "fromInteger on BitVectors not supported"

instance Bits BitVector where
  (.&.) = bitOp (.&.)
  (.|.) = bitOp (.|.)
  xor   = bitOp xor
  complement (BitVector v1 un1) = BitVector (V.map complement v1) un1
  -- Find out which bits are set, add or subtract n from each one, then filter out indices out of bounds.
  shift = internalShift
  rotate = internalRotate
  bitSize (BitVector v1 un1) = 8 * V.length v1 - un1
  isSigned bv = False

byteToString byte = concat $ map f [0..7]
  where f i = if testBit byte i then "1" else "0"

internalShow bv@(BitVector v unused) = concat $ V.toList $ V.imap f v
  where f idx byte = (byteToString $ indexBitVector bv idx)

bitOp op (BitVector v1 un1) (BitVector v2 un2) =
  if (V.length v1) == (V.length v2) && (un1 == un2)
     then BitVector newV un1
     else error "Bytestrings not equal size"
  where newV = V.zipWith op v1 v2

internalShift bv@(BitVector v unused) n = makeBitVector (bitSize bv) True filteredBits
  where initialBitsSet = bitsSet bv
        newBitsSet = map (+n) initialBitsSet
        filteredBits = filter indexOutOfBounds newBitsSet
        indexOutOfBounds idx = idx < 0 || idx >= bitSize bv

internalRotate bv@(BitVector v unused) n = makeBitVector nBits True newBitsSet
  where initialBitsSet = bitsSet bv
        newBitsSet = map (\x -> mod x nBits) $ map (+n) initialBitsSet
        nBits = bitSize bv

unusedBitsMask :: BitVector -> Word8
unusedBitsMask (BitVector v unused) = (shiftR (-1) unused)

bitsUsedInByte :: Word8 -> [Int]
bitsUsedInByte byte = filter (testBit byte) [0..7]

indexBitVector bv@(BitVector v unused) idx =
  if idx == lastIndex then byte .&. unusedBitsMask bv else byte
  where byte = v V.! idx
        lastIndex = V.length v - 1

bitsSet :: BitVector -> [Int]
bitsSet bv@(BitVector v unused) = concat $ V.toList $ V.imap f v
-- Shift bitsUsedInByte up since they won't generally be starting at index zero
  where f idx byte = map (+(8*idx)) $ bitsUsedInByte $ indexBitVector bv idx

-- | Make a bit vector of size nBits with the bits at the provided indices set to initVal
-- | The other bits are !initVal
makeBitVector :: Int -> Bool -> [Int] -> BitVector
makeBitVector nBits initVal bits =
  if nBits >= 0
    then BitVector bv (8-leftover) -- `debug` (" rev " ++ show bv)
    else error "BitVectors cannot have negative length"
  where baseByte = if initVal then 0 else (-1)
        (wholeBytes, leftover) = nBits `divMod` 8
        totalBytes = wholeBytes + signum leftover

        sortedIndices = L.sort bits
        pred a b = (a `div` 8) == (b `div` 8)
        byteIndexGroups = L.groupBy pred sortedIndices

        -- Groups are all non-empty
        byteClassifier idxs@(item:group) = (item `div` 8, idxs)
        byteClassifier [] = error "There should be no empty byte groups"
        byteIndexGroupPositions = map byteClassifier byteIndexGroups

        bv = indicesToBytes totalBytes baseByte byteIndexGroupPositions

indexGroupToByte baseByte thisGroup = L.foldl' complementBit baseByte modGroup
  where modGroup = map (\x -> mod x 8) thisGroup

indicesToBytes totalBytes baseByte groups = indicesToBytes' 0 groups []
  where indicesToBytes' currentIdx [] acc
        -- Done
          | currentIdx == totalBytes = V.fromList $ reverse acc -- `debug` ("final " ++ show acc)
        -- Pad with default bytes
          | otherwise = indicesToBytes' (currentIdx+1) [] (baseByte:acc)
        indicesToBytes' currentIdx remainingGroups@((groupIdx, thisGroup):rest) acc
        -- Some bits in this byte need to be flipped
          | groupIdx == currentIdx = indicesToBytes' (currentIdx+1) rest (thisByte:acc) -- `debug` ("eq " ++ show (thisByte:acc))
        -- Default byte here since nothing needs to be flipped
          | groupIdx > currentIdx = indicesToBytes' (currentIdx+1) remainingGroups (baseByte:acc)
            where thisByte = indexGroupToByte baseByte thisGroup

-- Need to mask out the high unused bits
toByteString :: BitVector -> BS.ByteString
toByteString bv@(BitVector v unusedBits) =
  if unusedBits == 0
    then BS.pack $ V.toList v
    else BS.pack $ V.toList maskedVector
    where maskedVector = V.snoc (V.init v) lastByte
          lastByte = (unusedBitsMask bv) .&. (V.last v)


-- | Clear all of the provided bits at once
clearBits :: BitVector -> [Int] -> BitVector
clearBits bv idxs = bv .&. mask
  where mask = makeBitVector (bitSize bv) False idxs

-- | Set all of the provided bits at once
setBits :: BitVector -> [Int] -> BitVector
setBits bv idxs = bv .|. mask
  where mask = makeBitVector (bitSize bv) True idxs

-- -- | Complement all of the provided bits at once
-- complementBits :: BitVector -> [Int] -> BitVector
-- complementBits (BitVector v un) bits = 0
--   where sortedIndices = sort bits
