-- module DeltaDebug ( ddmin
--                   , Outcome(..)
--                   ) where

-- import Data.BitSet
import qualified Data.ByteString as BS
import Data.Bits
import Data.Trie
import Data.BitVector
import Data.Word

data Outcome = Fail
             | Pass
             | Unresolved

-- type DeltaState = V.Vector
-- type Cache = Trie Outcome

-- Outer driver should have three modes:
-- * Top-level C/C++ decls
-- * line-by-line
-- * whitespace tokens
-- * characters

-- Store deltas as a bytestring and write a layer for flipping a list of bits
-- Convert to a bytestring to store in a trie (bytestring-trie)

-- | Given deltas and a testing function, return the minimum input
-- ddmin :: [String] -> ([String] -> IO Outcome) -> IO [String]
-- ddmin deltas tester = ddmin' vec0 cache0 2
--     where len     = length deltas
--           vec0    = V.replicate len 1 -- Start with all deltas in the input
--           cache0  = empty
--           deltas' = zip [0..] deltas
--           ddmin' vec cache nsubsets = do
--             return len


input = [1..20]
outcomes = [ Unresolved, Unresolved
           , Unresolved, Pass, Pass, Unresolved, Unresolved, Fail
           , Unresolved, Pass, Unresolved, Unresolved, Fail ]

type Chunk a = [a]

chunkInputs :: [a] -> Int -> [[a]]
chunkInputs inp numberOfChunks = reverse $ chunkInputs' inp []
  where chunkInputs' lst accum =
          let (c, rest) = splitAt chunkSize lst
          in if length lst > chunkSize
             then chunkInputs' rest $ c : accum
             else lst : accum
        chunkSize = ceiling $ (fromIntegral $ length inp) / (fromIntegral numberOfChunks)

bitOp op bs1 bs2 =
  if (BS.length bs1) == (BS.length bs2)
     then BS.unfoldrN (BS.length bs1) f 0
     else error "Bytestrings not equal size"
  where f idx = Just ((BS.index bs1 idx) `op` (BS.index bs2 idx), idx + 1)

-- BE CAREFUL: Need to wrap this in another type that records how many
-- bits in the last byte are NOT actually in use.  The completely
-- unused bits need to be masked out after bitXor and bitComplement
-- Declare a Bits instance?  Implemented correctly, that would leave
-- these functions alone.

-- data BitString = BitString BS.ByteVector Int

-- bitAnd = bitOp (.&.)
-- bitOr  = bitOp (.|.)
-- bitXor = bitOp xor
-- bitComplement = BS.map complement

-- Create an initial bytestring with (length input) ones (padded by trailing zeroes)

-- ddmin deltas (outcome:outcomes) activeChunkMask

-- makeInitialBytestring inputs = BS.snoc wholeBytes lastByte
--   where (nBytes, extra) = (length inputs) `divMod` nBits
--         nBits = bitSize (0::Word8)
--         lastByte = foldr f (complement 0) [0..(nBits - extra - 1)]
--         f idx w = clearBit w idx
--         wholeBytes = BS.replicate nBytes (complement 0)

bv1 = makeBitVector 10 [3, 5, 8] True

main = do
  -- let initActivePieces = makeInitialBytestring input
  -- putStrLn $ show $ initActivePieces
  putStrLn $ show $ chunkInputs input 5
  putStrLn $ show bv1

