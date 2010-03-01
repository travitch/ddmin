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
type Cache = Trie Outcome

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


-- input = [0..20]
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

-- data SearchState = SearchState [BitVector] [BitVector]

makeTestVectors activeBitMask nGroups = (map maskFunc bitVectors, map maskFunc complements)
  where chunks = chunkInputs input nGroups
        bitVectors = map (makeBitVector (bitSize activeBitMask) True) chunks
        complements = map complement bitVectors
        maskFunc = (activeBitMask .&.)

-- State is the result cache and the smallest failing input
ddmin testFunc input = evalState (ddmin' initActiveBits 2) (T.empty, input)
  where inputLen = length input
        initActiveBits = makeBitVector inputLen False []
        ddmin' activeBits nGroups = do
          let (divs, complements) = makeTestVectors activeBits nGroups
          res <- internalTest $ append divs complements
          (cache, smallestInputSoFar) <- get
          case res of
            -- Unresolved
            Nothing -> if nGroups == inputLen
                         -- Done
                         then return smallestInputSoFar
                         -- Increase granularity
                         else ddmin' activeBits (nGroups * 2)
            Just failingInput -> ddmin' (makeBitVector inputLen True failingInput) nGroups
        internalTest [] = return Nothing
        internalTest (testSet:rest) = do
          -- TODO: Convert testSet to a list of the inputs
          case testFunc testSet of
            Pass -> internalTest rest
            Unresolved -> internalTest rest
            Fail -> return $ Just testSet


main = do
  minimizedInput <- ddmin f [0..20]
  -- let initialActive = makeBitVector (length input) False []
  -- let (divs, compls) = makeTestVectors (makeBitVector (length input) False []) 4
  -- putStrLn $ show divs
  -- putStrLn $ show compls

  -- let mask2 = head $ tail compls
  --     (divs2, compls2) = makeTestVectors mask2 4

  -- putStrLn $ "Mask: " ++ show mask2
  -- putStrLn $ show divs2
  -- putStrLn $ show compls2

  -- let initActivePieces = makeBitVector (length input) False []
  --     initialChunks = chunkInputs input 2
  --     initBitVectors = map (makeBitVector (length input) False) initialChunks
  -- putStrLn $ show initActivePieces
  -- putStrLn $ show $ chunkInputs input 5
  -- putStrLn $ show initBitVectors
  -- putStrLn $ show $ map complement initBitVectors
