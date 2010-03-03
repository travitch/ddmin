-- module DeltaDebug ( ddmin
--                   , Outcome(..)
--                   ) where


import Control.Monad.State
import Data.IORef
import qualified Data.ByteString as BS
import Data.Bits
import Data.Trie as T
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
           , Unresolved, Pass, Unresolved, Unresolved, Fail, Fail, Fail, Fail, Fail, Fail, Fail, Fail ]

type Chunk a = [a]

chunkInputs :: [a] -> Int -> [[a]]
chunkInputs inp numberOfChunks = reverse $ chunkInputs' inp []
  where chunkInputs' lst accum =
          let (c, rest) = splitAt chunkSize lst
          in if length lst > chunkSize
             then chunkInputs' rest $ c : accum
             else lst : accum
        chunkSize = ceiling $ (fromIntegral $ length inp) / (fromIntegral numberOfChunks)

makeTestVectors input activeBitMask nGroups = (map maskFunc bitVectors, map maskFunc complements)
  where chunks = chunkInputs input nGroups
        bitVectors = map (makeBitVector (bitSize activeBitMask) True) chunks
        complements = map complement bitVectors
        maskFunc = (activeBitMask .&.)

updateCache testSet result = do
  (cache, smallestInputSoFar) <- get
  put (T.insert (toByteString testSet) result cache, smallestInputSoFar)

-- State is the result cache and the smallest failing input
ddmin :: [a] -> ([a] -> IO Outcome) -> IO [a]
ddmin input testFunc = evalState (ddmin' initActiveBits 2) (T.empty, input)
  where inputLen = length input
        indexedInput = zip [0..] input
        initActiveBits = makeBitVector inputLen False []
        ddmin' activeBits nGroups = do
          let (divs, complements) = makeTestVectors input activeBits nGroups
          res <- internalTest $ divs ++ complements
          (cache, smallestInputSoFar) <- get
          case res of
            Nothing -> if nGroups == inputLen then return smallestInputSoFar else ddmin' activeBits (nGroups * 2)
            Just failingInput -> ddmin' (makeBitVector inputLen True (testSetToList failingInput)) nGroups
        internalTest [] = return Nothing
        internalTest (testSet:rest) = do
          result <- testFunc $ testSetToList testSet
          updateCache testSet result
          case result of
            Pass -> internalTest rest
            Unresolved -> internalTest rest
            Fail -> return $ Just testSet
        testSetToList ts = map (\(idx, val) -> val) chosenTuples
          where chosenTuples = filter (\(idx, val) -> testBit ts idx) indexedInput

f ioref input = do
  putStrLn $ "Testing: " ++ input
  rval <- readIORef ioref
  writeIORef ioref (tail rval)
  return $ head rval

main = do
  let input = [0..20]
  r <- newIORef outcomes
  minimizedInput <- ddmin input (f r)
  putStrLn $ show minimizedInput
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
