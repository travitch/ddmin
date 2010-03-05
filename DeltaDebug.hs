-- module DeltaDebug ( ddmin
--                   , Outcome(..)
--                   ) where


import Control.Monad.State
import Data.IORef
import qualified Data.ByteString.Lazy as BS
import Data.Bits
import Data.Trie as T
import Data.BitVector
import Data.Word

import Debug.Trace

debug = flip trace


data Outcome = Fail
             | Pass
             | Unresolved

-- Outer driver should have three modes:
-- * Top-level C/C++ decls
-- * line-by-line
-- * whitespace tokens
-- * characters

outcomes = [ Unresolved, Unresolved
           , Unresolved, Pass, Pass, Unresolved, Unresolved, Fail
           , Unresolved, Pass, Unresolved, Unresolved, Fail, Fail, Fail, Fail, Fail, Fail, Fail, Fail ]

chunkInputs :: [a] -> Int -> [[a]]
chunkInputs inp numberOfChunks = reverse $ chunkInputs' inp []
  where chunkInputs' lst accum =
          let (c, rest) = splitAt chunkSize lst
          in if length lst > chunkSize
             then chunkInputs' rest $ c : accum
             else lst : accum
        chunkSize = ceiling $ (fromIntegral $ length inp) / (fromIntegral numberOfChunks)

extractIndexedData idat = map (\(idx, dat) -> dat) idat
extractIndices idat = map (\(idx, dat) -> idx) idat
indexedToVector idat len = makeBitVector len True (extractIndices idat)

makeTestVectors input vecLen nGroups = (map maskFunc bitVectors, map maskFunc complements)
  where chunks = chunkInputs (extractIndices input) nGroups
        bitVectors = map (makeBitVector vecLen True) chunks
        complements = map complement bitVectors
        maskFunc = (activeBitMask .&.)
        activeBitMask = indexedToVector input vecLen

updateCache testSet indexedList result = do
  (cache, smallestInputSoFar) <- get
  let bytestring = toByteString testSet
      updatedCache = T.insert bytestring result cache
  case result of
    Fail | length indexedList < length smallestInputSoFar -> put (updatedCache, indexedList) `debug` ("  Updating cache with " ++ show indexedList)
    _ -> put (updatedCache, smallestInputSoFar) `debug` "  Updating cache with smallest input remaining the same"


wasTested :: BitVector -> StateT (Trie Outcome, [a]) IO (Maybe Outcome)
wasTested testSet = do
  let bytestring = toByteString testSet
  (cache, si) <- get
  return $ T.lookup bytestring cache

-- | Given an input sequence and a function that can test
-- sub-sequences for failures, return the minimal failing input as per
-- Zeller 02
ddmin :: Show a => [a] -> ([a] -> IO Outcome) -> IO [a]
ddmin input testFunc = do
  smallestFailure <- evalStateT (ddmin' initialIndexedInput 2) (T.empty, input)
  return smallestFailure
  where inputLen = length input
        initialIndexedInput = zip [0..] input

        testSetToIndexed ts = filter (bitForIndexIsSet ts) initialIndexedInput
        bitForIndexIsSet ts (idx, val) = testBit ts idx

        -- | Recursively decompose the input sequence as per the ddmin
        -- algorithm described in Zeller 02.
        ddmin' currentInput nGroups = do
          let (divs, complements) = makeTestVectors currentInput inputLen nGroups
          divRes <- internalTest testFunc initialIndexedInput divs
          complRes <- internalTest testFunc initialIndexedInput complements

          (cache, smallestInput) <- get

          liftIO $ putStrLn $ show (divRes, complRes)
          liftIO $ putStrLn $ show (currentInput, nGroups)

          if length currentInput == nGroups
             then return smallestInput -- Done
             else case divRes of
                   Nothing -> case complRes of
                                Nothing -> ddmin' currentInput (min (nGroups * 2) (length currentInput)) -- Increase granularity
                                Just failingInput -> ddmin' (testSetToIndexed failingInput) (max (nGroups - 1) 2) -- Reduce to complement
                   Just failingInput -> ddmin' (testSetToIndexed failingInput) 2 -- Reset granularity

-- | Run the actual test on the first bitvector it is given.  Returns
-- Just the first failing test OR Nothing if all tests are unresolved
internalTest testFunc initialIndexedInput [] = return Nothing
internalTest testFunc initialIndexedInput (testSet:rest) = do
  previousResult <- wasTested testSet
  testIfNecessary previousResult testSet rest

  where testIfNecessary Nothing testSet rest = do
          result <- liftIO $ testFunc $ indexedList `debug` (" TS: " ++ show testSet)
          updateCache testSet indexedList result
          dispatchResult result testSet rest
          where indexedList = testSetToList testSet
        testIfNecessary (Just result) testSet rest = dispatchResult result testSet rest
        dispatchResult result testSet rest =
          case result of
            Pass -> internalTest testFunc initialIndexedInput rest
            Unresolved -> internalTest testFunc initialIndexedInput rest
            Fail -> return $ Just testSet

        bitForIndexIsSet ts (idx, val) = testBit ts idx
        testSetToList ts = extractIndexedData chosenTuples
          where chosenTuples = filter (bitForIndexIsSet ts) initialIndexedInput

f ioref input = do
  putStrLn $ "Testing: " ++ show input
  rval <- readIORef ioref
  writeIORef ioref (tail rval)
  return $ head rval

main = do
  let input = [0..20]
  r <- newIORef outcomes
  minimizedInput <- ddmin input (f r)
  putStrLn $ show minimizedInput

