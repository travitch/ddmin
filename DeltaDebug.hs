module DeltaDebug ( ddmin
                  , Outcome(..)
                  ) where

import Control.Monad.State
import qualified Data.ByteString.Lazy as BS
import Data.Bits
import Data.List.Split (chunk)
import Data.Trie as T
import Data.BitVector
import Data.Word

-- import Debug.Trace
-- debug = flip trace

data Outcome = Fail | Pass | Unresolved
             deriving (Eq,Show)

extractIndexedData = map snd
extractIndices = map fst
indexedToVector idat len = makeBitVector len True (extractIndices idat)

makeTestVectors input vecLen nGroups = (map maskFunc bitVectors, map maskFunc complements)
  where chunks = chunk nGroups (extractIndices input)
        bitVectors = map (makeBitVector vecLen True) chunks
        complements = map complement bitVectors
        maskFunc = (activeBitMask .&.)
        activeBitMask = indexedToVector input vecLen

updateCache testSet indexedList result = do
  (cache, smallestInputSoFar) <- get
  let bytestring = toByteString testSet
      updatedCache = T.insert bytestring result cache
  case result of
    Fail | length indexedList < length smallestInputSoFar -> put (updatedCache, indexedList)
    _ -> put (updatedCache, smallestInputSoFar)

wasTested :: BitVector -> StateT (Trie Outcome, [a]) IO (Maybe Outcome)
wasTested testSet = do
  let bytestring = toByteString testSet
  (cache, si) <- get
  return $ T.lookup bytestring cache

-- | Given an input sequence and a function that can test sub-sequences
-- | for failures, return the minimal failing input as per Zeller 02
ddmin :: Show a => [a] -> ([a] -> IO Outcome) -> IO [a]
ddmin input testFunc = evalStateT (ddmin' initialIndexedInput 2) (T.empty, input)
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

          if length currentInput == nGroups
             then return smallestInput -- Done
             else case divRes of
                   Nothing -> case complRes of
                                -- Increase granularity
                                Nothing -> ddmin' currentInput (min (nGroups * 2) (length currentInput))
                                -- Reduce to complement
                                Just failingInput -> ddmin' (testSetToIndexed failingInput) (max (nGroups - 1) 2)
                   -- Reset granularity
                   Just failingInput -> ddmin' (testSetToIndexed failingInput) 2

-- | Run the actual test on the first bitvector it is given.  Returns
-- Just the first failing test OR Nothing if all tests are unresolved
internalTest testFunc initialIndexedInput [] = return Nothing
internalTest testFunc initialIndexedInput (testSet:rest) = do
  previousResult <- wasTested testSet
  testIfNecessary previousResult testSet rest

  where testIfNecessary Nothing testSet rest = do
          result <- liftIO $ testFunc indexedList
          updateCache testSet indexedList result
          dispatchResult result testSet rest
          where
            indexedList = testSetToList testSet initialIndexedInput
        testIfNecessary (Just result) testSet rest = dispatchResult result testSet rest
        dispatchResult result testSet rest =
          case result of
            Pass -> internalTest testFunc initialIndexedInput rest
            Unresolved -> internalTest testFunc initialIndexedInput rest
            Fail -> return $ Just testSet

bitForIndexIsSet ts (idx, val) = testBit ts idx
testSetToList ts initialIndexedInput = extractIndexedData chosenTuples
  where
    chosenTuples = filter (bitForIndexIsSet ts) initialIndexedInput


