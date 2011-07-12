module Debug.DeltaDebug (
  -- * Types
  Outcome(..),
  -- * Interface
  ddmin
  ) where

import Control.Monad.State
import Data.Bits
import Data.List.Split ( chunk )
import Data.Trie as T
import Data.BitVector

-- | Representation of the outcomes of the test function when applied
-- to generated reduced test cases.
data Outcome = Fail
               -- ^ The test failed in the expected way
             | Pass
               -- ^ The test succeeded
             | Unresolved
               -- ^ The test provided no information
             deriving (Eq,Show)


type DeltaState a = StateT (Trie Outcome, [a]) IO

extractIndexedData :: [(a, b)] -> [b]
extractIndexedData = map snd

extractIndices :: [(a, b)] -> [a]
extractIndices = map fst

indexedToVector :: [(Int, a)] -> Int -> BitVector
indexedToVector idat len = makeBitVector len True (extractIndices idat)

makeTestVectors :: [(Int, b)] -> Int -> Int -> ([BitVector], [BitVector])
makeTestVectors input vecLen nGroups =
  (map maskFunc bitVectors, map maskFunc complements)
  where
    chunks = chunk nGroups (extractIndices input)
    bitVectors = map (makeBitVector vecLen True) chunks
    complements = map complement bitVectors
    maskFunc = (activeBitMask .&.)
    activeBitMask = indexedToVector input vecLen

updateCache :: BitVector -> [a] -> Outcome -> DeltaState a ()
updateCache testSet indexedList result = do
  (cache, smallestInputSoFar) <- get
  let bytestring = toByteString testSet
      updatedCache = T.insert bytestring result cache
  case result of
    Fail | length indexedList < length smallestInputSoFar -> put (updatedCache, indexedList)
    _ -> put (updatedCache, smallestInputSoFar)

wasTested :: BitVector -> DeltaState a (Maybe Outcome)
wasTested testSet = do
  let bytestring = toByteString testSet
  (cache, _) <- get
  return $ T.lookup bytestring cache

-- | Given an input sequence and a function that can test sub-sequences
-- | for failures, return the minimal failing input as per Zeller 02
ddmin :: Show a => [a] -> ([a] -> IO Outcome) -> IO [a]
ddmin input testFunc = evalStateT (ddmin' initialIndexedInput 2) (T.empty, input)
  where
    inputLen = length input
    initialIndexedInput = zip [0..] input

    testSetToIndexed ts = filter (bitForIndexIsSet ts) initialIndexedInput

    -- | Recursively decompose the input sequence as per the ddmin
    -- algorithm described in Zeller 02.
    ddmin' currentInput nGroups = do
      let (divs, complements) = makeTestVectors currentInput inputLen nGroups
      divRes <- internalTest testFunc initialIndexedInput divs
      complRes <- internalTest testFunc initialIndexedInput complements

      (_, smallestInput) <- get

      case length currentInput == nGroups of
        True -> return smallestInput -- Done
        False -> case divRes of
          Nothing -> case complRes of
            -- Increase granularity
            Nothing -> ddmin' currentInput (min (nGroups * 2) (length currentInput))
            -- Reduce to complement
            Just failingInput -> ddmin' (testSetToIndexed failingInput) (max (nGroups - 1) 2)
          -- Reset granularity
          Just failingInput -> ddmin' (testSetToIndexed failingInput) 2

-- | Run the actual test on the first bitvector it is given.  Returns
-- Just the first failing test OR Nothing if all tests are unresolved
internalTest :: ([a] -> IO Outcome) -> [(Int, a)] -> [BitVector] -> DeltaState a (Maybe BitVector)
internalTest _ _ [] = return Nothing
internalTest testFunc initialIndexedInput (testSet:rest) = do
  previousResult <- wasTested testSet
  case previousResult of
    Nothing -> do
      let indexedList = testSetToList testSet initialIndexedInput
      result <- liftIO $ testFunc indexedList
      updateCache testSet indexedList result
      dispatchResult result
    Just result -> dispatchResult result
  where
    dispatchResult result =
      case result of
        Pass -> internalTest testFunc initialIndexedInput rest
        Unresolved -> internalTest testFunc initialIndexedInput rest
        Fail -> return $ Just testSet

bitForIndexIsSet :: Bits a => a -> (Int, b) -> Bool
bitForIndexIsSet ts (idx, _) = testBit ts idx

testSetToList :: Bits a => a -> [(Int, b)] -> [b]
testSetToList ts initialIndexedInput = extractIndexedData chosenTuples
  where
    chosenTuples = filter (bitForIndexIsSet ts) initialIndexedInput


