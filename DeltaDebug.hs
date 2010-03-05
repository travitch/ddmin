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

updateCache testSet result = do
  (cache, smallestInputSoFar) <- get
  put (T.insert (toByteString testSet) result cache, smallestInputSoFar)


-- | Given an input sequence and a function that can test
-- sub-sequences for failures, return the minimal failing input as per
-- Zeller 02
ddmin :: [a] -> ([a] -> IO Outcome) -> IO [a]
ddmin input testFunc = do
  smallestFailure <- evalStateT (ddmin' initialIndexedInput 2) (T.empty, input)
  return smallestFailure
  where inputLen = length input
        initialIndexedInput = zip [0..] input

        -- | Recursively decompose the input sequence as per the ddmin
        -- algorithm described in Zeller 02.
        ddmin' currentInput nGroups = do
          let (divs, complements) = makeTestVectors currentInput inputLen nGroups
          divRes <- internalTest divs
          complRes <- internalTest complements

          (cache, smallestInputSoFar) <- get

          case divRes of
            Nothing -> case complRes of
                         Nothing -> if nGroups == inputLen
                                       then return smallestInputSoFar -- Done
                                       else ddmin' currentInput (nGroups * 2) -- Increase granularity
                         Just failingInput -> ddmin' (testSetToIndexed failingInput) (nGroups - 1) -- Reduce to complement
            Just failingInput -> ddmin' (testSetToIndexed failingInput) 2 -- Reset granularity

        -- | Run the actual test on the first bitvector it is given.
        -- Returns Just the first failing test OR Nothing if all tests
        -- are unresolved
        internalTest [] = return Nothing
        internalTest (testSet:rest) = do
          result <- liftIO $ testFunc $ testSetToList testSet
          updateCache testSet result
          case result of
            Pass -> internalTest rest
            Unresolved -> internalTest rest
            Fail -> return $ Just testSet

        testSetToList ts = extractIndexedData chosenTuples
          where chosenTuples = filter (bitForIndexIsSet ts) initialIndexedInput
        testSetToIndexed ts = filter (bitForIndexIsSet ts) initialIndexedInput
        bitForIndexIsSet ts (idx, val) = testBit ts idx

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

