{-# LANGUAGE  OverloadedStrings #-}
import Control.Exception ( bracket )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import System.Directory
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text
import System.Exit
import System.FilePath
import System.IO ( hClose, withFile, openTempFile, IOMode(..), Handle )
import System.Process
import Text.Regex.PCRE hiding ( empty )

import Debug.DeltaDebug
import Debug.DeltaDebug.InputStrategies

addArg :: String -> Config -> Either String Config
addArg s c = Right c { cmdLine = cmdLine c ++ [s] }

setInputFile :: String -> Config -> Either String Config
setInputFile s c = Right c { inputFile = s }

setOutputFile :: String -> Config -> Either String Config
setOutputFile s c = Right c { savedOutput = Just s }

setOutput :: String -> Config -> Either String Config
setOutput "both" c = Right c { searchOutput = Both }
setOutput "stdout" c = Right c { searchOutput = StdOut }
setOutput "stderr" c = Right c { searchOutput = StdErr }
setOutput s _ = Left $ "Invalid output type: " ++ s

setRegex :: String -> Config -> Either String Config
setRegex s c = Right c { searchRegex = s }

setHelp :: Config -> Config
setHelp c = c { isHelp = True }

arguments :: Mode Config
arguments = mode "ddmin" defaultConfig desc (flagArg addArg "COMMAND") as
  where
    desc = "A Haskell implementation of the ddmin algorithm"
    as = [ flagReq ["search-output"] setOutput "OUTPUT-TYPE" "The output stream to search: stdout, stderr, or both.  Default: both"
         , flagReq ["search-regex"] setRegex "REGEX" "The regex to search with (required)"
         , flagReq ["input-file"] setInputFile "FILE" "The initial input file (required)"
         , flagReq ["save-output"] setOutputFile "FILE" "The file in which to save the smallest output"
         , flagHelpSimple setHelp
         ]

data OutputType = StdErr
                | StdOut
                | Both
                deriving (Show)
data Config = Config { searchOutput :: OutputType
                     , searchRegex :: String
                     , inputFile :: FilePath
                     , savedOutput :: Maybe String
                     , cmdLine :: [String]
                     , isHelp :: Bool
                     } deriving (Show)

defaultConfig :: Config
defaultConfig = Config { searchOutput = Both
                       , searchRegex = error "A search regex must be specified"
                       , inputFile = error "An input file must be specified"
                       , savedOutput = Nothing
                       , cmdLine = []
                       , isHelp = False
                       }

getTestExecutable :: Config -> String
getTestExecutable cfg =
  case cmdLine cfg of
    exe : _ -> exe
    _ -> error "No command line specified"

-- Replace the ? argument with thisInput
makeTestArgs :: String -> [String] -> [String]
makeTestArgs thisInput (_:args) = map substituteTempFile args
    where
      substituteTempFile "?" = thisInput
      substituteTempFile x = x

fileAsInputLines :: FilePath -> IO [ByteString]
fileAsInputLines filename = do
  s <- BS.readFile filename
  return $ map (`BS.append` "\n") (BS.lines s)

fileAsInputChars :: FilePath -> IO String
fileAsInputChars = readFile

-- Add an input method for c-like languages.  Match top-level curly
-- braces and treat the braced unit (and the preceeding line) as a
-- unit.

-- If a destination was specified on the command line, save failing
-- inputs.
saveOutput :: Maybe FilePath -> ByteString -> IO ()
saveOutput Nothing _ = return ()
saveOutput (Just destination) fileContents =
  withFile destination WriteMode $ flip BS.hPut fileContents

disposeTmpFile :: (FilePath, Handle) -> IO ()
disposeTmpFile (fn, _) = removeFile fn

-- | Dumps the current state to a temporary file.  This temporary file
-- is substituted for the ? argument provided in the skeleton command
-- line.  If the output matches the regular expression given on the
-- command line, the test is marked as Fail.  An exit code of 0 is
-- marked as a Pass, and anything else is Unresolved.  The temporary
-- file is cleaned up when it is no longer needed (most of the time).
-- To be truly correct I should use bracket.  The error output is
-- displayed on stdout to demonstrate progress.
testFunc :: Config -> [ByteString] -> IO Outcome
testFunc cfg ls = do
  let failureRegex = searchRegex cfg
      (_, fileExt) = splitExtension (inputFile cfg)

  let tmpFileName = "ddmin" <.> fileExt
  tmpDir <- getTemporaryDirectory
  (exitCode, output, fileContents) <- bracket (openTempFile tmpDir tmpFileName) disposeTmpFile doTest

  case exitCode of
    ExitSuccess -> putStrLn "ExitSuccess" >> return Pass
    ExitFailure _ -> case output =~ failureRegex of
      True -> putStrLn "Fail" >> saveOutput (savedOutput cfg) fileContents >> return Fail
      False -> putStrLn "Unresolved" >> return Unresolved
  where
    doTest (fn, fh) = do
      let expandedArgs = makeTestArgs fn (cmdLine cfg)
          testExecutable = getTestExecutable cfg
          fileContents = BS.concat ls
      BS.hPut fh fileContents
      hClose fh
      (exitCode, outStr, errStr) <- readProcessWithExitCode testExecutable expandedArgs ""
      let output = case searchOutput cfg of
            Both -> concat [ outStr, "\n", errStr ]
            StdOut -> outStr
            StdErr -> errStr
      putStrLn (outStr ++ "\n" ++ errStr)
      return (exitCode, output, fileContents)

lineIsNotBlank :: ByteString -> Bool
lineIsNotBlank "\n" = False
lineIsNotBlank _ = True

main :: IO ()
main = do
  cfg <- processArgs arguments

  case isHelp cfg of
    True -> putStrLn $ showText (Wrap 80) $ helpText HelpFormatOne arguments
    False -> do
      print (cmdLine cfg)
      input <- fileAsInputLines (inputFile cfg)

--      let inputWithoutBlanks = filter lineIsNotBlank input
      minInput <- ddmin input{-WithoutBlanks-} (testFunc cfg)
      putStrLn $ "Smallest input: \n" ++ show (BS.concat minInput)