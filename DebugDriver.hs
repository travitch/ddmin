{-# LANGUAGE DeriveDataTypeable #-}
import DeltaDebug
import DeltaDebug.InputStrategies
import Directory
import System.Console.CmdArgs
import System.Exit
import System.FilePath
import System.IO (hPutStr, hClose, hGetContents, withFile, openTempFile, hIsEOF, hGetLine, IOMode(..))
import System.Process
import Text.Regex.PCRE hiding (empty)

data Config = Config { searchCombined :: String
                     , searchError    :: String
                     , searchOut      :: String
                     , inputFile      :: String
                     , savedOutput    :: String
                     , cmdLine        :: [String]
                     , unknownArgs    :: [String]
                       } deriving (Show, Data, Typeable)

config = mode $ Config { searchCombined = def &= text "Search the combined stdout and stderr for a regex" & typ "REGEX" & explicit & flag "search-combined"
                       , searchError    = def &= text "Search stderr for a regex" & typ "REGEX" & explicit & flag "search-error"
                       , searchOut      = def &= text "Search stdout for a regex" & typ "REGEX" & explicit & flag "search-out"
                       , inputFile      = def &= text "The input file to be minimized" & typFile & explicit & flag "input-file"
                       , savedOutput    = def &= text "Save the latest failing test to a file" & typFile & explicit & flag "saved-output"
                       , cmdLine        = def &= args
                       , unknownArgs    = def &= unknownFlags
                       }

getSearchRegex cfg =
  case (searchCombined cfg, searchError cfg, searchOut cfg) of
    (x, "", "") -> x
    ("", x, "") -> x
    ("", "", x) -> x
    _ -> error "Only specify one search target supported for now"

getTestExecutable cfg =
  case cmdLine cfg of
    exe : args -> exe
    _ -> error "No command line specified"

-- Replace the ? argument with thisInput
makeTestArgs thisInput (cmd:args) unknowns = map substituteTempFile (unknowns ++ args)
    where substituteTempFile "?" = thisInput
          substituteTempFile x = x

fileAsInputLines filename = do
  s <- readFile filename
  return $ byLine s

fileAsInputChars = readFile

-- If a destination was specified on the command line, save failing
-- inputs.
saveOutput "" fileContents = return ()
saveOutput destination fileContents =
  withFile destination WriteMode $ flip hPutStr fileContents

-- | Dumps the current state to a temporary file.  This temporary file
-- is substituted for the ? argument provided in the skeleton command
-- line.  If the output matches the regular expression given on the
-- command line, the test is marked as Fail.  An exit code of 0 is
-- marked as a Pass, and anything else is Unresolved.  The temporary
-- file is cleaned up when it is no longer needed (most of the time).
-- To be truly correct I should use bracket.  The error output is
-- displayed on stdout to demonstrate progress.
testFunc cfg lines = do
  let fileContents = concat lines
      failureRegex = getSearchRegex cfg
      testExecutable = getTestExecutable cfg
      (originalFilename, fileExt) = splitExtension $ inputFile cfg

  (tempFileName, tempHandle) <- openTempFile "/tmp" ("ddmin." ++ fileExt)
  let expandedArgs = makeTestArgs tempFileName (cmdLine cfg) (unknownArgs cfg)

  hPutStr tempHandle fileContents
  hClose tempHandle

  (exitCode, outStr, errStr) <- readProcessWithExitCode testExecutable expandedArgs ""
  let combinedOutput = outStr ++ "\n" ++ errStr

  putStrLn combinedOutput
  removeFile tempFileName

  case exitCode of
    ExitSuccess -> putStrLn "ExitSuccess" >> return Pass
    ExitFailure c -> if combinedOutput =~ failureRegex
                       then putStrLn "Fail" >> saveOutput (savedOutput cfg) fileContents >> return Fail
                       else putStrLn "Unresolved" >> return Unresolved

lineIsNotBlank "\n" = False
lineIsNotBlank _ = True

main = do
  cfg <- cmdArgs "A Haskell implementation of the ddmin algorithm" [config]

  input <- fileAsInputLines $ inputFile cfg

  let inputWithoutBlanks = filter lineIsNotBlank input
  minInput <- ddmin inputWithoutBlanks (testFunc cfg)
  putStrLn $ "Smallest input: \n" ++ concat minInput