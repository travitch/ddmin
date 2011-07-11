{-# LANGUAGE DeriveDataTypeable #-}
import DeltaDebug
import DeltaDebug.InputStrategies
import Directory
-- import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import System.Exit
import System.FilePath
import System.IO (hPutStr, hClose, hGetContents, withFile, openTempFile, hIsEOF, hGetLine, IOMode(..))
import System.Process
import Text.Regex.PCRE hiding (empty)

addArg :: String -> Config -> Either String Config
addArg s c = Right c { cmdLine = cmdLine c ++ [s] }

setCombined :: String -> Config -> Either String Config
setCombined s c = Right c { searchCombined = Just s }

setError :: String -> Config -> Either String Config
setError s c = Right c { searchError = Just s }

setOut :: String -> Config -> Either String Config
setOut s c = Right c { searchOut = Just s }

setInputFile :: String -> Config -> Either String Config
setInputFile s c = Right c { inputFile = Just s }

setOutputFile :: String -> Config -> Either String Config
setOutputFile s c = Right c { savedOutput = Just s }

arguments :: Mode Config
arguments = mode "explicit" defaultConfig desc (flagArg addArg "COMMAND") as
  where
    desc = "A Haskell implementation of the ddmin algorithm"
    as = [ flagReq ["search-combined"] setCombined "REGEX" "Search the combined stdout and stderr"
         , flagReq ["search-stdout"] setOut "REGEX" "Search stdout"
         , flagReq ["search-stderr"] setError "REGEX" "Search stderr"
         , flagReq ["input-file"] setInputFile "FILE" "The initial input file"
         , flagReq ["save-output"] setOutputFile "FILE" "The file in which to save the smallest output"
         ]

data Config = Config { searchCombined :: Maybe String
                     , searchError    :: Maybe String
                     , searchOut      :: Maybe String
                     , inputFile      :: Maybe String
                     , savedOutput    :: Maybe String
                     , cmdLine        :: [String]
                       } deriving (Show) -- , Data, Typeable)

defaultConfig :: Config
defaultConfig = Config { searchCombined = Nothing
                       , searchError = Nothing
                       , searchOut = Nothing
                       , inputFile = Nothing
                       , savedOutput = Nothing
                       , cmdLine = []
                       }

-- config = Config { searchCombined = def &= help "Search the combined stdout and stderr for a regex" &= typ "REGEX" &= explicit &= name "search-combined"
--                 , searchError    = def &= help "Search stderr for a regex" &= typ "REGEX" &= explicit &= name "search-error"
--                 , searchOut      = def &= help "Search stdout for a regex" &= typ "REGEX" &= explicit &= name "search-out"
--                 , inputFile      = def &= help "The input file to be minimized" &= typFile &= explicit &= name "input-file"
--                 , savedOutput    = def &= help "Save the latest failing test to a file" &= typFile &= explicit &= name "saved-output"
--                 , cmdLine        = def &= args -- help "The command to run.  Use ? for the input file" &= explicit &= name "cmd"
--                 }

getSearchRegex cfg =
  case (searchCombined cfg, searchError cfg, searchOut cfg) of
    (Just x, Nothing, Nothing) -> x
    (Nothing, Just x, Nothing) -> x
    (Nothing, Nothing, Just x) -> x
    _ -> error "Only specify one search target supported for now"

getTestExecutable cfg =
  case cmdLine cfg of
    exe : args -> exe
    _ -> error "No command line specified"

-- Replace the ? argument with thisInput
makeTestArgs thisInput (cmd:args) = map substituteTempFile args
    where
      substituteTempFile "?" = thisInput
      substituteTempFile x = x

fileAsInputLines filename = do
  s <- readFile filename
  return $ byLine s

fileAsInputChars = readFile

-- If a destination was specified on the command line, save failing
-- inputs.
saveOutput Nothing fileContents = return ()
saveOutput (Just destination) fileContents =
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
      Just infile = inputFile cfg
      (originalFilename, fileExt) = splitExtension infile

  (tempFileName, tempHandle) <- openTempFile "/tmp" ("ddmin." ++ fileExt)
  let expandedArgs = makeTestArgs tempFileName (cmdLine cfg)

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
  -- cfg <- cmdArgs {-"A Haskell implementation of the ddmin algorithm"-} config
  cfg <- processArgs arguments
  case inputFile cfg of
    Nothing -> error "input-file is required"
    Just _ -> return ()

  let Just infile = inputFile cfg

  input <- fileAsInputLines infile

  let inputWithoutBlanks = filter lineIsNotBlank input
  minInput <- ddmin inputWithoutBlanks (testFunc cfg)
  putStrLn $ "Smallest input: \n" ++ concat minInput