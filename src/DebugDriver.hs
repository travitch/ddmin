{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import System.Directory
import System.Console.CmdArgs.Explicit
import System.Exit
import System.FilePath
import System.IO ( hClose, withFile, openTempFile, IOMode(..) )
import System.Process
import Text.Regex.PCRE hiding ( empty )

import DeltaDebug
import DeltaDebug.InputStrategies

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
                       } deriving (Show)

defaultConfig :: Config
defaultConfig = Config { searchCombined = Nothing
                       , searchError = Nothing
                       , searchOut = Nothing
                       , inputFile = Nothing
                       , savedOutput = Nothing
                       , cmdLine = []
                       }

getSearchRegex :: Config -> String
getSearchRegex cfg =
  case (searchCombined cfg, searchError cfg, searchOut cfg) of
    (Just x, Nothing, Nothing) -> x
    (Nothing, Just x, Nothing) -> x
    (Nothing, Nothing, Just x) -> x
    _ -> error "Only specify one search target supported for now"

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
  return $ map (flip BS.append "\n") (BS.lines s)

fileAsInputChars :: FilePath -> IO String
fileAsInputChars = readFile

-- If a destination was specified on the command line, save failing
-- inputs.
saveOutput :: Maybe FilePath -> ByteString -> IO ()
saveOutput Nothing _ = return ()
saveOutput (Just destination) fileContents =
  withFile destination WriteMode $ flip BS.hPut fileContents

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
  let fileContents = BS.concat ls
      failureRegex = getSearchRegex cfg
      testExecutable = getTestExecutable cfg
      Just infile = inputFile cfg
      (_, fileExt) = splitExtension infile

  (tempFileName, tempHandle) <- openTempFile "/tmp" ("ddmin." ++ fileExt)
  let expandedArgs = makeTestArgs tempFileName (cmdLine cfg)

  BS.hPut tempHandle fileContents
  hClose tempHandle

  (exitCode, outStr, errStr) <- readProcessWithExitCode testExecutable expandedArgs ""
  let combinedOutput = outStr ++ "\n" ++ errStr

  putStrLn combinedOutput
  removeFile tempFileName

  case exitCode of
    ExitSuccess -> putStrLn "ExitSuccess" >> return Pass
    ExitFailure _ -> if combinedOutput =~ failureRegex
                       then putStrLn "Fail" >> saveOutput (savedOutput cfg) fileContents >> return Fail
                       else putStrLn "Unresolved" >> return Unresolved

lineIsNotBlank :: ByteString -> Bool
lineIsNotBlank "\n" = False
lineIsNotBlank _ = True

main :: IO ()
main = do
  cfg <- processArgs arguments
  case inputFile cfg of
    Nothing -> error "input-file is required"
    Just _ -> return ()

  putStrLn $ show (cmdLine cfg)

  let Just infile = inputFile cfg

  input <- fileAsInputLines infile

  let inputWithoutBlanks = filter lineIsNotBlank input
  minInput <- ddmin inputWithoutBlanks (testFunc cfg)
  putStrLn $ "Smallest input: \n" ++ (show (BS.concat minInput))