{-# LANGUAGE DeriveDataTypeable #-}
import DeltaDebug
import DeltaDebug.InputStrategies
import Directory
import System.Console.CmdArgs
import System.Exit
import System.FilePath
import System.IO (hPutStr, hClose, hGetContents, withFile, openTempFile, IOMode(..))
import System.Process
import Text.Regex.PCRE hiding (empty)

data Config = Config { search_combined :: String
                     , search_error    :: String
                     , search_out      :: String
                     , input_file      :: String
                     , saved_output    :: String
                     , cmdLine         :: [String]
                       } deriving (Show, Data, Typeable)

config = mode $ Config { search_combined = def &= text "Search the combined stdout and stderr for a regex" & typ "REGEX"
                       , search_error    = def &= text "Search stderr for a regex" & typ "REGEX"
                       , search_out      = def &= text "Search stdout for a regex" & typ "REGEX"
                       , input_file      = def &= text "The input file to be minimized" & typFile
                       , saved_output    = def &= text "Save the latest failing test to a file" & typFile
                       , cmdLine         = def &= args
                       }

getSearchRegex cfg =
  case (search_combined cfg, search_error cfg, search_out cfg) of
    (x, "", "") -> x
    ("", x, "") -> x
    ("", "", x) -> x
    _ -> error "Only specify one search target supported for now"

getTestExecutable cfg =
  case cmdLine cfg of
    exe : args -> exe
    _ -> error "No command line specified"

-- Replace the ? argument with thisInput
makeTestArgs thisInput (cmd:args) = map substituteTempFile args
    where substituteTempFile "?" = thisInput
          substituteTempFile x = x

fileAsInputLines filename = do
  s <- readFile filename
  return $ byLine s

fileAsInputChars = readFile

getCombinedOutput hOut hErr = do
  realOut <- hGetContents hOut
  realErr <- hGetContents hErr

  let combinedOut = realOut ++ "\n" ++ realErr
  putStrLn combinedOut

  return combinedOut

saveOutput "" fileContents = return ()
saveOutput destination fileContents =
  withFile destination WriteMode $ flip hPutStr fileContents

testFunc cfg lines = do
  let fileContents = concat lines
      failureRegex = getSearchRegex cfg
      testExecutable = getTestExecutable cfg
      (originalFilename, fileExt) = splitExtension $ input_file cfg

  (tempFileName, tempHandle) <- openTempFile "/tmp" ("ddmin." ++ fileExt)

  let procDesc = proc testExecutable $ makeTestArgs tempFileName $ cmdLine cfg
      params   = procDesc { std_err = CreatePipe
                          , std_out = CreatePipe
                          }

  hPutStr tempHandle fileContents
  hClose tempHandle
  (_, Just hOut, Just hErr, p) <- createProcess params

  combinedOutput <- getCombinedOutput hOut hErr

  exitCode <- waitForProcess p
  removeFile tempFileName
  -- hClose hOut
  -- hClose hErr

  putStrLn combinedOutput

  case exitCode of
    ExitSuccess -> putStrLn "ExitSuccess" >> return Pass
    ExitFailure c -> if combinedOutput =~ failureRegex
                       then putStrLn "Fail" >> saveOutput (saved_output cfg) fileContents >> return Fail
                       else putStrLn "Unresolved" >> return Unresolved

lineIsNotBlank "\n" = False
lineIsNotBlank _ = True

main = do
  cfg <- cmdArgs "A Haskell implementation of the ddmin algorithm" [config]

  input <- fileAsInputLines $ input_file cfg

  let inputWithoutBlanks = filter lineIsNotBlank input
  minInput <- ddmin inputWithoutBlanks (testFunc cfg)
  putStrLn $ "Smallest input: \n" ++ concat minInput