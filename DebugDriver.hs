import Text.Regex.PCRE
import DeltaDebug
import DeltaDebug.InputStrategies
import System.Exit
import System.IO (hPutStr, hClose, hGetContents, withFile, IOMode(..))
import System.Process

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

saveOutput fileContents = withFile "saved_failure.cpp" WriteMode $ flip hPutStr fileContents

testFunc failureRegex lines = do
  let fileContents = concat lines
      procDesc = proc "/u/t/r/travitch/private/research/sampler_cc_rose/bin/identity-unparser" ["-rose:unparse_includes", "input.cpp", "-c"]
      params   = procDesc { std_err = CreatePipe
                          , std_out = CreatePipe
                          -- , std_in  = CreatePipe
                          , close_fds = False
                          }
  withFile "input.cpp" WriteMode $ flip hPutStr fileContents
  (_, Just hOut, Just hErr, p) <- createProcess params

  -- hPutStr hIn fileContents
  -- hClose hIn

  combinedOutput <- getCombinedOutput hOut hErr

  exitCode <- waitForProcess p
  -- hClose hOut
  -- hClose hErr

  putStrLn combinedOutput

  case exitCode of
    ExitSuccess -> putStrLn "ExitSuccess" >> return Pass
    ExitFailure c -> if combinedOutput =~ failureRegex
                       then putStrLn "Fail" >> saveOutput fileContents >> return Fail
                       else putStrLn "Unresolved" >> return Unresolved

lineIsNotBlank "\n" = False
lineIsNotBlank _ = True

main = do
  input <- fileAsInputLines "cbi_sampler_test.cpp"
  let failureRegex = "which does not enclose"
      inputWithoutBlanks = filter lineIsNotBlank input
  minInput <- ddmin inputWithoutBlanks (testFunc failureRegex)
  putStrLn $ "Smallest input: \n" ++ concat minInput