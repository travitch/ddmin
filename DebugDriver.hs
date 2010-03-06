import Data.Text (pack, isInfixOf)
import DeltaDebug
import DeltaDebug.InputStrategies
import System.Exit
import System.IO (hPutStr, hClose, hGetContents, withFile, IOMode(..))
import System.Process

fileAsInputLines filename = do
  s <- readFile filename
  return $ byLine s

fileAsInputChars filename = readFile filename

getCombinedOutput hOut hErr = do
  realOut <- hGetContents hOut
  realErr <- hGetContents hErr

  let combinedOut = realOut ++ "\n" ++ realErr
  putStrLn combinedOut

  return combinedOut

saveOutput fileContents = withFile "saved_failure.cpp" WriteMode (\x -> hPutStr x fileContents)

testFunc failureText lines = do
  let fileContents = concat lines
      procDesc = proc "/u/t/r/travitch/private/research/sampler_cc_rose/bin/identity-unparser" ["-rose:unparse_includes", "input.cpp", "-c"]
      params   = procDesc { std_err = CreatePipe
                          , std_out = CreatePipe
                          , std_in  = CreatePipe
                          , close_fds = False
                          }
  withFile "input.cpp" WriteMode (\x -> hPutStr x fileContents)
  (Just hIn, Just hOut, Just hErr, p) <- createProcess params

  hPutStr hIn fileContents
  hClose hIn

  combinedOutput <- getCombinedOutput hOut hErr
  hClose hOut
  hClose hErr

  exitCode <- waitForProcess p

  putStrLn combinedOutput

  let errText = pack combinedOutput
  case exitCode of
    ExitSuccess -> (putStrLn "ExitSuccess") >> return Pass
    ExitFailure c -> if isInfixOf failureText errText
                       then (putStrLn "Fail") >> (saveOutput fileContents) >> return Fail
                       else (putStrLn "Unresolved") >> return Unresolved

lineIsNotBlank "\n" = False
lineIsNotBlank _ = True

main = do
  input <- fileAsInputLines "cbi_sampler_test.cpp"
  let failureText = pack " declaration of ‘class __gnu_cxx::new_allocator<char>’ in ‘std’ which does not enclose ‘__gnu_cxx’"
      inputWithoutBlanks = filter lineIsNotBlank input
  minInput <- ddmin inputWithoutBlanks (testFunc failureText)
  putStrLn $ "Smallest input: \n" ++ concat minInput