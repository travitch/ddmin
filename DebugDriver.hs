import Data.Text (pack, isInfixOf)
import DeltaDebug
import DeltaDebug.InputStrategies
import System.Exit
import System.IO (hPutStr, hClose, hGetContents)
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

testFunc failureText lines = do
  let fileContents = lines
      procDesc = proc "/s/gcc-2.95/bin/gcc" ["-O2", "-xc", "-"]
      params   = procDesc { std_err = CreatePipe
                          , std_out = CreatePipe
                          , std_in  = CreatePipe
                          , close_fds = False
                          }
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
                       then (putStrLn "Fail") >> return Fail
                       else (putStrLn "Unresolved") >> return Unresolved

main = do
  let failureText = pack "Internal compiler error"
  input <- fileAsInputChars "bug.c"
  minInput <- ddmin input (testFunc failureText)
  putStrLn $ "Smallest input: \n" ++ minInput