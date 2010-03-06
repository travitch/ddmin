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

testFunc failureText lines = do
  let fileContents = lines
      procDesc = proc "/s/gcc-2.95/bin/gcc" ["-O2", "-xc", "-"]
      params   = procDesc { std_err = CreatePipe
                          , std_out = CreatePipe
                          , std_in  = CreatePipe
                          , close_fds = False
                          }
  (Just hIn, Just hOut, Just hErr, p) <- createProcess params
  -- putStrLn fileContents
  hPutStr hIn fileContents
  hClose hIn
  realOut <- hGetContents hOut
  realErr <- hGetContents hErr
  exitCode <- waitForProcess p
  putStrLn (realErr ++ "\n" ++ realOut)
  hClose hErr
  hClose hOut
  let errText = pack (realErr ++ "\n" ++ realOut)
  case exitCode of
    ExitSuccess -> (putStrLn "ExitSuccess") >> return Pass
    ExitFailure c -> if isInfixOf failureText errText
                       then (putStrLn "Fail") >> return Fail
                       else (putStrLn "Unresolved") >> return Unresolved

main = do
  let failureText = pack "Internal compiler error"
  input <- fileAsInputChars "bug2.c"
  minInput <- ddmin input (testFunc failureText)
  putStrLn $ "Smallest input: \n" ++ minInput