import           Control.Concurrent
import           Control.Exception    (finally, throwTo)
import           Data.Time.Clock
import           System.Exit
import           System.IO
import           System.Posix.Signals

end :: ThreadId -> IO ()
end tid = throwTo tid ExitSuccess

finalPrint :: UTCTime -> IO ()
finalPrint start = do
  now <- getCurrentTime
  putStr "\nFinal value:\n"
  print $ diffUTCTime now start

exitOnQ :: ThreadId -> IO ()
exitOnQ tid = do
  hSetBuffering stdin NoBuffering
  c <- getChar
  if c == 'q'
    then end tid
    else exitOnQ tid

ticker :: UTCTime -> NominalDiffTime -> IO ()
ticker start last = do
  threadDelay 10000
  now <- getCurrentTime
  let diff = diffUTCTime now start
  if diff > last + 1
    then print diff >> ticker start (last + 1)
    else ticker start last

main :: IO ()
main = do
  tid <- myThreadId
  start <- getCurrentTime
  forkIO $ exitOnQ tid
  installHandler keyboardSignal (Catch $ end tid) Nothing
  finally (ticker start 0.0) (finalPrint start)
