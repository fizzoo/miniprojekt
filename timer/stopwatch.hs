import           Control.Concurrent   (ThreadId, forkIO, myThreadId,
                                       threadDelay)
import           Control.Exception    (finally, throwTo)
import           Data.Time.Clock      (NominalDiffTime, UTCTime, diffUTCTime,
                                       getCurrentTime)
import           System.Exit          (ExitCode (..))
import           System.IO            (BufferMode (..), hSetBuffering, stdin)
import           System.Posix.Signals (Handler (..), installHandler,
                                       keyboardSignal)

-- | Kill thread 'tid' nicely.
end :: ThreadId -> IO ()
end tid = throwTo tid ExitSuccess

-- | Print the final time, especially formatted.
finalPrint :: UTCTime -> IO ()
finalPrint start = do
  now <- getCurrentTime
  putStr "\nFinal value:\n"
  print $ diffUTCTime now start

-- | Wait for, and kill 'tid' if a q is pressed.
exitOnQ :: ThreadId -> IO ()
exitOnQ tid = do
  hSetBuffering stdin NoBuffering
  c <- getChar
  if c == 'q'
    then end tid
    else exitOnQ tid

-- | Keep time and tick once a second.
ticker :: UTCTime -> NominalDiffTime -> IO ()
ticker start lst = do
  threadDelay 10000
  now <- getCurrentTime
  let diff = diffUTCTime now start
  if diff > lst + 1
    then print diff >> ticker start (lst + 1)
    else ticker start lst

main :: IO ()
main = do
  tid <- myThreadId
  start <- getCurrentTime
  _ <- forkIO $ exitOnQ tid
  _ <- installHandler keyboardSignal (Catch $ end tid) Nothing
  finally (ticker start 0.0) (finalPrint start)
