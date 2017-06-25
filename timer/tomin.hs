import           Data.Maybe (mapMaybe)
import           System.IO  (BufferMode (..), hSetBuffering, stdin, stdout)
import           Text.Read  (readMaybe)

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  interact $ unlines . map (formatTime . round) . todoub . map stripIfS . words

stripIfS :: String -> String
stripIfS x = if last x == 's' then init x else x

todoub :: [String] -> [Double]
todoub inp = mapMaybe readMaybe inp :: [Double]

formatTime :: Integer -> String
formatTime x
  | x < 60 = show x ++ "s"
  | x < 3600 = let (m, s) = divMod x 60
               in show m ++ "m " ++ show s ++ "s"
  | otherwise = let (h, y) = divMod x 3600
                    (m, s) = divMod y 60
                in show h ++ "h " ++ show m ++ "m " ++ show s ++ "s "

