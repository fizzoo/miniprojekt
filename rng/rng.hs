import System.Random
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let maxv = case length args of 1 -> read $ head args :: Integer
                                   _ -> 100
    nr <- randomRIO (0,maxv-1)
    print nr
