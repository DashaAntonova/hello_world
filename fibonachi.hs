module Main where

import System.IO
import Data.List

createArray :: Int -> [Int]
createArray x = [fibbonachi x | x <- [1..x]]

fibbonachi :: Int -> Int
fibbonachi 1 = 1
fibbonachi 2 = 1
fibbonachi x = fibbonachi (x-1) + fibbonachi (x-2)

main :: IO ()
main = do
    putStrLn "Количество:"
    kolStr <- getLine 
    let kolInt = read kolStr
        arr = createArray kolInt
     
    print arr
    putStrLn "End"
