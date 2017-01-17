{-Напишите с помощью типа Random функцию, которая будет создавать 
случайные деревья заданной глубины. Значение в узле является 
случайным числом от 0 до 100, также число дочерних деревьев в 
каждом узле случайно, оно изменяется от 0 до 10.-}

import Data.Tree
import System.Random
import System.IO
import Control.Monad.Primitive

createTree :: Int -> Tree Integer
createTree 1 = Node (unsafeInlineIO (randomRIO (1,100 :: Integer))) []
createTree x = Node (unsafeInlineIO (randomRIO (1,100 :: Integer))) [createTree (x-1) | y <- [1..unsafeInlineIO (randomRIO (1,10 :: Int))]]

showForest :: [Tree Integer] -> String
showForest [] = ""
showForest (x:xs) = "[" ++ showTree x ++ "]" ++ " " ++ showForest xs

showTree :: Tree Integer -> String
showTree (Node x xs) = show x ++ if showForest xs == "" then "" else (" (" ++ showForest xs ++ ")")

main = do
    putStrLn "Какова глубина деревьев?"
    depthStr <- getLine
    let for = createTree (read depthStr)
    putStrLn $ showTree for

{-
При глубине дерева = 3 получается вот такое дерево:
2 ([90 ([87] [94] [90] )] [27 ([58] [60] )] [76 ([40] [2] [58] [52] [16] [69] [36] )] [85 ([33] [6] [51] [90] [37] )] [94 ([67] [65] [55] [97] [62] )] )
Вот так оно симпатичнее выглядит:
2 (
    [90 (
        [87] 
        [94] 
        [90] )
    ] 
    [27 (
        [58] 
        [60])
    ] 
    [76 (
        [40] 
        [2] 
        [58] 
        [52] 
        [16] 
        [69] 
        [36])
    ] 
    [85 (
        [33] 
        [6] 
        [51] 
        [90] 
        [37])
    ] 
    [94 (
        [67] 
        [65] 
        [55] 
        [97] 
        [62])
    ]
)
-}