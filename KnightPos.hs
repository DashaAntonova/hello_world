{-
Из книги "Изучай Haskell во имя добра", стр. 399
У нас есть шахматная доска и на ней только одна фигура – конь. Мы хотим определить, 
может ли конь достигнуть определённой позиции в три хода.
В качестве упражнения вы можете изменить эту функцию так, чтобы она показывала 
вам ходы, которые нужно совершить, когда вы можете достигнуть одной позиции из другой.
-}

import Control.Monad
import Data.List

type KnightPos = (Int, Int)

moveKnight :: [KnightPos] -> [[KnightPos]]
moveKnight a = filter (/= []) res
    where res = [if rightPos (x+2, y-1) then a ++ [(x+2, y-1)] else []] ++
            [if rightPos (x+2, y+1) then a ++ [(x+2, y+1)] else []] ++ 
            [if rightPos (x-2, y-1) then a ++ [(x-2, y-1)] else []] ++ 
            [if rightPos (x-2, y+1) then a ++ [(x-2, y+1)] else []] ++ 
            [if rightPos (x+1, y-2) then a ++ [(x+1, y-2)] else []] ++ 
            [if rightPos (x+1, y+2) then a ++ [(x+1, y+2)] else []] ++ 
            [if rightPos (x-1, y-2) then a ++ [(x-1, y-2)] else []] ++ 
            [if rightPos (x-1, y+2) then a ++ [(x-1, y+2)] else []]
            where x = fst (last a)
                  y = snd (last a)
    
rightPos :: KnightPos -> Bool
rightPos (x, y) = if x `elem` [1..8] && y `elem` [1..8] then True else False

in3 :: [KnightPos] -> [[KnightPos]]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

in2 :: [KnightPos] -> [[KnightPos]]
in2 start = return start >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> [[KnightPos]]
canReachIn3 start end = filter (\x -> equalPos start x) (in3 [end])

equalPos :: KnightPos -> [KnightPos] -> Bool
equalPos a b = if a == last b then True else False

main = do
    let begin = (6,2)
        end = (4,7)
        move1 = moveKnight [begin]
        move2 = in2 [begin]
        move3 = in3 [begin]
        can = canReachIn3 begin end
    putStrLn "Первый ход"
    mapM print move1
    putStrLn "Второй ход"
    mapM print move2
    putStrLn "Третий ход"
    mapM print move3
    putStrLn "can"
    mapM print can
    print begin
    putStrLn "End"