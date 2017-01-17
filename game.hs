{-Напишите с помощью типа Random функцию игры в кости, 
два игрока бросают по очереди кости (два кубика с шестью гранями, 
грани пронумерованы от 1 до 6). 
Они бросают кубики 10 раз выигрывает тот, у кого в сумме выпадет 
больше очков. Функция принимает начальное состояние и выводит 
результат игры: суммарные баллы игроков.-}

import System.Random
import System.IO
import Control.Monad.Primitive

createArray :: Int -> [Int]
createArray x = [unsafeInlineIO (randomRIO (1 :: Int, read "6")) | x <- [1..x]]

main = do
    putStrLn "Сколько раз игроки бросят кости?"
    kolStr <- getLine
        
    let kolInt = read kolStr
        x = createArray kolInt
        y = createArray kolInt
        sumX = sum x
        sumY = sum y
    
    print $ show x ++ " = " ++ show sumX
    print $ show y ++ " = " ++ show sumY
    putStrLn "End"