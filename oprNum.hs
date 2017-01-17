{-Напишите программу для угадывания чисел. Компьютер загадал 
число в заданном диапазоне и просит вас угадать его. Если вы 
ошибаетесь он подсказывает: “холодно-горячо” или “больше-меньше”. 
Программа принимает два аргумента, которые определяют диапазон 
возможных значений для неизвестного числа.-}

import System.Random
import System.IO
import Control.Monad.Primitive
import System.Environment

funcOpr :: Integer -> Integer -> Integer -> Integer -> IO Bool
funcOpr x numInt numMin numMax = do
    if (x == numInt) then do
        putStrLn "Правильно!"
        return True
    else do
        if numInt < x then do
            putStrLn ("Нет. Число от " ++ show numInt ++ " до " ++ show numMax ++ ". Какое?")
            zzz <- getLine
            funcOpr x (read (zzz)) numInt numMax
        else do
            if numInt > x then do
                putStrLn ("Нет. Число от " ++ show numMin ++ " до " ++ show numInt ++ ". Какое?")
                zzz <- getLine
                funcOpr x (read (zzz)) numMin numInt
            else return False

main = do
    zzz <- getArgs
    let x = unsafeInlineIO (randomRIO (1, 100 :: Integer))
        numMin = 1
        numMax = 100

    putStrLn $ show x
    putStrLn "Задумано число от 1 до 100. Какое?"
    numStr <- getLine
    res <- funcOpr x (read numStr) 1 100
    putStrLn "End"