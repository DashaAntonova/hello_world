{-С помощью стандартных функций для генерации случайных чисел напишите программу, 
которая проводит состязание по игре в кости. Программа принимает аргументом суммарное 
число очков необходимых для победы. Двое игроков бросают по очереди кости побеждает 
тот, кто первым наберёт заданную сумму.
Сделайте так чтобы результаты выводились постепенно. С каждым нажатием на Enter вы 
подбрасываете кости (два шестигранных кубика). После каждого раунда программа выводит
промежуточные результаты.
-}

import System.Random
import System.IO
import Control.Monad.Primitive
import System.Environment

gameDice :: Integer -> [Integer] -> [Integer] -> IO ()
gameDice kol res1 res2 = do
    let x = res1 ++ [unsafeInlineIO (randomRIO (1 :: Integer, read "6"))]
        y = res2 ++ [unsafeInlineIO (randomRIO (1 :: Integer, read "6"))]
    str <- getLine      
                --это для постепенного вывода результатов. 
                --Можно как-нибудь посимпатичнее сделать постепенный вывод?
    putStrLn $ (show x) ++ " = " ++ show (sum x)
    putStrLn $ (show y) ++ " = " ++ show (sum y)
    if (sum x >= kol) || (sum y >= kol) then putStrLn "" else gameDice kol x y

main = do
    putStrLn "Сколько очков необходимо набрать для победы?"
    kolStr <- getLine
    gameDice (read kolStr) [] []
    
    putStrLn "End"