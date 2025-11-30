{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Parallel.Strategies (using, parList, rdeepseq)
import Control.DeepSeq (force)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)

-- Типы данных:
-- Матрица — это список строк, строка — список чисел.
type Row = [Double]
type Matrix = [Row]

-- ==========================================
-- 1. ЛОГИКА МЕТОДА ГАУССА
-- ==========================================

-- Функция вычитания строки (eliminate)
-- Берет "ведущую" строку (pivot) и целевую строку.
-- Вычисляет коэффициент и вычитает ведущую из целевой, чтобы получить 0 в начале.
eliminate :: Row -> Row -> Row
eliminate pivotRow targetRow =
    let factor = head targetRow / head pivotRow
    in zipWith (\p t -> t - factor * p) pivotRow targetRow

-- Основная рекурсивная функция (Параллельная версия)
parallelGauss :: Matrix -> Matrix
parallelGauss [] = []
parallelGauss (pivotRow:rows) =
    let 
        -- 1. Создаем список задач: применить eliminate ко всем нижним строкам
        nextRows = map (eliminate pivotRow) rows

        -- 2. МАГИЯ ПАРАЛЛЕЛИЗМА:
        -- `using` parList rdeepseq говорит Haskell'у:
        -- "Вычисли каждый элемент этого списка (parList) полностью (rdeepseq) и параллельно"
        parNextRows = nextRows `using` parList rdeepseq
        
        -- 3. Рекурсия:
        -- Берем хвосты строк (tail), так как первые элементы уже нули, 
        -- и запускаем алгоритм для оставшейся части матрицы.
    in pivotRow : parallelGauss (map tail parNextRows)

-- ==========================================
-- 2. ГЕНЕРАЦИЯ ДАННЫХ
-- ==========================================

-- Генерация матрицы N x (N+1) по простой формуле
generateMatrix :: Int -> Matrix
generateMatrix n = 
    [ [ fromIntegral (i + j) | j <- [1..(n+1)] ] | i <- [1..n] ]

-- ==========================================
-- 3. MAIN
-- ==========================================

main :: IO ()
main = do
    putStrLn "=== Parallel Gauss Elimination ==="
    
    -- РАЗМЕР МАТРИЦЫ
    -- 1000 - оптимально для теста. Если слишком быстро, ставь 2000.
    let size = 1000 
    
    printf "Generating matrix %d x %d...\n" size (size + 1)
    let matrix = generateMatrix size
    
    putStrLn "Starting calculation..."
    
    -- Засекаем время
    start <- getCurrentTime
    
    -- !result и force нужны, чтобы вычисления произошли ИМЕННО ЗДЕСЬ,
    -- а не отложились ленивым Haskell на потом.
    let !result = force (parallelGauss matrix)
    
    end <- getCurrentTime
    
    -- Выводим время
    printf "Done! Time taken: %s\n" (show (diffUTCTime end start))
    
    -- Для отладки: если матрица маленькая, покажем результат
    if size <= 5
        then print result
        else putStrLn "Result is too large to print."