{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Parallel.Strategies (using, parList, rdeepseq)
import Control.DeepSeq (force)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)

type Row = [Double]
type Matrix = [Row]

eliminate :: Row -> Row -> Row
eliminate pivotRow targetRow =
    let factor = head targetRow / head pivotRow
    in zipWith (\p t -> t - factor * p) pivotRow targetRow

parallelGauss :: Matrix -> Matrix
parallelGauss [] = []
parallelGauss (pivotRow:rows) =
    let 
        nextRows = map (eliminate pivotRow) rows
        parNextRows = nextRows `using` parList rdeepseq
    in pivotRow : parallelGauss (map tail parNextRows)

generateMatrix :: Int -> Matrix
generateMatrix n = 
    [ [ fromIntegral (i + j) | j <- [1..(n+1)] ] | i <- [1..n] ]

main :: IO ()
main = do
    putStrLn "=== Parallel Gauss Elimination ==="
    
    let size = 1000 
    
    printf "Generating matrix %d x %d...\n" size (size + 1)
    let matrix = generateMatrix size
    
    putStrLn "Starting calculation..."
    
    start <- getCurrentTime
    
    let !result = force (parallelGauss matrix)
    
    end <- getCurrentTime

    printf "Done! Time taken: %s\n" (show (diffUTCTime end start))

    if size <= 5
        then print result
        else putStrLn "Result is too large to print."
