-------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- TBD.
-------------------------------------------------------------------------------
module Main where

import System.Hardware.Haskino
import Data.Word
import Data.Bits

isEven :: Word8 -> Arduino Bool
isEven 0 = return True
isEven n = isOdd $ n-1

isOdd :: Word8 -> Arduino Bool
isOdd 0 = return False
isOdd n = isEven $ n - 1

prog :: Arduino ()
prog = do
    b <- isOdd 4
    digitalWrite 2 b
    return ()

{-
func1 :: Arduino Bool
func1 = do
    b <- digitalRead 2
    if b
    then return True
    else func2

func2 :: Arduino Bool
func2 = do
    b <- digitalRead 3
    if b
    then return False
    else func1

prog :: Arduino ()
prog = do
    func1
    return ()
    --test 5
-}

main :: IO ()
-- main = putStrLn $ show prog
main = compileProgram prog "mut.ino"


-- main = putStrLn $ show newEvenOdd
