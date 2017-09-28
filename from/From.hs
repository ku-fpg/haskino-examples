-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.Blink
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.  This version is
-- written with the expression based version of the commands and procedures
-- introduced in version 0.3 of Haskino
-------------------------------------------------------------------------------

module Main where

import Data.Word
import System.Hardware.Haskino

test_deep' :: Expr Word8 -> Expr Word8 -> Arduino (Expr ())
test_deep' i j = do
    appLambda "test" (appExpr (appExpr test_deep_lam j) i)

test_deep_lam' :: Expr Word8 -> Expr Word8 -> Arduino (Expr ())
test_deep_lam' i j = do
    digitalWriteE (i+j) (lit True)

test_deep_lam :: Arduino (Expr ())
test_deep_lam =
    lamExpr (RemArgW8 0) (lamExpr (RemArgW8 1) (test_deep_lam' (RemArgW8 0) (RemArgW8 1)))

test :: Arduino (Expr ())
test = test_deep' (lit 3) (lit 4)

main :: IO ()
main = compileProgramE test "test.ino"
-- main = blinkExample

