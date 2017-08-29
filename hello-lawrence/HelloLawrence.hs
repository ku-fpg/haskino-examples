-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Deep.ScheduledLCDE
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Creates a scheduled task on the Arduino which alternates writing 'Rock',
-- 'Chalk' and 'Jayhawk' to the LCD screen every second and a half.
-- Note: This example requires a Mega2560 board, as the Uno boards do not have
-- enough RAM.
-------------------------------------------------------------------------------

module Main where

import System.Hardware.Haskino
import LCD

hitachi :: LCDController
hitachi = Hitachi44780 { lcdRS = 8
                     , lcdEN = 9
                     , lcdD4 = 4
                     , lcdD5 = 5
                     , lcdD6 = 6
                     , lcdD7 = 7
                     , lcdBL = Just 10
                     , lcdRows = 2
                     , lcdCols = 16
                     , dotMode5x10 = False
                     }

-- Task which will execute on Arduino, write an 'Rock' to the display, delay a
-- second, write a 'Chalk' to the display, delay a second, write a 'Jayhawk'
-- to the display.

theProgram :: Arduino ()
theProgram = do
  lcd <- lcdRegister hitachi
  lcdBacklightOn lcd
  hello lcd

hello :: LCD -> Arduino ()
hello lcd =
  loop $ do
    lcdHome lcd
    lcdWrite lcd $ litString "Rock   "
    delayMillis 1500   
    lcdHome lcd
    lcdWrite lcd $ litString "Chalk  "
    delayMillis 1500   
    lcdHome lcd
    lcdWrite lcd $ litString "Jayhawk"
    delayMillis 1500
    lcdClear lcd

-- Execute this function to run program with firmware interpreter
lcdExample :: IO ()
lcdExample = withArduino True "/dev/cu.usbmodem1421" theProgram

main :: IO ()
main = compileProgram theProgram "helloLawrence.ino"
-- main = lcdExample
