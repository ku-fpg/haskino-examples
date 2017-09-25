-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.Echo
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- This example Echos characters received on the serial port to both the
-- the LCD display and back to the serial port.
-------------------------------------------------------------------------------

module Main where

import System.Hardware.Haskino
import Data.Word
import LCD

portNum :: Word8
portNum = 0

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

echo :: Arduino ()
echo = do
    lcd <- lcdRegister hitachi
    lcdBacklightOn lcd
    lcdClear lcd
    lcdHome lcd
    serialBegin portNum 115200
    loop $ do
        waitForInput
        c <- serialRead portNum
        let ch = fromIntegral c
        lcdWriteChar lcd ch
        serialWrite portNum ch
  where
    waitForInput :: Arduino ()
    waitForInput = do
        a <- serialAvailable portNum
        if a > 0
        then return ()
        else waitForInput

echoExample :: IO ()
echoExample = withArduino True "/dev/cu.usbmodem1421" echo

main :: IO ()
main = compileProgram echo "echo.ino"
-- main = blinkExample

