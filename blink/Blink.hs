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

led :: Word8
led = 13

delay :: Word32 -> Word32
delay d = d + 400

blink :: Word32 -> Arduino ()
blink t = do
    -- let led = 13
    -- let delay = 250 + t
    setPinMode led OUTPUT
    loop $ do
        digitalWrite led True
        delayMillis (delay t)
        digitalWrite led False
        delayMillis (delay t)

blinkExample :: IO ()
blinkExample = withArduino True "/dev/cu.usbmodem1421" (blink 500)

main :: IO ()
main = compileProgram (blink 500) "blink.ino"
-- main = blinkExample

