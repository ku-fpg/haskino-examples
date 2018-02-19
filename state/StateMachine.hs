-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Deep.State
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- TBD
-------------------------------------------------------------------------------

module Main where

import System.Hardware.Haskino
import Data.Word
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

-- | There are 5 keys on the OSepp shield.
data Key = KeyNone
         | KeyRight
         | KeyLeft
         | KeyUp
         | KeyDown
         | KeySelect

-- fromEnum is currently not working with translation,
-- so we do not use derviving Enum here.

keyValue :: Key -> Word8
keyValue KeyNone   = 0
keyValue KeyRight  = 1
keyValue KeyLeft   = 2
keyValue KeyUp     = 3
keyValue KeyDown   = 4
keyValue KeySelect = 5

getKey :: Arduino Word8
getKey = do
    key <- analogKey
    waitRelease
    delayMillis 100
    return key
      where
        analogKey :: Arduino Word8
        analogKey = do
            v <- analogRead 0
            case v of
              _ | v < 30  -> return (keyValue KeyRight)
              _ | v < 180 -> return (keyValue KeyUp)
              _ | v < 360 -> return (keyValue KeyDown)
              _ | v < 535 -> return (keyValue KeyLeft)
              _ | v < 760 -> return (keyValue KeySelect)
              _           -> analogKey

        waitRelease :: Arduino ()
        waitRelease = do
            v <- analogRead 0
            if v < 760 then waitRelease else return ()

displayState :: LCD -> Word8 -> Word8 -> Arduino ()
displayState lcd s k = do
    lcdHome lcd
    lcdWrite lcd $ (litString "State ") ++ showB s 

theProgram :: Arduino ()
theProgram = do
    lcd <- lcdRegister hitachi
    lcdBacklightOn lcd
    stateMachine lcd

stateMachine :: LCD -> Arduino ()
stateMachine lcd =
    state1 $ keyValue KeyNone
  where
    state1 :: Word8 -> Arduino ()
    state1 k = do
       displayState lcd 1 k
       key <- getKey
       state2 key

    state2 :: Word8 -> Arduino ()
    state2 k = do
       displayState lcd 2 k
       key <- getKey
       case key of
          1 -> state3 key
          2 -> state4 key
          _ -> state2 key

    state3 :: Word8 -> Arduino ()
    state3 k = do
       displayState lcd 3 k
       key <- getKey
       case key of
          1 -> state4 key
          2 -> state2 key
          _ -> state3 key

    state4 :: Word8 -> Arduino ()
    state4 k = do
       displayState lcd 4 k
       key <- getKey
       case key of
          1 -> state2 key
          2 -> state3 key 
          _ -> state4 key

main :: IO ()
main = compileProgram theProgram "stateMachine.ino"
-- main = lcdExample
