-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.Firmware
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- TBD.
-------------------------------------------------------------------------------
module BoardControlCmds where

import System.Hardware.Haskino
import Data.Word
import Data.Bits

import FirmwareCmds 

processBoardControlCommand :: [Word8] -> Arduino ()
processBoardControlCommand m = 
    case head m of
        c | c == firmwareCmdVal BC_CMD_SYSTEM_RESET -> processSystemReset $ tail m
          | c == firmwareCmdVal BC_CMD_SET_PIN_MODE -> processSetPinMode $ tail m
          | c == firmwareCmdVal BC_CMD_DELAY_MILLIS -> processDelayMillis $ tail m
          | c == firmwareCmdVal BC_CMD_DELAY_MICROS -> processDelayMicros $ tail m
        _                                            -> return ()


processSystemReset :: [Word8] -> Arduino ()
processSystemReset m = return ()

processSetPinMode :: [Word8] -> Arduino ()
processSetPinMode m = return ()

processDelayMillis :: [Word8] -> Arduino ()
processDelayMillis m = do
    if (head m == 0) && (m !! 1 == exprTypeVal EXPR_WORD32)
    then delayMillis $ fromIntegral (m !! 2) `shiftL` 24 .|.
                       fromIntegral (m !! 3) `shiftL` 16 .|.
                       fromIntegral (m !! 4) `shiftL`  8 .|.
                       fromIntegral (m !! 5)
    else return ()

processDelayMicros :: [Word8] -> Arduino ()
processDelayMicros m = 
    if (head m == 0) && (m !! 1 == exprTypeVal EXPR_WORD32)
    then delayMicros $ fromIntegral (m !! 2) `shiftL` 24 .|.
                       fromIntegral (m !! 3) `shiftL` 16 .|.
                       fromIntegral (m !! 4) `shiftL`  8 .|.
                       fromIntegral (m !! 5)
    else return ()
