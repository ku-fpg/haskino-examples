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

import FirmwareCmds 
import BoardControlCmds
import BoardStatusCmds
import Comms
import DigitalCmds

parseMessage :: [Word8] -> Arduino ()
parseMessage m = do
    case (head m) .&. 0xF0 of
        c | c == firmwareTypeVal BC_CMD_TYPE  -> processBoardControlCommand m
          | c == firmwareTypeVal BS_CMD_TYPE  -> processBoardStatusCommand m
          | c == firmwareTypeVal DIG_CMD_TYPE -> processDigitalCommand m
        {-
        ALG_CMD_TYPE  -> processAnalogCommand m 
        I2C_CMD_TYPE  -> processI2CCommand m 
        SER_CMD_TYPE  -> processSerialCommand m 
        STEP_CMD_TYPE -> processStepperCommand m 
        SVRO_CMD_TYPE -> processServoCommand m  
        REF_CMD_TYPE  -> processRefernceCommand m
        -}
        _             -> return ()

firmware :: Arduino ()
firmware = do
    cr <- newRemoteRef (0::Word8)
    serialBegin portNum 115200
    loop $ do
        f <- readFrame cr
        parseMessage f
        return ()

main :: IO ()
main = compileProgram firmware "firmware.ino"

