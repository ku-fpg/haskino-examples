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
import AnalogCmds
import BoardControlCmds
import BoardStatusCmds
import Comms
import DigitalCmds
-- import I2CCmds
-- import SerialCmds
--i mport StepperCmds

parseMessage :: [Word8] -> Arduino ()
parseMessage m = do
    if null m 
    then return ()
    else
        case (head m) .&. 0xF0 of
            c | c == firmwareTypeVal BC_CMD_TYPE   -> processBoardControlCommand m
              | c == firmwareTypeVal BS_CMD_TYPE   -> processBoardStatusCommand m
              | c == firmwareTypeVal DIG_CMD_TYPE  -> processDigitalCommand m
              | c == firmwareTypeVal ALG_CMD_TYPE  -> processAnalogCommand m
              -- | c == firmwareTypeVal I2C_CMD_TYPE  -> processI2CCommand m
              -- | c == firmwareTypeVal SER_CMD_TYPE  -> processSerialCommand m
              -- | c == firmwareTypeVal STEP_CMD_TYPE -> processStepperCommand m
            {-
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

