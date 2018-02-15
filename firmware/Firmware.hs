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
import I2CCmds
import SerialCmds
import ServoCmds
import StepperCmds

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
              | c == firmwareTypeVal I2C_CMD_TYPE  -> processI2CCommand m
              | c == firmwareTypeVal SER_CMD_TYPE  -> processSerialCommand m
              | c == firmwareTypeVal STEP_CMD_TYPE -> processStepperCommand m
              | c == firmwareTypeVal SVRO_CMD_TYPE -> processServoCommand m
            _             -> return ()

firmware :: RemoteRef Word8 -> Arduino ()
firmware cr = firmware'
  where
    firmware' :: Arduino ()
    firmware'  = do
        f <- readFrame cr
        parseMessage f
        firmware'

allocRef :: Arduino ()
allocRef = do
    cr <- newRemoteRef 0
    firmware cr

main :: IO ()
main = compileProgram allocRef "firmware.ino"

