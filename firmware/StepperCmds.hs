-------------------------------------------------------------------------------
-- |
-- Module      :  StepperCmds
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- TBD.
-------------------------------------------------------------------------------
module StepperCmds where

import System.Hardware.Haskino
import Data.Word
import Data.Bits

import Comms
import FirmwareCmds 

processStepperCommand :: [Word8] -> Arduino ()
processStepperCommand m = 
    case head m of
        c | c == firmwareCmdVal STEP_CMD_2PIN      -> process2Pin     $ tail m
          | c == firmwareCmdVal STEP_CMD_4PIN      -> process4Pin     $ tail m
          | c == firmwareCmdVal STEP_CMD_SET_SPEED -> processSetSpeed $ tail m
          | c == firmwareCmdVal STEP_CMD_STEP      -> processStep     $ tail m
        _                                          -> return ()

process2Pin :: [Word8] -> Arduino ()
process2Pin m = do
    if (m !! 1 == exprTypeVal EXPR_WORD16) && (m !! 2 == 0) &&
       (m !! 5 == exprTypeVal EXPR_WORD8 ) && (m !! 6 == 0) &&
       (m !! 8 == exprTypeVal EXPR_WORD8 ) && (m !! 9 == 0)
    then do
        s <- stepper2Pin (fromIntegral (m !! 4) `shiftL` 8 .|.
                          fromIntegral (m !! 3)                ) (m !! 7) (m !! 10)
        sendReply (firmwareReplyVal STEP_RESP_2PIN) $ ( exprTypeVal EXPR_WORD8     ) :
                                                      ( exprOpVal EXPR_LIT         ) :
                                                      s : []
    else return ()

process4Pin :: [Word8] -> Arduino ()
process4Pin m = do
    if (m !!  1 == exprTypeVal EXPR_WORD16) && (m !!  2 == 0) &&
       (m !!  5 == exprTypeVal EXPR_WORD8 ) && (m !!  6 == 0) &&
       (m !!  8 == exprTypeVal EXPR_WORD8 ) && (m !!  9 == 0) &&
       (m !! 11 == exprTypeVal EXPR_WORD8 ) && (m !! 12 == 0) &&
       (m !! 14 == exprTypeVal EXPR_WORD8 ) && (m !! 15 == 0)
    then do
        s <- stepper4Pin (fromIntegral (m !! 6) `shiftL` 24 .|.
                          fromIntegral (m !! 5) `shiftL` 16 .|.
                          fromIntegral (m !! 4) `shiftL` 8  .|.
                          fromIntegral (m !! 3)                 ) (m !! 7) (m !! 10) (m !! 13) (m !! 16)
        sendReply (firmwareReplyVal STEP_RESP_2PIN) $ ( exprTypeVal EXPR_WORD8     ) :
                                                      ( exprOpVal EXPR_LIT         ) :
                                                      s : []
    else return ()

processSetSpeed :: [Word8] -> Arduino ()
processSetSpeed m = do 
    if (head m == exprTypeVal EXPR_WORD8)  && (m !! 1 == 0) &&
       (m !! 3 == exprTypeVal EXPR_WORD32) && (m !! 4 == 0) 
    then do
        let speed = fromIntegral (m !! 8) `shiftL` 24 .|.
                    fromIntegral (m !! 7) `shiftL` 16 .|.
                    fromIntegral (m !! 6) `shiftL`  8 .|.
                    fromIntegral (m !! 5)
        stepperSetSpeed (m !! 2) speed
    else return ()

processStep :: [Word8] -> Arduino ()
processStep m = do 
    if (m !! 1 == exprTypeVal EXPR_WORD8 ) && (m !! 2 == 0) &&
       (m !! 4 == exprTypeVal EXPR_WORD16) && (m !! 5 == 0) 
    then do
        let steps = fromIntegral (m !! 7) `shiftL` 8 .|.
                    fromIntegral (m !! 6)
        stepperSetSpeed (m !! 3) steps
        sendReply (firmwareReplyVal STEP_RESP_STEP) []
    else return ()

