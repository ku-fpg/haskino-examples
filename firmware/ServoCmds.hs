-------------------------------------------------------------------------------
-- |
-- Module      :  ServoCmds
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- TBD.
-------------------------------------------------------------------------------
module ServoCmds where

import System.Hardware.Haskino
import Data.Word
import Data.Bits

import Comms
import FirmwareCmds 

processServoCommand :: [Word8] -> Arduino ()
processServoCommand m = 
    case head m of
        c | c == firmwareCmdVal SRVO_CMD_ATTACH       -> processAttach      $ tail m
          | c == firmwareCmdVal SRVO_CMD_DETACH       -> processDetach      $ tail m
          | c == firmwareCmdVal SRVO_CMD_WRITE        -> processWrite       $ tail m
          | c == firmwareCmdVal SRVO_CMD_WRITE_MICROS -> processWriteMicros $ tail m
          | c == firmwareCmdVal SRVO_CMD_READ         -> processRead        $ tail m
          | c == firmwareCmdVal SRVO_CMD_READ_MICROS  -> processReadMicros  $ tail m
        _                                             -> return ()

processAttach :: [Word8] -> Arduino ()
processAttach m = do
    if (m !! 1 == exprTypeVal EXPR_WORD8  ) && (m !! 2 == 0) &&
       (m !! 4 == exprTypeVal EXPR_WORD16 ) && (m !! 5 == 0) &&
       (m !! 8 == exprTypeVal EXPR_WORD16 ) && (m !! 9 == 0)
    then do
        s <- servoAttachMinMax (m !! 3) (fromIntegral (m !! 6)  `shiftL` 8 .|.
                                         fromIntegral (m !! 7 ) ) (
                                         fromIntegral (m !! 10) `shiftL` 8 .|.
                                         fromIntegral (m !! 11) )
        sendReply (firmwareReplyVal SRVO_RESP_ATTACH) $ ( exprTypeVal EXPR_WORD8     ) :
                                                        ( exprOpVal EXPR_LIT         ) :
                                                        s : []
    else return ()

processDetach :: [Word8] -> Arduino ()
processDetach m = do
    if (m !!  0 == exprTypeVal EXPR_WORD8 ) && (m !!  1 == 0) 
    then servoDetach (m !! 2)
    else return ()

processWrite :: [Word8] -> Arduino ()
processWrite m = do 
    if (head m == exprTypeVal EXPR_WORD8 )  && (m !! 1 == 0) &&
       (m !! 3 == exprTypeVal EXPR_WORD16) && (m !! 4 == 0) 
    then do
        let deg = fromIntegral (m !! 5) `shiftL` 8 .|.
                  fromIntegral (m !! 6) 
        servoWrite (m !! 2) deg
    else return ()

processWriteMicros :: [Word8] -> Arduino ()
processWriteMicros m = do 
    if (head m == exprTypeVal EXPR_WORD8 )  && (m !! 1 == 0) &&
       (m !! 3 == exprTypeVal EXPR_WORD16) && (m !! 4 == 0) 
    then do
        let us = fromIntegral (m !! 5) `shiftL` 8 .|.
                 fromIntegral (m !! 6) 
        servoWrite (m !! 2) us
    else return ()

processRead :: [Word8] -> Arduino ()
processRead m = do
    if (m !! 1 == exprTypeVal EXPR_WORD8) && (m !! 2 == 0)
    then do
        d <- servoRead (m !! 3)
        sendReply (firmwareReplyVal SRVO_RESP_READ) $ ( exprTypeVal EXPR_WORD16     ) :
                                                      ( exprOpVal EXPR_LIT          ) :
                                                      ( fromIntegral $ d `shiftR` 8 ) :  (fromIntegral $ d .&. 8) : []
    else return ()

processReadMicros :: [Word8] -> Arduino ()
processReadMicros m = do
    if (m !! 1 == exprTypeVal EXPR_WORD8) && (m !! 2 == 0)
    then do
        u <- servoReadMicros (m !! 3)
        sendReply (firmwareReplyVal SRVO_RESP_READ) $ ( exprTypeVal EXPR_WORD16     ) :
                                                      ( exprOpVal EXPR_LIT          ) :
                                                      ( fromIntegral $ u `shiftR` 8 ) :  (fromIntegral $ u .&. 8) : []
    else return ()


