-------------------------------------------------------------------------------
-- |
-- Module      :  DigitalCmds
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- TBD.
-------------------------------------------------------------------------------
module DigitalCmds where

import System.Hardware.Haskino
import Data.Word

import Comms
import FirmwareCmds 

processDigitalCommand :: [Word8] -> Arduino ()
processDigitalCommand m = 
    case head m of
        c | c == firmwareCmdVal DIG_CMD_READ_PIN   -> processReadPin $ tail m
          | c == firmwareCmdVal DIG_CMD_WRITE_PIN  -> processWritePin $ tail m
          | c == firmwareCmdVal DIG_CMD_READ_PORT  -> processReadPort $ tail m
          | c == firmwareCmdVal DIG_CMD_WRITE_PORT -> processWritePort $ tail m
        _                                          -> return ()

processReadPin :: [Word8] -> Arduino ()
processReadPin m = do
    if (head m == exprTypeVal EXPR_WORD8) && (m !! 1 == 0)
    then do
        b <- digitalRead $ m !! 2
        sendReply (firmwareReplyVal DIG_RESP_READ_PIN) $ (if b then 1 else 0) : []
    else return ()

processWritePin :: [Word8] -> Arduino ()
processWritePin m = do
    if (head m == exprTypeVal EXPR_WORD8) && (m !! 1 == 0) &&
       (m !! 3 == exprTypeVal EXPR_WORD8) && (m !! 4 == 0)
    then digitalWrite (m !! 2) $ if m !! 5 == 0 then False else True
    else return ()

processReadPort :: [Word8] -> Arduino ()
processReadPort m = do 
    if (head m == exprTypeVal EXPR_WORD8) && (m !! 1 == 0) &&
       (m !! 3 == exprTypeVal EXPR_WORD8) && (m !! 4 == 0)
    then do
        p <- digitalPortRead (m !! 2) (m !! 5)
        sendReply (firmwareReplyVal DIG_RESP_READ_PORT) $ p : []
    else return ()

processWritePort :: [Word8] -> Arduino ()
processWritePort m = do 
    if (head m == exprTypeVal EXPR_WORD8) && (m !! 1 == 0) &&
       (m !! 3 == exprTypeVal EXPR_WORD8) && (m !! 4 == 0) &&
       (m !! 6 == exprTypeVal EXPR_WORD8) && (m !! 7 == 0)
    then digitalPortWrite (m !! 2) (m !! 5) (m !! 8)
    else return ()
