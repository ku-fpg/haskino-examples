-------------------------------------------------------------------------------
-- |
-- Module      :  SerialCmds
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- TBD.
-------------------------------------------------------------------------------
module SerialCmds where

import System.Hardware.Haskino
import Data.Word
import Data.Bits

import Comms
import FirmwareCmds 

processSerialCommand :: [Word8] -> Arduino ()
processSerialCommand m = 
    case head m of
        c | c == firmwareCmdVal SER_CMD_BEGIN      -> processBegin     $ tail m
          | c == firmwareCmdVal SER_CMD_END        -> processEnd       $ tail m
          | c == firmwareCmdVal SER_CMD_READ       -> processRead      $ tail m
          | c == firmwareCmdVal SER_CMD_READ_LIST  -> processReadList  $ tail m
          | c == firmwareCmdVal SER_CMD_WRITE      -> processWrite     $ tail m
          | c == firmwareCmdVal SER_CMD_WRITE_LIST -> processWriteList $ tail m
        _                                          -> return ()

processBegin :: [Word8] -> Arduino ()
processBegin m = do
    if (head m == exprTypeVal EXPR_WORD8) && (m !! 1 == 0) &&
       (m !! 3 == exprTypeVal EXPR_WORD32) && (m !! 4 == 0)
    then serialBegin (m !! 2) $ fromIntegral (m !! 8) `shiftL` 24 .|.
                                fromIntegral (m !! 7) `shiftL` 16 .|.
                                fromIntegral (m !! 6) `shiftL`  8 .|.
                                fromIntegral (m !! 5)
    else return ()

processEnd :: [Word8] -> Arduino ()
processEnd m = do
    if (head m == exprTypeVal EXPR_WORD8) && (m !! 1 == 0) 
    then serialEnd (m !! 2) 
    else return ()

processRead :: [Word8] -> Arduino ()
processRead m = do
    if (m !! 1 == exprTypeVal EXPR_WORD8) && (m !! 2 == 0)
    then do
        s <- serialRead (m !! 3) 
        sendReply (firmwareReplyVal SER_RESP_READ) $ ( exprTypeVal EXPR_WORD32        ) :
                                                     ( exprOpVal EXPR_LIT             ) :
                                                     ( fromIntegral $ s .&.      0xFF ) :
                                                     ( fromIntegral $ s `shiftR`  8   ) : 
                                                     ( fromIntegral $ s `shiftR` 16   ) : 
                                                     ( fromIntegral $ s `shiftR` 24   ) : [] 
    else return ()

processReadList :: [Word8] -> Arduino ()
processReadList m = do
    if (m !! 1 == exprTypeVal EXPR_WORD8) && (m !! 2 == 0) 
    then do
        l <- serialReadList (m !! 3)
        sendReply (firmwareReplyVal SER_RESP_READ_LIST) $ ( exprTypeVal EXPR_LIST8  ) :
                                                          ( exprOpVal EXPR_LIT      ) :
                                                          ( fromIntegral $ length l ) : l
    else return ()

processWrite :: [Word8] -> Arduino ()
processWrite m = do
    if (head m == exprTypeVal EXPR_WORD8) && (m !! 1 == 0) &&
       (m !! 3 == exprTypeVal EXPR_WORD8) && (m !! 4 == 0) 
    then serialWrite (m !! 2) (m !! 5)
    else return ()

processWriteList :: [Word8] -> Arduino ()
processWriteList m = do
    if (head m == exprTypeVal EXPR_WORD8) && (m !! 1 == 0) &&
       (m !! 3 == exprTypeVal EXPR_LIST8) && (m !! 4 == 0) &&
       (length m == (fromIntegral (m !! 5)) + 6)
    then serialWriteList (m !! 2) (drop 6 m)
    else return ()
