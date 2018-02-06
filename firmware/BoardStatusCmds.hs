-------------------------------------------------------------------------------
-- |
-- Module      :  BoardStatusCmds
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- TBD.
-------------------------------------------------------------------------------
module BoardStatusCmds where

import System.Hardware.Haskino
import Data.Word
import Data.Bits

import FirmwareCmds 

processBoardStatusCommand :: [Word8] -> Arduino ()
processBoardStatusCommand m = 
    case head m of
        c | c == firmwareCmdVal BS_CMD_REQUEST_VERSION -> processRequestVersion $ tail m
          | c == firmwareCmdVal BS_CMD_REQUEST_TYPE    -> processRequestType $ tail m
          | c == firmwareCmdVal BS_CMD_REQUEST_MICROS  -> processRequestMicros $ tail m
          | c == firmwareCmdVal BS_CMD_REQUEST_MILLIS  -> processRequestMillis $ tail m
          | c == firmwareCmdVal BS_CMD_DEBUG           -> processDebug $ tail m
        _                                              -> return ()

processRequestVersion :: [Word8] -> Arduino ()
processRequestVersion _ = return ()

processRequestType :: [Word8] -> Arduino ()
processRequestType m = do
    serialWriteList 0 $ (firmwareReplyVal BS_RESP_TYPE) : 0 : []

processRequestMicros :: [Word8] -> Arduino ()
processRequestMicros m = return () 
{-
    us <- micros
    (fromIntegral us `shiftR` 24) :  
    if (head m == 0) && (m !! 1 == exprTypeVal EXPR_WORD32)
    then delayMillis $ fromIntegral (m !! 2) `shiftL` 24 .|.
                       fromIntegral (m !! 3) `shiftL` 16 .|.
                       fromIntegral (m !! 4) `shiftL`  8 .|.
                       fromIntegral (m !! 5)
    else return ()
-}

processRequestMillis :: [Word8] -> Arduino ()
processRequestMillis m = return () 
{-
    ms <- millis
    if (head m == 0) && (m !! 1 == exprTypeVal EXPR_WORD32)
    then delayMicros $ fromIntegral (m !! 2) `shiftL` 24 .|.
                       fromIntegral (m !! 3) `shiftL` 16 .|.
                       fromIntegral (m !! 4) `shiftL`  8 .|.
                       fromIntegral (m !! 5)
    else return ()
-}
processDebug :: [Word8] -> Arduino ()
processDebug _ = return ()

