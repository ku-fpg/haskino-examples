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

import Comms
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
processRequestVersion _ = do
    v <- queryFirmware
    serialWriteList 0 $ (firmwareReplyVal BS_RESP_VERSION) : ( fromIntegral $ v `shiftR` 8) : (fromIntegral $ v .&. 8) : []

processRequestType :: [Word8] -> Arduino ()
processRequestType _ = do
    p <- queryProcessor
    ch <- calcChecksum [1,2]
    serialWriteList 0 $ (firmwareReplyVal BS_RESP_TYPE) : p : ch : []

processRequestMicros :: [Word8] -> Arduino ()
processRequestMicros _ = do 
    us <- micros
    serialWriteList 0 $ (firmwareReplyVal BS_RESP_MICROS) : ( fromIntegral $ us `shiftR` 24) : ( fromIntegral $ us `shiftR` 16) : ( fromIntegral $ us `shiftR` 8) :  (fromIntegral $ us .&. 8) : []

processRequestMillis :: [Word8] -> Arduino ()
processRequestMillis _ = do 
    ms <- millis
    serialWriteList 0 $ (firmwareReplyVal BS_RESP_MICROS) : ( fromIntegral $ ms `shiftR` 24) : ( fromIntegral $ ms `shiftR` 16) : ( fromIntegral $ ms `shiftR` 8) :  (fromIntegral $ ms .&. 8) : []

processDebug :: [Word8] -> Arduino ()
processDebug ws = do
    serialWriteList 0 $ (firmwareReplyVal BS_RESP_STRING) :  ws

