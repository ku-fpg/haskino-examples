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
          | c == firmwareCmdVal BS_CMD_REQUEST_TYPE    -> processRequestType    $ tail m
          | c == firmwareCmdVal BS_CMD_REQUEST_MICROS  -> processRequestMicros  $ tail m
          | c == firmwareCmdVal BS_CMD_REQUEST_MILLIS  -> processRequestMillis  $ tail m
          | c == firmwareCmdVal BS_CMD_DEBUG           -> processDebug          $ tail m
        _                                              -> return ()

processRequestVersion :: [Word8] -> Arduino ()
processRequestVersion _ = do
    v <- queryFirmware
    sendReply (firmwareReplyVal BS_RESP_VERSION) $ ( fromIntegral $ v .&.      0xFF  ) : 
                                                   ( fromIntegral $ v `shiftR` 8     ) : []

processRequestType :: [Word8] -> Arduino ()
processRequestType _ = do
    p <- queryProcessor
    sendReply (firmwareReplyVal BS_RESP_TYPE) $ p : []

processRequestMicros :: [Word8] -> Arduino ()
processRequestMicros _ = do 
    us <- micros
    sendReply (firmwareReplyVal BS_RESP_MICROS) $ ( fromIntegral $ us .&.      0xFF  ) :
                                                  ( fromIntegral $ us `shiftR`  8    ) : 
                                                  ( fromIntegral $ us `shiftR` 16    ) : 
                                                  ( fromIntegral $ us `shiftR` 24    ) : [] 

processRequestMillis :: [Word8] -> Arduino ()
processRequestMillis _ = do 
    ms <- millis
    sendReply (firmwareReplyVal BS_RESP_MILLIS) $ ( fromIntegral $ ms .&.      0xFF  ) :
                                                  ( fromIntegral $ ms `shiftR`  8    ) :
                                                  ( fromIntegral $ ms `shiftR` 16    ) : 
                                                  ( fromIntegral $ ms `shiftR` 24    ) : []

processDebug :: [Word8] -> Arduino ()
processDebug ws = do
    sendReply (firmwareReplyVal BS_RESP_STRING) ws

