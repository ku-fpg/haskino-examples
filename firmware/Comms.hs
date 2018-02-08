-------------------------------------------------------------------------------
-- |
-- Module      :  Comms
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- TBD.
-------------------------------------------------------------------------------
module Comms where

import System.Hardware.Haskino
import Data.Word
import Data.Bits

hdlcEscape :: Word8
hdlcEscape = 0x7D

hdlcFrameFlag :: Word8
hdlcFrameFlag = 0x7E

hdlcMask :: Word8
hdlcMask = 0x20

sendEncodedByte :: Word8 -> Arduino ()
sendEncodedByte b = 
    if b == hdlcFrameFlag || b == hdlcEscape
    then do
        serialWrite 0 hdlcEscape
        serialWrite 0 $ b `xor` hdlcMask
    else serialWrite 0 b

sendReplyBytes :: [Word8] -> Arduino ()
sendReplyBytes l = sendReplyBytes' $ 0 : l
  where
    check :: [Word8] -> Word8
    check l' = head l' + l' !! 1

    sendReplyBytes' :: [Word8] -> Arduino ()
    sendReplyBytes' l' = 
        if length l' == 2
        then sendEncodedByte $ check l'
        else do
            sendEncodedByte $ head l'
            sendReplyBytes' $ check l' : drop 2 l'

sendReply :: Word8 -> [Word8] -> Arduino ()
sendReply ty rep = do 
    sendReplyBytes $ ty : rep
    serialWrite 0 hdlcFrameFlag
