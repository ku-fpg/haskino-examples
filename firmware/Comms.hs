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

portNum :: Word8
portNum = 0

hdlcEscape :: Word8
hdlcEscape = 0x7D

hdlcFrameFlag :: Word8
hdlcFrameFlag = 0x7E

hdlcMask :: Word8
hdlcMask = 0x20

readChar :: Arduino Word8
readChar = do
    a <- serialAvailable portNum
    if a > 0
    then do
        s <- serialRead portNum
        return $ fromIntegral s
    else readChar

checkFrame :: [Word8] -> Word8 -> Arduino [Word8]
checkFrame f ch = do
    let l = ((length f) - 1)
    let ch' = ch - (f !! l)
    if ch'  == f !! l
    then return $ take l f
    else return []

readFrame :: RemoteRef Word8 -> Arduino [Word8]
readFrame ref = readFrame' []
  where
    readFrame' :: [Word8] -> Arduino [Word8]
    readFrame' l = do
        c <- readChar
        if c == hdlcEscape
        then do
            c' <- readChar
            ch <- readRemoteRef ref
            writeRemoteRef ref (ch + c' `xor` hdlcMask)
            readFrame' $ c' `xor` hdlcMask : l
        else do
            if c == hdlcFrameFlag 
            then do
                ch' <- readRemoteRef ref
                checkFrame (reverse l) ch'
            else do
                ch'' <- readRemoteRef ref
                writeRemoteRef ref (ch'' + c)
                readFrame' $ c : l

sendEncodedByte :: Word8 -> Arduino ()
sendEncodedByte b = 
    if b == hdlcFrameFlag || b == hdlcEscape
    then do
        serialWrite portNum hdlcEscape
        serialWrite portNum $ b `xor` hdlcMask
    else serialWrite portNum b

sendReplyBytes :: [Word8] -> Arduino ()
sendReplyBytes l = sendReplyBytes' $ 0 : l
  where
    check :: [Word8] -> Word8
    check l' = head l' + l' !! 1

    sendReplyBytes' :: [Word8] -> Arduino ()
    sendReplyBytes' l' = 
        if length l' == 2
        then sendEncodedByte $ head l'
        else do
            sendEncodedByte $ l' !! 1
            sendReplyBytes' $ check l' : drop 2 l'

sendReply :: Word8 -> [Word8] -> Arduino ()
sendReply ty rep = do 
    sendReplyBytes $ ty : rep
    serialWrite portNum hdlcFrameFlag
