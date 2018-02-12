-------------------------------------------------------------------------------
-- |
-- Module      :  AnalogCmds
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- TBD.
-------------------------------------------------------------------------------
module AnalogCmds where

import System.Hardware.Haskino
import Data.Word
import Data.Bits

import Comms
import FirmwareCmds 

processAnalogCommand :: [Word8] -> Arduino ()
processAnalogCommand m = 
    case head m of
        c | c == firmwareCmdVal ALG_CMD_READ_PIN   -> processReadPin   $ tail m
          | c == firmwareCmdVal ALG_CMD_WRITE_PIN  -> processWritePin  $ tail m
          | c == firmwareCmdVal ALG_CMD_TONE_PIN   -> processTonePin   $ tail m
          | c == firmwareCmdVal ALG_CMD_NOTONE_PIN -> processNoTonePin $ tail m
        _                                          -> return ()

processReadPin :: [Word8] -> Arduino ()
processReadPin m = do
    if (m !! 1 == exprTypeVal EXPR_WORD8) && (m !! 2 == 0)
    then do
        a <- analogRead $ m !! 3
        sendReply (firmwareReplyVal ALG_RESP_READ_PIN) $ ( exprTypeVal EXPR_WORD16        ) :
                                                         ( exprOpVal EXPR_LIT             ) :
                                                         ( fromIntegral $ a .&.      0xFF ) :
                                                         ( fromIntegral $ a `shiftR` 8    ) : []  
    else return ()

processWritePin :: [Word8] -> Arduino ()
processWritePin m = do
    if (head m == exprTypeVal EXPR_WORD8)  && (m !! 1 == 0) &&
       (m !! 3 == exprTypeVal EXPR_WORD16) && (m !! 4 == 0)
    then analogWrite (m !! 2) $ fromIntegral (m !! 6) `shiftL` 8 .|.
                                fromIntegral (m !! 5)
    else return ()

processTonePin :: [Word8] -> Arduino ()
processTonePin m = do 
    if (head m == exprTypeVal EXPR_WORD8)  && (m !! 1 == 0) &&
       (m !! 3 == exprTypeVal EXPR_WORD16) && (m !! 4 == 0) &&
       (m !! 7 == exprTypeVal EXPR_WORD32) && (m !! 8 == 0)
    then do
        let duration = fromIntegral (m !! 11) `shiftL` 24 .|.
                       fromIntegral (m !! 10) `shiftL` 16 .|.
                       fromIntegral (m !! 9 ) `shiftL`  8 .|.
                       fromIntegral (m !! 8 )
            freq =     fromIntegral (m !! 6 ) `shiftL`  8 .|.
                       fromIntegral (m !! 5 )
        if duration == 0
        then tone (m !! 2) freq Nothing
        else tone (m !! 2) freq (Just duration)
    else return ()

processNoTonePin :: [Word8] -> Arduino ()
processNoTonePin m = do 
    if (head m == exprTypeVal EXPR_WORD8) && (m !! 1 == 0) 
    then noTone (m !! 2) 
    else return ()
