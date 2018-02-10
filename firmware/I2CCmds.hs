-------------------------------------------------------------------------------
-- |
-- Module      :  I2CCmds
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- TBD.
-------------------------------------------------------------------------------
module I2CCmds where

import System.Hardware.Haskino
import Data.Word

import Comms
import FirmwareCmds 

processI2CCommand :: [Word8] -> Arduino ()
processI2CCommand m = 
    case head m of
        c | c == firmwareCmdVal I2C_CMD_CONFIG -> processConfig $ tail m
          | c == firmwareCmdVal I2C_CMD_READ   -> processRead   $ tail m
          | c == firmwareCmdVal I2C_CMD_WRITE  -> processWrite  $ tail m
        _                                      -> return ()

processRead :: [Word8] -> Arduino ()
processRead m = do
    if (head m == exprTypeVal EXPR_WORD8) && (m !! 1 == 0) &&
       (m !! 3 == exprTypeVal EXPR_WORD8) && (m !! 4 == 0)
    then do
        l <- i2cRead (m !! 2)  (m !! 5)
        sendReply (firmwareReplyVal I2C_RESP_READ) $ ( fromIntegral $ length l) : l
    else return ()

processWrite :: [Word8] -> Arduino ()
processWrite m = do
    if (head m == exprTypeVal EXPR_WORD8) && (m !! 1 == 0) &&
       (m !! 3 == exprTypeVal EXPR_LIST8) && (m !! 4 == 0) &&
       (length m == (fromIntegral (m !! 5)) + 6)
    then i2cWrite (m !! 2) (drop 6 m)
    else return ()

processConfig :: [Word8] -> Arduino ()
processConfig _ = i2cConfig 
