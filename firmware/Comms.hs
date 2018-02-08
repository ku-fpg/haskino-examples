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

calcChecksum :: [Word8] -> Arduino Word8
calcChecksum l = calcChecksum' $ 0 : l
  where
    check :: [Word8] -> Word8
    check l = head l + l !! 1

    calcChecksum' :: [Word8] -> Arduino Word8
    calcChecksum' l = 
        if length l == 2
        then return $ check l
        else calcChecksum' $ check l : drop 2 l
