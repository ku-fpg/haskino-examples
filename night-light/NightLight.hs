-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Example.NightLight
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-------------------------------------------------------------------------------

module Main where

import           Data.Maybe              (fromMaybe)
import           Data.Word
import           System.Hardware.Haskino hiding (len)

data PlayState = Play | Wait

data Note = B3 | C4 | C4_Sharp | D4 | D4_Sharp | E4 | F4
           | F4_Sharp | G4 | G4_Sharp | A4_Flat | A4 | A4_Sharp
           | B4_Flat | B4 | C5 | C5_Sharp | D5
  deriving (Eq,Show)

frequencies :: [(Note, Word16)]
frequencies = [ (B3, 247)
              , (C4, 261)
              , (C4_Sharp, 277)
              , (D4, 293)
              , (D4_Sharp, 311)
              , (E4 , 329)
              , (F4, 349)
              , (F4_Sharp ,369)
              , (G4, 392)
              , (G4_Sharp, 415)
              , (A4_Flat , 415)
              , (A4,440)
              , (A4_Sharp, 466)
              , (B4_Flat, 466)
              , (B4, 493)
              , (C5, 523)
              , (C5_Sharp, 554)
              , (D5 , 587)
              ]

buzzer, led, photoCell:: Word8
buzzer = 13;
led = 8;
photoCell= 5;

light :: Word16
light = 500 -- dark room

delay :: Word32
delay = 1000

nightLight :: Arduino ()
nightLight = do
  setPinMode buzzer OUTPUT
  setPinMode led OUTPUT
  loop $ do
    l <- analogRead photoCell
    if l < light then
      do
        digitalWrite led True
        leanOnMe
    else
      digitalWrite led False

nightLightExample :: IO ()
nightLightExample = withArduino True "/dev/cu.usbmodem1421" nightLight

main :: IO ()
main = compileProgram nightLight "nightlight.ino"

--void loop(){
--  int val = analogRead(photoCell);
--  if (state == STATE_PLAY ){
--    if ( val < light){
--       digitalWrite(led, HIGH);
--       leanOnMe();
--       songsPlayed++;
--
--       if ( songsPlayed == timesToPlay * numSongs){
--         state = STATE_WAIT;
--       }
--       if ( song == numSongs -1){
--         song = 0;
--       }else{
--         song++;
--       }
--     }else{
--      digitalWrite(led,LOW);
--     }
--  }else{
--    if (analogRead(photoCell) > light){
--       state = STATE_PLAY;
--       songsPlayed = 0;
--       digitalWrite(led,LOW);
--    }
--  }
--}
dtone :: Word8 -> Note -> Word32 -> Arduino ()
dtone buzzerPin  note  len  = do
  let freq = fromMaybe 0 $ lookup note frequencies
  l <- analogRead photoCell
  -- if it is dark turn light on and play music
  if l < light then
    do
       digitalWrite led  True
       analogWrite buzzerPin (fromIntegral freq)
       delayMillis len
       analogWrite buzzerPin 0
  else
       digitalWrite led  False

--int timesToPlay = 1;
--
--int length = 300;
--int light = 500;
--int numSongs = 6;
--int songsPlayed = 0;
--
--int bright = 0;
--int fade = 5;
--
--void setup(){
--  pinMode(buzzer, OUTPUT);
--  pinMode(led, OUTPUT);
--  Serial.begin(9600);
--}

leanOnMe :: Arduino ()
leanOnMe = do
  let len = 200
  delayMillis (len * 2)
  dtone buzzer C4 (len * 2)
  delayMillis len
  dtone buzzer C4 len
  dtone buzzer D4 len
  dtone buzzer E4 len
  dtone buzzer F4 (len * 2)
  delayMillis len
  dtone buzzer F4 len
  dtone buzzer E4 len
  dtone buzzer D4 len


  dtone buzzer C4 (len * 2)
  delayMillis len
  dtone buzzer C4 len
  dtone buzzer D4 len
  dtone buzzer E4 (len * 2)
  dtone buzzer E4 (len * 2)
  delayMillis len
  dtone buzzer D4 (len * 2)
  delayMillis len
  dtone buzzer C4 (len * 2)
  delayMillis len

  dtone buzzer C4 len
  dtone buzzer D4 len
  dtone buzzer E4 len
  dtone buzzer F4 (len * 2)
  delayMillis len
  dtone buzzer F4 len
  dtone buzzer E4 len
  dtone buzzer D4 len
  dtone buzzer C4 (len * 2)
  delayMillis len
  dtone buzzer C4 len
  dtone buzzer D4 len
  dtone buzzer E4 len
  dtone buzzer B3 (len * 2)
  delayMillis len
  dtone buzzer C4 len



