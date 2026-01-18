{-# LANGUAGE TemplateHaskell #-}

module SelfCorrectingLenses where

import Control.Lens

{- 
   Ref: Optics By Example - Chapter 3.6: Data Correction
   
   Sometimes we want a lens to enforce invariants when setting values.
   For example, a "Time" struct where minutes must be 0-59.
   
   Note: This violates the "Get-Put" law (putting back what you got might result in a different value),
   but is often accepted for normalization logic.
-}

data Time = Time 
  { _hours :: Int
  , _minutes :: Int 
  } deriving Show

makeLenses ''Time

-- A "smart" lens for minutes that handles overflow
-- If you set 75 minutes, it adds 1 hour and sets 15 minutes.
smartMinutes :: Lens' Time Int
smartMinutes = lens _minutes setMinutes
  where
    setMinutes :: Time -> Int -> Time
    setMinutes t m = 
      let (hAdd, mNew) = m `divMod` 60
      in t & minutes .~ mNew
           & hours %~ (+ hAdd)

-- Example: Clamping a value
data Percentage = Percentage { _val :: Int } deriving Show
makeLenses ''Percentage

clampedVal :: Lens' Percentage Int
clampedVal = lens _val (\p v -> p & val .~ max 0 (min 100 v))

exampleCorrecting :: (Time, Percentage)
exampleCorrecting =
  let 
    -- Overflow minutes
    t = Time 10 0 & smartMinutes .~ 75 -- Should become Time 11 15
    
    -- Clamp percentage
    p = Percentage 50 & clampedVal .~ 150 -- Should become Percentage 100
  in (t, p)
