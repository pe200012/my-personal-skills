{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module VirtualFields where

import Control.Lens

{- 
   Ref: Optics By Example - Chapter 3.5: Virtual Fields
   
   A "Virtual Field" is a lens that focuses on data that isn't directly stored 
   in the structure but is computed from it.
   
   It must define both:
   1. How to VIEW the virtual value (Getter)
   2. How to SET the virtual value by updating the underlying storage (Setter)
-}

data Temperature = Temperature 
  { _celsius :: Double 
  } deriving Show

makeLenses ''Temperature

-- A lens for Fahrenheit, even though we only store Celsius
-- F = C * 1.8 + 32
-- C = (F - 32) / 1.8

fahrenheit :: Lens' Temperature Double
fahrenheit = lens getF setF
  where
    getF :: Temperature -> Double
    getF t = (t ^. celsius) * 1.8 + 32
    
    setF :: Temperature -> Double -> Temperature
    setF t f = t & celsius .~ ((f - 32) / 1.8)

exampleVirtual :: (Double, Double)
exampleVirtual = 
  let temp = Temperature 100 -- 100°C (Boiling)
      f = temp ^. fahrenheit -- Should be 212°F
      
      -- We can set Fahrenheit, and it updates Celsius!
      temp2 = temp & fahrenheit .~ 32 -- Freezing
      c = temp2 ^. celsius   -- Should be 0°C
  in (f, c)
