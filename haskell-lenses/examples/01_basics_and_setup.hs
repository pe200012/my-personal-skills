{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module BasicsAndSetup where

import Control.Lens

-- Define a simple data type
data Point = Point
  { _x :: Double
  , _y :: Double
  } deriving (Show)

-- Automatically generate lenses for fields starting with _
makeLenses ''Point

-- Example Data
p1 :: Point
p1 = Point { _x = 1.0, _y = 2.0 }

-- 1. Getters (^.)
-- Access fields using the generated lenses
getX :: Double
getX = p1 ^. x  -- Result: 1.0

-- 2. Setters (.~)
-- Set a value (returns a new value, data is immutable)
setP1 :: Point
setP1 = p1 & x .~ 5.0 -- Result: Point { _x = 5.0, _y = 2.0 }

-- 3. Modifiers (%~)
-- Apply a function to a field
modifyP1 :: Point
modifyP1 = p1 & y %~ (+ 3.0) -- Result: Point { _x = 1.0, _y = 5.0 }

-- 4. Function application (&)
-- '&' is just reverse application, making chains readable
chainedOps :: Point
chainedOps = p1 
  & x .~ 10.0
  & y %~ (* 2)
