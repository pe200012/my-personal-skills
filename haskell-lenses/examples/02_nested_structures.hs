{-# LANGUAGE TemplateHaskell #-}

module NestedStructures where

import Control.Lens
import Data.Char (toUpper)

data Address = Address
  { _street :: String
  , _city   :: String
  } deriving (Show)
makeLenses ''Address

data User = User
  { _name    :: String
  , _address :: Address
  } deriving (Show)
makeLenses ''User

-- Example Data
user1 :: User
user1 = User "Alice" (Address "123 Main St" "Springfield")

-- 1. Deep Composition
-- Lenses compose with (.) just like functions
-- Access nested field:
getCity :: String
getCity = user1 ^. address . city 
-- "Springfield"

-- 2. Deep Update
-- Modify nested field:
moveToNY :: User
moveToNY = user1 & address . city .~ "New York"
-- User { _name = "Alice", _address = Address { _street = "123 Main St", _city = "New York" } }

-- 3. Deep Modification
-- Uppercase the street name
upperStreet :: User
upperStreet = user1 & address . street %~ (map toUpper)
