{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}

module GenericLensExample where

import Control.Lens
import Data.Generics.Labels ()
import GHC.Generics (Generic)

-- generic-lens allows accessing fields without makeLenses
-- You just need GHC.Generics and OverloadedLabels

data Person = Person
  { name :: String
  , age  :: Int
  , address :: Address
  } deriving (Show, Generic)

data Address = Address
  { street :: String
  , city   :: String
  } deriving (Show, Generic)

-- Usage Examples

person1 :: Person
person1 = Person "Alice" 30 (Address "Main St" "New York")

-- 1. Getters with #field
getName :: String
getName = person1 ^. #name

-- 2. Setters with #field
setName :: Person
setName = person1 & #name .~ "Bob"

-- 3. Nested updates
-- Note: You can mix #labels with standard lens operators
updateStreet :: Person
updateStreet = person1 & #address . #street .~ "Broadway"

-- 4. Modifiers
birthday :: Person
birthday = person1 & #age %~ (+1)

-- 5. Type-based lenses (Data.Generics.Product.Typed)
-- Useful when a type appears only once in the record
-- import Data.Generics.Product.Typed (typed)
-- getAddr :: Address
-- getAddr = person1 ^. typed @Address
