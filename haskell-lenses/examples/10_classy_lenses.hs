{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module ClassyLenses where

import Control.Lens

{- 
   Ref: Optics By Example - Chapter 14: Classy Lenses
   
   "Classy Lenses" decouple your code from concrete data types.
   Instead of writing a function that takes a 'Person', you write a function
   that takes "anything that has a name".
   
   The pattern:
   class HasName a where
     name :: Lens' a String
-}

data Person = Person { _personName :: String, _personAge :: Int }
data Pet = Pet { _petName :: String, _petOwner :: String }

makeLenses ''Person
makeLenses ''Pet

-- Define the class
class HasName a where
  name :: Lens' a String

-- Implement instances
instance HasName Person where
  name = personName

instance HasName Pet where
  name = petName

-- A function that works on BOTH Person and Pet
upperCaseName :: HasName a => a -> a
upperCaseName x = x & name %~ map toUpper
  where toUpper c = c -- (Import Data.Char toUpper in real code)

-- makeClassy automation
-- Control.Lens can generate this boilerplate for you!
data Company = Company { _companyName :: String, _companyCEO :: Person }
makeClassy ''Company

-- Now we have 'HasCompany' class automatically.
-- And because Company contains a Person, we can make it an instance of HasName too!
instance HasName Company where
  name = companyCEO . name

exampleClassy :: (Person, Pet)
exampleClassy = 
  let p = Person "Alice" 30
      d = Pet "Fido" "Alice"
  in (upperCaseName p, upperCaseName d)
