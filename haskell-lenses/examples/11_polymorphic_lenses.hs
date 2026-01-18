{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module PolymorphicLenses where

import Control.Lens
import Data.Char (ord)

{- 
   Ref: Optics By Example - Chapter 4: Polymorphic Optics
   
   A "Simple" Lens has the type: Lens' s a
   which expands to: Lens s s a a
   It means: Changing the focus (a) to another (a) results in the same structure (s).
   
   A "Polymorphic" Lens has the type: Lens s t a b
   It means: Changing the focus (a) to (b) results in a NEW structure (t).
   
   This is powerful for:
   1. Changing type parameters of a container (e.g., Maybe Int -> Maybe String)
   2. Transforming nested types
-}

data Box a = Box { _value :: a } deriving Show

-- Polymorphic lens for Box
-- s=Box a, t=Box b, a=a, b=b
boxValue :: Lens (Box a) (Box b) a b
boxValue = lens _value (\_ v -> Box v)

examplePolymorphic :: (Box Int, Box String)
examplePolymorphic =
  let intBox = Box 123
      -- Change Int to String inside the Box!
      strBox = intBox & boxValue %~ show
  in (intBox, strBox)

-- Tuple examples are naturally polymorphic in lens
exampleTuple :: (Int, Bool)
exampleTuple = (1, "hello") & _2 .~ True
-- Here: (Int, String) -> (Int, Bool)
-- _2 :: Lens (a, b) (a, c) b c

-- Traversals can also be polymorphic
exampleTraverse :: [String]
exampleTraverse = [1, 2, 3] & traversed %~ show
-- [Int] -> [String]
