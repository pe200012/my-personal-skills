{- 
   Ref: https://oleg.fi/gists/posts/2017-04-18-glassery.html
   
   The "Glassery" blog post by Oleg Grenrus describes the hierarchy of optics based on Profunctors.
   It categorizes optics by the constraints they place on the profunctor 'p'.
   
   Hierarchy (from most specific/strongest to least specific/weakest):
   1. Equality         (p is any Profunctor)
   2. Iso              (p is Profunctor)
   3. Lens             (p is Strong) / Prism (p is Choice)
   4. AffineTraversal  (p is Strong and Choice)
   5. Traversal        (p is Traversing)
   6. Setter           (p is Mapping)
   
   This example file demonstrates how to construct and use these different types of optics
   using the base constructors provided by the lens library.
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module GlasseryHierarchy where

import Control.Lens
import Data.Char (toUpper)
import Numeric.Natural (Natural)

-- 1. ISO
-- An Iso defines a reversible transformation between two types.
-- Use 'iso' to construct one.
-- Law: to . from = id, from . to = id

newtype UserId = UserId Int deriving Show

-- An Iso between UserId and Int
-- iso :: (s -> a) -> (b -> t) -> Iso s t a b
userIdIso :: Iso' UserId Int
userIdIso = iso (\(UserId i) -> i) UserId

exampleIso :: (Int, UserId)
exampleIso = 
  ( UserId 123 ^. userIdIso       -- View: 123
  , 456 ^. from userIdIso         -- Review: UserId 456
  )


-- 2. LENS
-- A Lens focuses on a single piece of data within a larger structure.
-- It requires a getter and a setter.
-- lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b

data Person = Person { _name :: String, _age :: Int } deriving Show

-- Manual lens construction (instead of makeLenses)
nameLens :: Lens' Person String
nameLens = lens _name (\p n -> p { _name = n })

exampleLens :: (String, Person)
exampleLens =
  let p = Person "Alice" 30
  in ( p ^. nameLens                  -- View: "Alice"
     , p & nameLens .~ "Bob"          -- Set: Person "Bob" 30
     )


-- 3. PRISM
-- A Prism focuses on one branch of a Sum type (like Maybe or Either).
-- It requires a "constructor" (review) and a "matcher" (preview).
-- prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
-- Often 'prism'' for simple monomorphic cases.

data Response = Success String | Error Int deriving Show

-- Manual prism construction
_Success :: Prism' Response String
_Success = prism' Success $ \r -> case r of
  Success s -> Just s
  Error _   -> Nothing

examplePrism :: (Maybe String, Response)
examplePrism =
  ( Success "Ok" ^? _Success          -- Preview: Just "Ok"
  , Error 404 ^? _Success             -- Preview: Nothing
  ) -- Note: Review (_Success # "Ok") constructs the value


-- 4. AFFINE TRAVERSAL (Optional)
-- An Affine Traversal focuses on 0 or 1 target.
-- It's weaker than a Lens (might miss) and weaker than a Prism (can't always construct).
-- Represented in 'lens' usually just as a Traversal that happens to hit 0 or 1 times,
-- but strictly it's an optic.

-- Example: Accessing the generic 'Left' of an Either, but only if it's a string
-- This is a bit contrived, usually we compose prisms/lenses.
maybeStringLeft :: Traversal' (Either Int String) String
maybeStringLeft f (Right s) = Right <$> f s
maybeStringLeft _ (Left i)  = pure (Left i)

-- 5. TRAVERSAL
-- A Traversal focuses on 0 or more targets.
-- The key is that it uses an Applicative context to combine results.
-- traversal :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> Traversal s t a b

data Box a = Box a a a deriving Show

-- Traversal over all elements in the Box
boxTraversal :: Traversal (Box a) (Box b) a b
boxTraversal f (Box x y z) = Box <$> f x <*> f y <*> f z

exampleTraversal :: Box Int
exampleTraversal = Box 1 2 3 & boxTraversal %~ (*10) -- Box 10 20 30


-- 6. SETTER
-- A Setter can only modify data, not view it.
-- It uses 'Identity' functor implicitly (or Settable).
-- sets :: ((a -> b) -> s -> t) -> Setter s t a b

-- A setter that maps over a list and applies a function
mappedSetter :: Setter (Box a) (Box b) a b
mappedSetter = sets (\g (Box x y z) -> Box (g x) (g y) (g z))

exampleSetter :: Box String
exampleSetter = Box "a" "b" "c" & mappedSetter %~ (map toUpper) -- Box "A" "B" "C"
