{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module EsotericOptics where
import Control.Lens
import Data.Data
import Data.Generics.Uniplate.Data () -- for Biplate instances if needed, usually Plated is enough for Data
import GHC.Generics (Generic)
{- 
   Ref: Hackage - Control.Lens
   
   Beyond the standard Lens/Prism/Traversal, there are specialized optics 
   that are incredibly useful for specific domains.
   
   1. Plated: "Uniplate" style traversals. 
      Find every occurrence of a type 'a' inside a recursive structure 's'.
      Key operators: cosmos, universe, biplate.
      
   2. Wrapped: Isomorphisms for Newtypes.
      Key optic: _Wrapped, _Unwrapped.
      
   3. Equality: The strongest optic (Witnesses that a ~ b).
      Key optic: simple, equality.
-}
-- === 1. Plated ===
-- Useful for recursive structures (Trees, ASTs, JSON, etc.)
data Expr 
  = Val Int 
  | Add Expr Expr 
  | Mul Expr Expr
  deriving (Show, Data) -- Derive Data for Plated auto-derivation
instance Plated Expr 
-- Default instance uses Data.Data to find immediate children
-- Get all values in an expression tree
allValues :: Expr -> [Int]
allValues e = [i | Val i <- universe e]
-- Modify every Int in the tree
doubleAll :: Expr -> Expr
doubleAll = transform $ \e -> case e of
  Val i -> Val (i * 2)
  _     -> e
  
-- === 2. Wrapped ===
-- Automates Newtype wrapping/unwrapping
newtype UserId = UserId { _getUserId :: Int } 
  deriving (Show, Eq)
makeWrapped ''UserId
-- Use _Wrapped to treat UserId as Int
incrementId :: UserId -> UserId
incrementId uid = uid & _Wrapped +~ 1
-- Works in reverse too
createId :: UserId
createId = 0 ^. _Unwrapped
-- === 3. Equality ===
-- Can be used as a Getter, Setter, Lens, Prism, everything.
-- Mostly used for type proofs or trivial conversions.
intEq :: Equality' Int Int
intEq = simple
-- Not extremely common in app code, but fundamental to the theory.