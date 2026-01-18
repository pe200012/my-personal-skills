{-# LANGUAGE TemplateHaskell #-}

module SumTypesPrisms where

import Control.Lens

-- Prisms are for Sum Types (Enums/Unions)
data Response
  = Success String
  | Error Int
  deriving (Show)

-- Generate Prisms: _Success, _Error
makePrisms ''Response

-- 1. Construction (Review)
-- Use the prism to construct the value (using #)
okResp :: Response
okResp = _Success # "Operation complete"

-- 2. Matching (Preview)
-- Check if a value matches a constructor (^?)
isSuccess :: Response -> Maybe String
isSuccess r = r ^? _Success

-- 3. Updating safely
-- Only update if it matches the constructor
handleError :: Response -> Response
handleError r = r & _Error %~ (+ 400)
-- If r was (Success "ok"), it remains unchanged.
-- If r was (Error 4), it becomes (Error 404).

-- 4. Standard Prisms
-- _Left, _Right for Either
-- _Just, _Nothing for Maybe
maybeInt :: Maybe Int
maybeInt = Just 5

incrementMaybe :: Maybe Int
incrementMaybe = maybeInt & _Just %~ (+1) 
-- Just 6

eitherVal :: Either String Int
eitherVal = Left "fail"

-- Update only if Right
updateRight :: Either String Int
updateRight = eitherVal & _Right %~ (*10)
-- Left "fail" (unchanged)
