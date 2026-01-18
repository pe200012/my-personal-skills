{-# LANGUAGE OverloadedStrings #-}

module JsonManipulation where

import Control.Lens
import Data.Aeson.Lens
import Data.Aeson (Value)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Vector as V

-- Assuming 'jsonBlob' is a ByteString containing:
-- {
--   "user": {
--     "name": "Bob",
--     "tags": ["admin", "staff"]
--   }
-- }

-- 1. Reading values
-- Get the name as Text. 'key' focuses on a JSON object key.
-- '_String' is a Prism that matches if the value is a JSON string.
getName :: BL.ByteString -> Maybe Text
getName blob = blob ^? key "user" . key "name" . _String

-- 2. Deep update
-- Change "Bob" to "Robert"
setName :: Value -> Value
setName v = v & key "user" . key "name" . _String .~ "Robert"

-- 3. Array manipulation
-- Add a tag "active" to the tags array
addTag :: Value -> Value
addTag v = v & key "user" . key "tags" . _Array %~ (\vec -> V.snoc vec "active")
-- Note: _Array focuses on the Vector of Values

-- 4. Searching with 'nth'
-- Get the first tag
firstTag :: Value -> Maybe Text
firstTag v = v ^? key "user" . key "tags" . nth 0 . _String
