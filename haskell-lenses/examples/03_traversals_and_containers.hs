{-# LANGUAGE TemplateHaskell #-}

module TraversalsAndContainers where

import Control.Lens
import qualified Data.Map as M

-- 1. Lists and 'traversed'
-- Modify all elements in a list
allIncremented :: [Int]
allIncremented = [1, 2, 3] & traversed %~ (+1)
-- Result: [2, 3, 4]

-- Modify a specific struct inside a list
data Item = Item { _val :: Int } deriving Show
makeLenses ''Item

items :: [Item]
items = [Item 1, Item 2, Item 3]

updateItems :: [Item]
updateItems = items & traversed . val %~ (*10)
-- Result: [Item 10, Item 20, Item 30]

-- 2. Maps: 'at' vs 'ix'
myMap :: M.Map String Int
myMap = M.fromList [("foo", 1), ("bar", 2)]

-- 'ix' targets an existing element. If it's missing, nothing happens (no error).
updateBar :: M.Map String Int
updateBar = myMap & ix "bar" %~ (*100)
-- Result: fromList [("foo",1),("bar",200)]

safeUpdateMissing :: M.Map String Int
safeUpdateMissing = myMap & ix "baz" %~ (*100)
-- Result: Unchanged (no "baz" key)

-- 'at' targets the Maybe value at a key. Allows inserting or deleting.
-- Delete "foo":
deleteFoo :: M.Map String Int
deleteFoo = myMap & at "foo" .~ Nothing

-- Insert "baz":
insertBaz :: M.Map String Int
insertBaz = myMap & at "baz" .~ Just 99
