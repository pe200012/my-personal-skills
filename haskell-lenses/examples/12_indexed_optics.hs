{-# LANGUAGE OverloadedStrings #-}

module IndexedOptics where

import Control.Lens
import qualified Data.Map as M

{- 
   Ref: Optics By Example - Chapter 11: Indexed Optics
   
   Indexed optics provide access to the "index" (key or position) of the focus 
   alongside the value itself.
   
   Key operators:
   - itraversed: Traverse with index
   - iover: Modify with access to index
   - ifoldMap: Fold with access to index
-}

-- Example 1: Modifying a list based on index
-- Multiply value by its index
indexedList :: [Int]
indexedList = [10, 10, 10] & iover traversed (\i v -> v + i)
-- Result: [10, 11, 12] (10+0, 10+1, 10+2)

-- Example 2: Filtering a Map based on Keys (Indices)
-- Keep only keys that start with 'a'
dataMap :: M.Map String Int
dataMap = M.fromList [("apple", 1), ("banana", 2), ("avocado", 3)]

filteredMap :: M.Map String Int
filteredMap = dataMap & partsOf (itraversed . indices (\k -> head k == 'a')) %~ id
-- Note: 'indices' filters the traversal based on the index.

-- Example 3: converting Map to list of (Key, Value) string pairs using ifoldMap
kvPairs :: [String]
kvPairs = ifoldMap (\k v -> [k ++ ": " ++ show v]) dataMap

-- Example 4: Re-indexing
-- You can change the index as you traverse!
-- reindexed :: [Int]
-- reindexed = [1,2,3] & reindexed (+1) ...
