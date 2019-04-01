{-
Copyright 2019 Adobe. All rights reserved. This file is licensed to you under
the Apache License, Version 2.0 (the "License"); you may not use this file
except in compliance with the License. You may obtain a copy of the License at
http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed
under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
REPRESENTATIONS OF ANY KIND, either express or implied. See the License for the
specific language governing permissions and limitations under the License.
-}
-----------------------------------------------------------------------------
-- |
-- Modified from the following:
-- Module      :  Data.List.Unique
-- Copyright   :  (c) Volodymyr Yashchenko
-- License     :  BSD3
--
-- Maintainer  :  ualinuxcn@gmail.com
-- Stability   :  Unstable
-- Portability :  portable
--
-- Library provides the functions to find unique and duplicate elements in the list

module Unique
   ( sortUniq
   , repeated
   , allUnique
   )
   where


import           Data.List           (group, sort)
import           Data.List.Extra     (nubOrd)

-- | 'sortUniq' sorts the list and removes the duplicates of elements. Example:
--
-- > sortUniq "foo bar" == " abfor"

sortUniq :: Ord a => [a] -> [a]
sortUniq = sort . nubOrd

sg :: Ord a => [a] -> [[a]]
sg = group . sort

filterByLength :: Ord a => (Int -> Bool) -> [a] -> [[a]]
filterByLength p = filter (p . length) . sg

-- | 'repeated' finds only the elements that are present more than once in the list. Example:
--
-- > repeated  "foo bar" == "o"

repeated :: Ord a => [a] -> [a]
repeated = repeatedBy (>1)

-- | The repeatedBy function behaves just like repeated, except it uses a user-supplied equality predicate.
--
-- > repeatedBy (>2) "This is the test line" == " eist"

repeatedBy :: Ord a => (Int -> Bool) -> [a] -> [a]
repeatedBy p = map head . filterByLength p

-- | 'allUnique' checks whether all elements of the list are unique
--
-- > allUnique "foo bar" == False
-- > allUnique ['a'..'z'] == True
-- > allUnique [] == True (!)
-- Since 0.4.7

allUnique :: Ord a => [a] -> Bool
allUnique = all ( (==) 1 . length) . sg
