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
module NameGen (
  applyNG, ContainsVars(..), evalState, NameGen, newName, {-newName0,-} Str(..)
) where

import Control.Monad.State.Strict (State, get, put, evalState)
import qualified Data.Set as Set
import Data.Set (Set, empty, union, singleton, notMember)
import Text.Printf (printf)

import Misc

---- ContainsVars -----

class ContainsVars a where
  vars_in :: a -> Set VarName

instance ContainsVars a => ContainsVars [a] where
  vars_in = foldMap vars_in

instance ContainsVars a => ContainsVars (Maybe a) where
  vars_in = foldMap vars_in

instance (ContainsVars a, ContainsVars b) => ContainsVars (a, b) where
  vars_in (x, y) = vars_in x `union` vars_in y

instance ContainsVars () where
  vars_in _ = empty

data Str = Str { unStr :: String }

instance ContainsVars Str where
  vars_in (Str s) = singleton s

----- NameGen -----

type NameGen = State (Set VarName)

applyNG :: ContainsVars a => (a -> NameGen b) -> a -> b
applyNG f x = evalState (f x) (vars_in x)

-- newName0 isn't used anyplace and NameGenSpec has no tests for it.
-- I'm leaving the hooks for it here in case I decide to resurrect it.

newName :: String -> NameGen String
newName = newName' False
--newName0 = newName' True

newName' :: Bool -> String -> NameGen String
newName' allowPrefix prefix =
  do
    usedNames <- get
    let numbered_candidates = map (printf "%s_%u" prefix) [0::Integer ..]
    let candidates = if allowPrefix then prefix : numbered_candidates
                                    else numbered_candidates
    let name = head $ filter (flip notMember usedNames) $ candidates
    put $ Set.insert name usedNames
    return name
