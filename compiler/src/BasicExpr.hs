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
module BasicExpr(
  app1,
  Expr(..), Shape, Bounds(..), litI, litR,
  nobounds, lobound, hibound, bounds, bmap,
  module Number
  )
where

import Misc
import NameGen (ContainsVars(..))
import Number
import qualified Data.Set as Set

data Expr = Var VarName | Lit Number | Apply FctName [Expr]
  deriving (Eq, Show)

instance ContainsVars Expr where
  vars_in (Var v) = Set.singleton v
  vars_in (Lit _) = Set.empty
  vars_in (Apply _ args) = vars_in args

app1 :: FctName -> Expr -> Expr
app1 fctname arg = Apply fctname [arg]

type Shape = [Expr]

data Bounds = Bounds{ lower_bound, upper_bound :: Maybe Expr }
  deriving (Eq, Show)

instance ContainsVars Bounds where
  vars_in (Bounds x y) = vars_in x `Set.union` vars_in y

nobounds :: Bounds
nobounds = Bounds Nothing Nothing

lobound, hibound :: Expr -> Bounds
lobound e = Bounds (Just e) Nothing
hibound e = Bounds Nothing (Just e)

bounds :: Expr -> Expr -> Bounds
bounds lo hi = Bounds (Just lo) (Just hi)

bmap :: (Expr -> Expr) -> Bounds -> Bounds
bmap f (Bounds x y) = Bounds (fmap f x) (fmap f y)

litI :: Integer -> Expr
litR :: Scientific -> Expr
litI = Lit . IntVal
litR = Lit . RealVal
