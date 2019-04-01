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
{-# LANGUAGE PatternSynonyms #-}
module BasicModel (TypeS(..), TypeSB(..), DistrExpr(..), Decl(..), ParmDecls,
                  pattern IType, pattern RType, pattern RTypeB, pattern VecType,
                  pattern MatType, pattern RArrType)
where

import qualified Data.Set as Set

import BasicExpr
import Misc
import NameGen (ContainsVars(..))
import AST (ElemType(..))

data TypeS = TypeS ElemType Shape
  deriving (Eq, Show)
data TypeSB = TypeSB ElemType Shape Bounds
  deriving (Eq, Show)
data DistrExpr = DistrExpr DistrName [Expr]
  deriving (Eq, Show)
  -- unlike StanAST.DistrExpr this has no truncation Bounds
data Decl t = Decl{ declVar :: VarName, declType :: t }
  deriving (Eq, Show)
  -- t is (), TypeS, or TypeSB
type ParmDecls = [Decl TypeSB]

pattern IType :: TypeS
pattern IType = TypeS IntType []

pattern RType :: TypeS
pattern RType = TypeS RealType []

pattern RTypeB :: Bounds -> TypeSB
pattern RTypeB bnds = TypeSB RealType [] bnds

pattern VecType :: Expr -> TypeS
pattern VecType n = TypeS RealType [n]

pattern MatType :: Expr -> Expr -> TypeS
pattern MatType m n = TypeS RealType [m, n]

pattern RArrType :: [Expr] -> TypeS
pattern RArrType shape = TypeS RealType shape

--- vars_in ---

instance ContainsVars TypeS where
  vars_in (TypeS _ sh) = vars_in sh

instance ContainsVars TypeSB where
  vars_in (TypeSB _ sh bnds) = vars_in (sh, bnds)

instance ContainsVars t => ContainsVars (Decl t) where
  vars_in (Decl v t) = Set.insert v $ vars_in t

instance ContainsVars DistrExpr where
  vars_in (DistrExpr _ args) = vars_in args
