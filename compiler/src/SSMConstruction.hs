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
{-# LANGUAGE TupleSections #-}
module SSMConstruction
( SSMConstruction(..), stripDraws, toSSMCtor
) where

import Control.Exception
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tuple.Extra (second)

import Misc
import AST

data SSMConstruction = SSMConstruction
  { ssmExpr :: ExprT
  , knownVars :: [VarName]
  , inferredVars :: [(VarName, Natural)]
    -- (v, n) means an n-dimensional value named v
  }
  deriving (Eq, Show)

toSSMCtor :: FctDefT -> SSMConstruction
toSSMCtor FctDef{defParms = parms, defBody = body} =
  assert (hasType TSDType body) $
  assert ok_known_vars $
  assert ok_inferred_vars $
  SSMConstruction body' known_vars_list inferred_vars_list
  where
    ok_known_vars = known_vars `Set.isSubsetOf` parmVars
    known_vars_list = Set.toList known_vars
    known_vars = Map.keysSet $ Map.filter (not . snd) used_vars
    
    ok_inferred_vars = inferred_vars {-Set.-}`disjoint` parmVars
    inferred_vars_list = Map.assocs inferred_vars_dims
    inferred_vars = Map.keysSet inferred_vars_dims
    inferred_vars_dims = Map.map (numDim . fst) $ Map.filter snd used_vars
    
    parmVars = Set.fromList $ map fst parms
    used_vars :: Map VarName (Type, Bool)
    (used_vars, body') = stripDraws body

stripDraws :: ExprT -> (Map VarName (Type, Bool), ExprT)
stripDraws e@(Apply TSDType "ssm" args) =
  (used_vars, e)
  where
    used_vars = markF $ Map.unionsWith combEq $ map usedVars args
stripDraws (Let t v edef ebody) =
  case Map.lookup v used of
    Just (typ, isDraw) ->
      assert (typ == typeOf edef) $
      assert (not isDraw) $
      (used', Let t v edef ebody')
    Nothing ->
      (used, ebody')
  where
    used' = Map.unionWith combEq (markF $ usedVars edef) (Map.delete v used)
    (used, ebody') = stripDraws ebody
stripDraws (Draw _ v edistr ebody) =
  case Map.lookup v used of
    Just (typ, isDraw) ->
      assert (typ{isDistr = True} == typeOf edistr) $
      assert (not isDraw) $
      (Map.adjust (second $ const True) v used, body')
    Nothing ->
      (used, body')
  where
    (used, body') = stripDraws ebody
stripDraws e =
  error $ "stripDraws argument must have unrollable form:\n" ++ show e

combEq :: Eq a => a -> a -> a
combEq x y
  | x == y    = x
  | otherwise = error "Inconsistent values"

markF :: Map a b -> Map a (b, Bool)
markF = Map.map ( , False)

usedVars :: ExprT -> Map VarName Type
usedVars (Var typ v) =
  Map.singleton v typ
usedVars (Lit _ _) =
  Map.empty
usedVars (Apply _ _ args) =
  Map.unionsWith combEq $ map usedVars args
usedVars _ =
  error "usedVars argument must be a basic expression"
 
-- TODO: reimplement using Set.disjoint when containers 0.5.11 becomes
-- available.

disjoint :: Ord a => Set.Set a -> Set.Set a -> Bool
disjoint x y = Set.null $ x `Set.intersection` y
