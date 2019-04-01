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
module Checks (checkProgram) where

import Control.Monad (forM)
import Control.Monad.State.Strict (evalState, get, put, State)
import Unique (repeated)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf (printf)

import AST
import Misc

mainFctName :: String
mainFctName = "main"

checkProgram :: Set FctName -> [ FctDefT ] -> [ ErrorMsg ]
checkProgram fnames defs =
  noMain ++ badMainType ++ badMainParms ++ multiFctDefs ++ multiParamName ++
  redefinedFunctions ++ typeErrors ++ shapeDependencyErrors ++ multiVarDefs
 where
  noMain =
    whenl (null mainDefs) (printf "No function '%s' defined" mainFctName)
  badMainType =
    whenl (any ((/= TSDType) . defRetType) mainDefs) $
    printf "Function '%s' return type must be real$~" mainFctName
  badMainParms =
    let parmTypes = concatMap (map snd . defParms) mainDefs
    in
      whenl (any (isDistr . tsbBase) parmTypes)
      (printf "Function '%s' cannot have a distribution parameter" mainFctName)
  multiFctDefs =
    map ("Multiple definitions for function: " ++) $ repeated (map defName defs)
  multiParamName =
    concatMap multip defs where
    multip FctDef{ defName=name, defParms=parms } =
      let pfx = "Function '" ++ name ++
                "' has multiple parameters with the same name: "
      in  map (pfx ++) $ repeated (map fst parms)
  redefinedFunctions =
    map ("Redefined function: " ++) $
    filter (flip Set.member fnames) $
    map defName defs
  mainDefs =
    filter ((== mainFctName) . defName) defs
  typeErrors =
    concatMap checkFctDefType defs
  shapeDependencyErrors = [] -- TODO: figure out how to check these
  multiVarDefs =
    concatMap checkMultiVarDef defs

-- Some of this is already checked in typedFctDef, but here we have a more
-- strict requirement: a variable cannot be defined multiple times even if
-- they are in different lexical scopes within the same function.
checkMultiVarDef :: FctDefT -> [ ErrorMsg ]
checkMultiVarDef FctDef{defName = fname, defParms = parms, defBody = body} =
  map errmsg $ Set.elems $ evalState (cmv body) parmNames
  where
    errmsg v = printf "Function %s: Variable %s multiply defined" fname v
    parmNames = Set.fromList $ map fst parms
    cmv :: ExprT -> State (Set VarName) (Set VarName)
    cmv (Var _ _) =
      return Set.empty
    cmv (Lit _ _) =
      return Set.empty
    cmv (Let _ v edef ebody) = cmvdef v edef ebody
    cmv (Draw _ v edistr ebody) = cmvdef v edistr ebody
    cmv (Apply _ _ args) = Set.unions <$> forM args cmv
    cmvdef v e1 e2 = do
      vars <- get
      put $ Set.insert v vars
      let mdvars0 = if v `elem` vars then Set.singleton v else Set.empty
      mdvars1 <- cmv e1
      mdvars2 <- cmv e2
      return $ mdvars0 `Set.union` mdvars1 `Set.union` mdvars2

checkFctDefType :: FctDefT -> [ErrorMsg]
checkFctDefType FctDef{ defName = name, defRetType = retTyp, defBody = body }
  =
  whenl (bodyTyp /= retTyp) $
    printf "Function %s: body must have type %s, has type %s instead"
           name (show retTyp) (show bodyTyp)
  where
    bodyTyp = typeOf body

whenl :: Bool -> a -> [a]
whenl b x = if b then [x] else []
