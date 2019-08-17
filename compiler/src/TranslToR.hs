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
module TranslToR (
  ssmCreationFct, ssmCreationFct1, translateExprToR, translateSimpleExprToR
) where

import Control.Exception (assert)
import Data.List (genericLength)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Tuple.Extra (first)
import Numeric.Natural

import Misc
import AST
import RAST

type Nat = Natural

ssmCreationFct :: ExprT -> [VarName] -> [(VarName, Nat)] -> RFctDef
ssmCreationFct = ssmCreationFct1 . translateExprToR . checkIsTSD where
  checkIsTSD e = assert (hasType TSDType e) $ e

ssmCreationFct1 :: ([RStmt], RExpr) -> [VarName] -> [(VarName, Nat)] -> RFctDef
-- In the third argument (v, n) means an n-dimensional value named v
ssmCreationFct1 (stmts, retVal) known_vars inferred_vars =
  RFctDef { rfctParms = fctParms
          , rfctStmts = get_known_vars ++ get_inferred_vars ++ stmts
          , rfctReturn = retVal
          }
  where
    fctParms = ["data", "posterior_draws", "i"]
    get_known_vars = map extract_known_var known_vars
    get_inferred_vars = map extract_inferred_var inferred_vars

    extract_known_var :: VarName -> RStmt
    extract_known_var v = RLet v $ RField (RVar "data") (v ++ "_")

    extract_inferred_var :: (VarName, Nat) -> RStmt
    extract_inferred_var (v, n) =
      RLet v $
        RIndex True (RField (RVar "posterior_draws") v) $
        Just (RVar "i") : replicate (fromIntegral n) Nothing

isSimpleType :: Type -> Bool
isSimpleType typ =
  not (isDistr typ) && not (isTS typ)

translateExprToR :: ExprT -> ([RStmt], RExpr)
translateExprToR (Let _ v edef ebody) =
  first ((RLet v (translateSimpleExprToR edef)) :) $ translateExprToR ebody
translateExprToR (Apply TSDType "ssm" args@[aZ, aH, aT, aQ, aa0, aP0])
  | hasType RType aH && all (hasType VecType) [aZ, aa0] &&
    all (hasType MatType) [aT, aQ, aP0]
  = ([], retVal)
  where
    retVal = RApply' "dlm::dlm" []
             [ ("FF", RApply' "matrix" [aZ'] [("nrow", rlitI 1)])
             , ("V", RApply "as.matrix" [aH'])
             , ("GG", aT')
             , ("W", aQ')
             , ("m0", aa0')
             , ("C0", aP0')
             ]
    [aZ', aH', aT', aQ', aa0', aP0'] = map translateSimpleExprToR args
translateExprToR _ = error "Invalid argument to translateExprToR"

translateSimpleExprToR :: ExprT -> RExpr
translateSimpleExprToR = transl where
  transl (Var _ v) = RVar v
  transl (Lit _ x) = RLit x
  transl (Apply _ fctName args) =
    case Map.lookup fctName translMap of
      Just f -> f (map typeOf args) (map transl args)
      Nothing -> error "Unknown function in translateSimpleExprToR"
  transl _ = error "Invalid argument to translateSimpleExprToR"

translMap :: Map FctName ([Type] -> [RExpr] -> RExpr)
translMap = Map.fromList
  [ "*" .->
    checkTypes [[IType, IType], [RType, RType], [RType, VecType],
                [VecType, RType], [RType, MatType], [MatType, RType],
                [RType, RArrType 3], [RArrType 3, RType]] $
    RApply "*"

  , "/" .->
    checkTypes [[RType, RType], [RType, VecType], [VecType, RType],
                [RType, MatType], [MatType, RType]] $
    RApply "/"

  , "+" .->
    checkTypes [[IType, IType], [RType, RType], [RType, VecType],
                [VecType, RType], [RType, MatType], [MatType, RType]] $
    RApply "+"

  , "-" .->
    checkTypes [[IType, IType], [RType, RType], [RType, VecType],
                [VecType, RType], [RType, MatType], [MatType, RType]] $
    RApply "-"

  , "^" .->
    checkTypes [[RType, IType], [RType,RType]] $ RApply "^"

  , "[]" .-> bracketTranslFct

  , "{}" .-> arrayTranslFct

  , "blocks4" .-> blocks4TranslFct

  , "diag" .-> diagTranslFct

  , "div" .-> checkTypes [[IType, IType]] $ RApply "%/%"
  -- TODO: R rounds downwards, Stan (and C++) rounds towards zero. How
  -- do we resolve this?

  , "exponential_mt_rate" .->
    checkTypes [[RType]] $ RApply "chronikis::exponential_mt_rate"

  , "i2r" .-> checkTypes [[IType]] $ RApply "as.numeric"

  , "log" .-> checkTypesUnaryReal $ RApply "log"

  , "mat11" .-> checkTypes [[RType]] $ RApply "as.matrix"

  , "mat22" .->
    checkTypes [[RType, RType, RType, RType]] $
    \[a,b,c,d]-> RApply' "matrix" [RApply "c" [a,c,b,d]] [("nrow", rlitI 2)]

  , "negate" .->
    checkTypes [[RType], [VecType], [MatType], [IType]] $ RApply "unary-"

  , "sqrt" .-> checkTypesUnaryReal $ RApply "sqrt"

  , "square" .->
    checkTypesUnaryReal $ \[x]-> RApply "^" [x, rlitI 2]

  , "to_matrix" .->
    checkTypes [[VecType]] $ RApply "as.matrix"

  , "transp" .->
    checkTypes [[MatType]] $ RApply "t"

  , "vec" .-> vecTranslFct

  , "vec0" .->
    checkTypes [[IType]] $ RApply "rep.int" . (rlitR 0.0 :)    
  ]

-- TODO: ensure consistency with Env.bracketSigs
bracketTranslFct :: [Type] -> [RExpr] -> RExpr
bracketTranslFct (t0@Type{numDim=n} : idxTypes) (a0 : idxArgs)
  | isSimpleType t0 && all (== IType) idxTypes && n > 0 && numIdxs <= n
  = RIndex True a0 $ map Just idxArgs ++ replicate resDim Nothing
  where
    numIdxs = genericLength idxArgs
    resDim = fromIntegral $ n - numIdxs
bracketTranslFct _ _
  = error "Unexpected argument types for []"

-- TODO: ensure consistency with Env.arraySigs
arrayTranslFct :: [Type] -> [RExpr] -> RExpr
arrayTranslFct (typ1 : typs') args
  | all (== typ1) typs' && isSimpleType typ1 && numDim typ1 >= 2
  = RApply' "abind::abind" args [("along", rlitI 0)]
arrayTranslFct _ _
  = error "Unexpected argument types for {}"

blocks4TranslFct :: [Type] -> [RExpr] -> RExpr
blocks4TranslFct [MatType, MatType, MatType, MatType] [a1,a2,a3,a4] =
  RApply "rbind" [RApply "cbind" [a1, a2],
                  RApply "cbind" [a3, a4]]
blocks4TranslFct [RType, VecType, VecType, MatType] [a1,a2,a3,a4] =
  RApply "rbind" [RApply "cbind" [a1, RApply "t" [a2]],
                  RApply "cbind" [a3, a4]]
blocks4TranslFct [MatType, VecType, VecType, RType] [a1,a2,a3,a4] =
  RApply "rbind" [RApply "cbind" [a1, a2],
                  RApply "cbind" [RApply "t" [a3], a4]]
blocks4TranslFct _ _ =
  error "Unexpected argument types for blocks4"

diagTranslFct :: [Type] -> [RExpr] -> RExpr
diagTranslFct [RArrType 3] = RApply "chronikis::bdiag_from_arr3"
diagTranslFct [VecType] = RApply "chronikis::diagv"
diagTranslFct types
  | all (== MatType) types = RApply "dlm::bdiag"
  | otherwise              = error "Unexpected argument types for diag"

vecTranslFct :: [Type] -> [RExpr] -> RExpr
vecTranslFct argTypes
  | all (== RType) argTypes   =
    \args-> if null args then RApply "numeric" [rlitI 0] else RApply "c" args
  | all (== VecType) argTypes = RApply "c"
  | otherwise                 = error "Unexpected argument types for vec"

checkTypes :: [[Type]] -> a -> [Type] -> a
checkTypes allowed x actual
  | actual `elem` allowed = x
  | otherwise             = error "Unexpected argument types"

checkTypesUnaryReal :: a -> [Type] -> a
checkTypesUnaryReal f [typ]
  | isSimpleType typ = f
checkTypesUnaryReal _ _
  = error "Unexpected argument type(s) for unary real op."
