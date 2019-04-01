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
module TranslateImpl where

import Control.Exception
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (first, second)
import Text.Printf (printf)

import BasicExpr
import BasicModel
import Misc
import NameGen (ContainsVars(..))
import AST (ElemType(..))
import qualified StanAST as S
import SSM
import StanAST (StanAST(..), StanModelBlock(..))

type DeclSB = Decl TypeSB
type DeclS = Decl TypeS

data Model = Model ParmDecls ModelBody
  deriving (Eq, Show)

data ModelBody
  = Let DeclS Expr ModelBody
  | Draw DeclSB DistrExpr ModelBody
  | TSDistr SSM
  deriving (Eq, Show)

---- unroll ----
data VarDef = LetDef DeclS Expr | DrawDef DeclSB DistrExpr
  deriving (Eq, Show)

data UnrolledModel =
  UnrolledModel
  { mdlParms :: [DeclSB]
  , mdlDefs :: [VarDef]
  , mdlDistr :: SSM
  }
  deriving (Show)

unrollModel :: Model -> UnrolledModel
unrollModel (Model pd body) =
  UnrolledModel
  { mdlParms = pd
  , mdlDefs = defs
  , mdlDistr = ssm
  }
  where
    (defs, ssm) = unrollVarDefs body
    unrollVarDefs :: ModelBody -> ([VarDef], SSM)
    unrollVarDefs (Let decl e rest) =
      first ((LetDef decl e) :) $ unrollVarDefs rest
    unrollVarDefs (Draw decl d rest) =
      first ((DrawDef decl d) :) $ unrollVarDefs rest
    unrollVarDefs (TSDistr x) =
      ([], x)

--------

translate :: Set VarName -> Model -> StanAST
translate vars_to_monitor (Model parm_decls body) =
  let parm_names = map declVar parm_decls
      parm_types = map (translateTypeSB . declType) parm_decls
      st_data = zip parm_names parm_types
      (st_xdata, body_dynamic) = xdataDefs body
      (draws, (defs_post_draws, ssm)) = drawDefs $ replace_dmvs body_dynamic
      st_parms = map fst draws
      pvars = map fst st_parms
      me_draws = zip pvars (map snd draws)
      (st_xparms, me_defs) = part_xparm_deps vars_to_monitor defs_post_draws
      ssm' = ssmMap translateExpr ssm
  in StanAST
     { stdata = st_data
     , stxdata = st_xdata
     , stparms = st_parms
     , stxparms = st_xparms
     , stmodel = StanModelBlock
       { medefs = me_defs
       , medraws = me_draws
       , messm = ssm'
       }
     }

translateTypeSB :: TypeSB -> (S.TypeS, Bounds)
translateTypeSB (TypeSB et shape bnds) =
  (vectorize (S.Type et' shape'), bnds')
  where
    bnds' = bmap translateExpr bnds
    shape' = map translateExpr shape
    et' = case et of
            IntType -> S.IntType
            RealType -> S.RealType
    vectorize (S.Type S.RealType [e]) =
      S.Type (S.VectorType e) []
    vectorize (S.Type S.RealType (e1:e2:rest)) =
      S.Type (S.MatrixType e1 e2) rest
    vectorize t =
      t

translateTypeS :: TypeS -> S.TypeS
translateTypeS = fst . translTypeSwithB

translTypeSwithB :: TypeS -> (S.TypeS, Bounds)
translTypeSwithB = translateTypeSB . typeSBfromS

typeSBfromS :: TypeS -> TypeSB
typeSBfromS (TypeS et shape) = TypeSB et shape nobounds

translateExpr :: Expr -> Expr
translateExpr x@(Var _) = x
translateExpr x@(Lit _) = x
translateExpr (Apply fctname args) = construct_expr stan_args
  where
    stan_args = map translateExpr args
    construct_expr =
      case Map.lookup fctname fct_translation_map of
        Nothing -> error $ "Cannot translate function: " ++ fctname
        Just f -> f

translateDistrExpr :: DistrExpr -> S.DistrExpr
translateDistrExpr (DistrExpr dname args) =
  construct_dexpr stan_args
  where
    stan_args = map translateExpr args
    construct_dexpr =
      case Map.lookup dname distr_translation_map of
        Nothing -> error $ "Cannot translate distribution: " ++ dname
        Just f -> f

translateVarDefB :: (Decl TypeS, Expr) -> (S.BoundedTypedVar, Expr)
translateVarDefB ((Decl v typ), e) = ((v, typ'), e') where
  typ' = translTypeSwithB typ
  e' = translateExpr e

translateVarDefU :: ((Decl TypeS), Expr) -> (S.TypedVar, Expr)
translateVarDefU ((Decl v typ), e) = ((v, typ'), e') where
  typ' = translateTypeS typ
  e' = translateExpr e

fct_translation_map :: Map FctName ([Expr] -> Expr)
fct_translation_map = Map.fromList
  [ "*" .-> app_lassoc "*"
  , "*_real_arr3" .-> Apply "scale_matrices"
  , "*_arr3_real" .-> \[a,r]->Apply "scale_matrices" [r,a]
  , "/" .-> Apply "/"
  , "/_real_vec" .-> Apply "./"
  , "/_real_mat" .-> Apply "./"
  , "+" .-> app_lassoc "+"
  , "-" .-> Apply "-"
  , "^" .-> Apply "^"
  , "[]" .-> Apply "[]"
  , "{}" .-> Apply "{}" -- we've guaranteed that args are not scalars or vectors
  , "blocks4_mmmm" .-> Apply "blocks4"
  , "blocks4_rvvm" .->
    \[a,b,c,d]-> Apply "blocks4" [mat11 a, mat1n b, matn1 c, d]
  , "blocks4_mvvr" .->
    \[a,b,c,d]-> Apply "blocks4" [a, matn1 b, mat1n c, mat11 d]
  , "diag_vec" .-> Apply "diag_matrix"
  , "diag_arr3" .-> Apply "block_diag"
  , "diag_mats" .-> applyArr "block_diag" mat00
  -- "diag_sqr" has been transformed away already
  , "div" .-> Apply "/"
  , "exponential_mt_rate" .-> Apply "exponential_mt_rate"
  , "i2r" .-> Apply "+" . (litR 0.0 :)
  , "mat11" .-> mat11 . head
  , "mat22" .-> mat22
  , "negate" .-> Apply "-"
  , "sqrt" .-> Apply "sqrt"
  , "square" .-> Apply "square"
  , "to_matrix" .-> Apply "to_matrix"
  , "transp" .-> Apply "'"
  , "vec_reals" .->
    let f [] = emptyVector
        f [e] = Apply "rep_vector" [e, litI 1]
        f args = app1 "to_vector" $ Apply "{}" args
    in f
  , "vec_vecs" .-> applyArr "vec_append" emptyVector
  , "vec0" .-> Apply "rep_vector" . (litR 0.0 :)
  ]
  where
    app_lassoc fctname = foldl1 (app2 fctname)
    app2 fctname e1 e2 = Apply fctname [e1, e2]
    mat00 = Apply "rep_matrix" [litR 0, litI 0, litI 0]
    mat11 e = Apply "rep_matrix" [e, litI 1, litI 1]
    mat1n = app1 "'" . matn1
    matn1 = app1 "to_matrix"
    mat22 [a11, a12, a21, a22] =                   -- row-major order
      Apply "to_matrix" [arr, litI 2, litI 2]
      where arr = Apply "{}" [a11, a21, a12, a22]  -- column-major order
    mat22 _ = logicError "Wrong number of arguments to mat22"
    applyArr _ dflt [] = dflt
    applyArr fctName _ args = app1 fctName $ Apply "{}" args
    emptyVector = Apply "rep_vector" [litR 0, litI 0]

-- Drawn variable: One defined via Draw (v, typb) depxr.
-- Draw-dependent variable (ddv): One defined via Let (v, typu) expr
--   where either expr or the shape of typu references a drawn variable or
--   another draw-dependent variable.
-- Static variable: One defined via Let (v, typu) expr that is NOT a
--   draw-dependent variable.
-- Draw-mediating varible (dmv): A draw-dependent variable used in the
--  shape or bounds for a drawn variable. (Note that it is an error for
--  the shape of any variable to make use of any but static variables, so
--  a dmv should only be used in the bounds of a drawn variable.)

-- REQUIRE: All vars defined in x are used only after they are defined.
-- PURPOSE: Replace any draw-mediating variable from the bounds
-- of any drawn variable, by substituting in the dmv's definition.
replace_dmvs :: ModelBody -> ModelBody
replace_dmvs = rmv_unused_vars . rdmv m0 s0 where
  m0 = Map.empty
  s0 = Set.empty
  
  rdmv :: Map VarName Expr -> Set VarName -> ModelBody -> ModelBody
  rdmv dd_defs draw_vars = f where
    f (Let (Decl v typu@(TypeS _ shape)) expr rest) =
      assert (not $ or $ map is_dd shape) $
      Let (Decl v typu) expr $
      rdmv dd_defs' draw_vars rest
      where
        dd_defs' = if is_dd expr
                   then Map.insert v (replace_ddvs expr) dd_defs
                   else dd_defs
    f (Draw (Decl v (TypeSB et shape bnds)) dexpr rest) =
      assert (not $ or $ map is_dd shape) $
      Draw (Decl v typb') dexpr $
      rdmv dd_defs draw_vars' rest
      where
        draw_vars' = Set.insert v draw_vars
        typb' = TypeSB et shape bnds'
        bnds' = Bounds (newbnd lo) (newbnd hi)
        Bounds lo hi = bnds
        newbnd = fmap (\e-> if is_dd e then replace_ddvs e else e)
    f x@(TSDistr _) = x
    
    is_dd (Lit _) = False
    is_dd (Var v) = v `Map.member` dd_defs || v `Set.member` draw_vars
    is_dd (Apply _ args) = or $ map is_dd args

    replace_ddvs x@(Lit _) = x
    replace_ddvs x@(Var v) = case Map.lookup v dd_defs of
                               Nothing -> x
                               Just e' -> e'
    replace_ddvs (Apply fctname args) = Apply fctname $ map replace_ddvs args

-- TODO: reimplement using Set.disjoint when containers 0.5.11 becomes
-- available.

disjoint :: Ord a => Set a -> Set a -> Bool
disjoint x y = null $ x `Set.intersection` y

overlaps :: Ord a => Set a -> Set a -> Bool
overlaps x y = not $ {-Set.-}disjoint x y

rmv_unused_vars :: ModelBody -> ModelBody
rmv_unused_vars = snd . ruv where
  ruv :: ModelBody -> (Set VarName, ModelBody) -- fst . ruv gives used vars
  ruv (Let vt@(Decl v typu) edef body) =
    let (used, body') = ruv body in
    if v `Set.notMember` used
    then (used, body')
    else ( used `Set.union` vars_in edef `Set.union` typeU_vars typu
         , Let vt edef body'
         )
  ruv (Draw vt@(Decl v typb) de@(DistrExpr _ args) body) =
    let (used, body') = ruv body in
    if v `Set.notMember` used
    then (used, body')
    else ( used `Set.union` vars_in args `Set.union` typeB_vars typb
         , Draw vt de body'
         )
  ruv x@(TSDistr ssm) =
    (vars_in ssm, x)

part_xparm_deps :: Set VarName -> [(DeclS, Expr)] ->
                   ([(S.BoundedTypedVar, Expr)], [(S.TypedVar, Expr)])
part_xparm_deps xparm_vars defs =
  assert ok $ (transl_xp_defs, transl_other_defs)
  where
    ok = {-Set.-}disjoint shape_vars (def_vars `Set.union` xparm_vars)
    shape_vars = Set.unions $ map (typeU_vars . declType . fst) defs
    def_vars = Set.fromList $ map (declVar . fst) defs
    transl_xp_defs :: [(S.BoundedTypedVar, Expr)]
    transl_xp_defs = map translateVarDefB xp_defs
    transl_other_defs :: [(S.TypedVar, Expr)]
    transl_other_defs = map translateVarDefU other_defs
    xp_defs, other_defs :: [(DeclS, Expr)]
    (xp_defs, other_defs) =
      List.partition ((flip Set.member xp_dependencies) . declVar . fst) defs
    xp_dependencies = foldr combine Set.empty defs
    combine (Decl v _, d) vars =
      if v `Set.member` xparm_vars || v `Set.member` vars
      then Set.insert v $ vars_in d `Set.union` vars
      else vars

typeU_vars :: TypeS -> Set VarName
typeU_vars (TypeS _ shape) = vars_in shape

typeB_vars :: TypeSB -> Set VarName
typeB_vars (TypeSB et shape (Bounds lo hi)) =
  typeU_vars (TypeS et shape) `Set.union`
    bound_vars lo `Set.union` bound_vars hi
  where
    bound_vars Nothing = Set.empty
    bound_vars (Just e) = vars_in e
    
xdataDefs :: ModelBody -> ([(S.BoundedTypedVar, Expr)], ModelBody)
xdataDefs = xddefs Set.empty where
  xddefs varset (Draw vt@(Decl v _) dexpr body) =
    second (Draw vt dexpr) $ xddefs (Set.insert v varset) body
  xddefs _ x@(TSDistr _) =
    ([], x)
  xddefs varset (Let vt@(Decl v (TypeS _ shape)) edef body) =
    if varset `overlaps` vars_in shape
      then error $ printf "Shape of %s must be static" v
    else if varset `overlaps` vars_in edef
      then second (Let vt edef) $ xddefs (Set.insert v varset) body
    else
      first ((translateVarDefB (vt, edef)) :) $ xddefs varset body

drawDefs :: ModelBody -> ([(S.BoundedTypedVar, S.DistrExpr)],
                          ([(DeclS, Expr)], SSM))
drawDefs (Let vt edef body) =
  (second . first) ((vt, edef) :) $ drawDefs body
drawDefs (TSDistr ssm) =
  ([], ([], ssm))
drawDefs (Draw (Decl v typb) dexpr body) =
  first (((v, typ'), dexpr') :) $ drawDefs body
  where
    typ' = translateTypeSB typb
    dexpr' = translateDistrExpr dexpr

distr_translation_map :: Map DistrName ([Expr] -> S.DistrExpr)
distr_translation_map = Map.fromList
  [ "exponential_m" .-> dapp1 "exponential" . Apply "inv"
  -- "exponential_mt" already transformed away
  , "exponential_r" .-> dapp "exponential"
  , "exponential_rt" .->
    \[rate, ub]-> S.DistrExpr "exponential" [rate] (hibound ub)
  , "half_cauchy" .-> dhalf "cauchy"
  , "half_normal" .-> dhalf "normal"
  , "normal" .-> dapp "normal"
  , "uniform" .-> dapp "uniform"
  ]
  where
    dapp dname args = S.DistrExpr dname args nobounds
    dapp1 dname arg = S.DistrExpr dname [arg] nobounds
    dhalf dname [scale] = S.DistrExpr dname [litR 0, scale] lower0
    dhalf dname _ = logicError $ "Wrong number of arguments to dhalf " ++ dname
    lower0 = lobound (litR 0)
