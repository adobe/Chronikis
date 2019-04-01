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
module InferShapesBoundsImpl
where

import Control.Applicative ((<|>), empty)
import Control.Exception (assert)
import Control.Monad.Reader
import Data.List (genericLength)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
--import Data.Semigroup (Semigroup, (<>))
import Data.Set (Set)
import qualified Data.Set as Set
--import Debug.Trace (trace)
import Text.Printf (printf)

import BasicExpr
import BasicModel
import Misc
import AST (ElemType(..))
import qualified AST as A
import Env (baseFctShEnv, baseDistrShEnv, ShapeSigFct)
import SSM
import qualified Translate as T

newtype Disambig = Disambig ([A.Type] -> Maybe FctName)

instance Semigroup Disambig where
  Disambig x <> Disambig y =
    Disambig $ \a -> x a <|> y a

instance Monoid Disambig where
  mempty = Disambig $ const empty

disamb1 :: [A.Type] -> FctName -> Disambig
disamb1 typs fname = Disambig $ \typs' -> justWhen (typs' == typs) fname

disambn :: A.Type -> FctName -> Disambig
disambn typ fname = Disambig $ \typs' -> justWhen (all (== typ) typs') fname

disamb_ :: FctName -> Disambig
disamb_ = Disambig . const . Just

data TTMEnv =
     TTMEnv { fctShEnv :: Map FctName (ShapeSigFct TypeS)
            , distrShEnv :: Map FctName (ShapeSigFct TypeSB)
            , disambigEnv :: Map FctName Disambig
            }
type REnv = Reader TTMEnv

toTranslateModel :: A.FctDefT -> T.Model
toTranslateModel =
  toTranslateModel1 $ TTMEnv baseFctShEnv baseDistrShEnv dsMap

toTranslateModel1 :: TTMEnv -> A.FctDefT -> T.Model
toTranslateModel1 env fd = assert ok $ tmodel
  where
  tmodel = flip runReader env $ toModel fd >>= inferShapesBounds
  ok = (vtd == vtm)
  vtd = varTypesDefT fd
  vtm = varTypesModel tmodel

data Model = Model ParmDecls ModelBody
  deriving (Eq, Show)
data ModelBody
  = Let VarName Expr ModelBody
  | Draw VarName DistrExpr ModelBody
  | TSDistr SSM
  deriving (Eq, Show)

--- Disambiguation environment ---

--        _  -> logicError "Bad arg types for block4")
dsMap :: Map FctName Disambig
dsMap = Map.fromList
  [ ("*" .->
       disamb1 [A.RArrType 3, A.RType] "*_arr3_real" <>
       disamb1 [A.RType, A.RArrType 3] "*_real_arr3" <>
       disamb_ "*")
    
  , ("/" .->
       disamb1 [A.RType, A.VecType] "/_real_vec" <>
       disamb1 [A.RType, A.MatType] "/_real_mat" <>
       disamb_ "/")
    
  , ("blocks4" .->
       disamb1 [A.RType, A.VecType, A.VecType, A.MatType] "blocks4_rvvm" <>
       disamb1 [A.MatType, A.VecType, A.VecType, A.RType] "blocks4_mvvr" <>
       disamb1 [A.MatType, A.MatType, A.MatType, A.MatType] "blocks4_mmmm")
    
  , ("diag" .->
       disamb1 [A.RArrType 3] "diag_arr3" <>
       disamb1 [A.VecType]    "diag_vec" <>
       disamb1 [A.RType]      "mat11" <>
       disambn A.MatType      "diag_mats")
    
  , ("vec" .->
       disambn A.RType   "vec_reals" <>
       disambn A.VecType "vec_vecs")
  ]

--- Getting variable types ---

varTypesDefT :: A.FctDefT -> Map VarName A.Type
varTypesDefT fd = Map.fromList $ vtb $ A.defBody fd
  where
    vtb (A.Let _ v e rest) = (v, A.typeOf e) : vtb rest
    vtb (A.Draw _ v d rest) =
      assert (A.isDistr typ) $
      (v, typ{A.isDistr = False}) : vtb rest
      where
        typ = A.typeOf d
    vtb _ = []

varTypesModel :: T.Model -> Map VarName A.Type
varTypesModel (T.Model _ body) =
  Map.fromList $ vtb body
  where
    vtb (T.Let (Decl v t) _ rest) = (v, toType t) : vtb rest
    vtb (T.Draw (Decl v t) _ rest) = (v, toType $ toTypeS t) : vtb rest
    vtb _ = []
    toTypeS (TypeSB et sh _) = TypeS et sh
    toType (TypeS et sh) =
      A.Type{ A.elemType = et, A.numDim = genericLength sh
            , A.isTS = False, A.isDistr = False }

---- Converting A.FctDefT to Model ----

toModel :: A.FctDefT -> REnv Model
toModel A.FctDef{A.defParms = parms, A.defBody = body} = do
  parms' <- mapM toDecl parms
  Model parms' <$> toModelBody body

toDecl :: (VarName, A.TypeSBT) -> REnv (Decl TypeSB)
toDecl (varName, typ) = do
  typ' <- toTypeSB typ
  return $ Decl varName typ'

toTypeSB :: A.TypeSBT -> REnv TypeSB
toTypeSB typ = 
  do
    sh <- mapM toBasicExpr $ A.tsbShape typ
    bnds <- toBasicBounds $ A.tsbBounds typ
    let ok = isDataType bt && fromIntegral (length sh) == A.numDim bt
    unless ok $
      error $ "In toTypeSB: errror convertying type " ++ show typ
    return $ TypeSB et sh bnds
  where
    bt = A.tsbBase typ    
    et = A.elemType bt

isDataType :: A.Type -> Bool
isDataType typ = not (A.isDistr typ) && not (A.isTS typ)
                 -- A future release will allow A.isTS typ

toBasicBounds :: A.BoundsT -> REnv Bounds
toBasicBounds (A.Bounds lo hi) = do
  lo' <- traverse toBasicExpr lo
  hi' <- traverse toBasicExpr hi
  return $ Bounds lo' hi'

toBasicExpr :: A.ExprT -> REnv Expr
toBasicExpr (A.Var _ varName) =
  return $ Var varName
toBasicExpr (A.Lit _ x) =
  return $ Lit x
toBasicExpr (A.Apply _ fctName args) = do
  fctName1 <- disambiguateSigs fctName (map A.typeOf args)
  args' <- mapM toBasicExpr args
  return $ Apply fctName1 args'
toBasicExpr A.Let{} =
  error "toBasicExpr requires an argument without Let"
toBasicExpr A.Draw{} =
  error "toBasicExpr requires an argument without Draw"

toModelBody :: A.ExprT -> REnv ModelBody
toModelBody (A.Let _ varName edef ebody) = do
  edef' <- toBasicExpr edef
  ebody' <- toModelBody ebody
  return $ Let varName edef' ebody'
toModelBody (A.Draw _ varName edistr ebody) = do
  edistr' <- toBasicDistr edistr
  ebody' <- toModelBody ebody
  return $ Draw varName edistr' ebody'
toModelBody (A.Apply _ "ssm" args@[_, _, _, _, _, _]) = do
  [aZ', aH', aT', aQ', aa0', aP0'] <- mapM toBasicExpr args
  return $ TSDistr $
    SSM{ vecZ = aZ'
       , scalH = aH'
       , matT = aT'
       , matQ = aQ'
       , veca0 = aa0'
       , matP0 = aP0'
       }
toModelBody _ = error "toModelBody requires an argument in BasicModel form"

toBasicDistr :: A.ExprT -> REnv DistrExpr
toBasicDistr (A.Apply typ dname args) =
  assert ((not $ A.isTS typ) && A.isDistr typ) $
  DistrExpr dname <$> mapM toBasicExpr args
toBasicDistr e =
  error ("Error converting ExprT to DistrExpr: " ++ show e)

disambiguateSigs :: FctName -> [A.Type] -> REnv FctName
disambiguateSigs fctName argts = do
  disambigMap <- reader disambigEnv
  return $
    case Map.lookup fctName disambigMap of
      Nothing -> fctName
      Just (Disambig dfct) ->
        flip fromMaybe (dfct argts) $
        error $ printf "Bad arg types for %s in disambiguateSig" fctName

---------------------------------------

type VEnv = Map VarName TypeS

inferShapesBounds :: Model -> REnv T.Model
inferShapesBounds model =
  T.Model parms' <$> f (parmsEnv parms') Set.empty body'
  where
    Model parms' body' = model
    f :: VEnv -> Set VarName -> ModelBody -> REnv T.ModelBody
    f venv drawDeps (Let v e body) =
      do
        vtyp <- exprTypeS venv e
        let venv' = Map.insert v vtyp venv
        T.Let (Decl v vtyp) e <$> f venv' drawDeps' body
      where
        drawDeps' = if drawDepExpr drawDeps e
                    then Set.insert v drawDeps
                    else drawDeps
    f venv drawDeps (Draw v de body) =
      do
        vtyp@(TypeSB _ shape _) <- drawTypeSB venv de
        let ok = not $ or $ map (drawDepExpr drawDeps) shape
        () <- unless ok $
                error $ printf "Shape of %s depends on drawn variable(s)" v
        let venv' = Map.insert v (typeSfromSB vtyp) venv
        T.Draw (Decl v vtyp) de <$> f venv' drawDeps' body
      where
        drawDeps' = Set.insert v drawDeps
    f _ _ (TSDistr ssm) =
      return $ T.TSDistr ssm

drawDepExpr :: Set VarName -> Expr -> Bool
drawDepExpr drawDeps = f where
  f (Var v) = v `Set.member` drawDeps
  f (Lit _) = False
  f (Apply _ args) = or $ map f args

parmsEnv :: ParmDecls -> VEnv
parmsEnv decls = Map.fromList $ map f decls where
  f (Decl v t) = (v, typeSfromSB t)

typeSfromSB :: TypeSB -> TypeS
typeSfromSB (TypeSB et shape _) = TypeS et shape

exprTypeS :: VEnv -> Expr -> REnv TypeS
exprTypeS venv = f where
  f (Var v) =
    return $ venv ! v
  f (Lit (IntVal _)) =
    return $ TypeS IntType []
  f (Lit (RealVal _)) =
    return $ TypeS RealType []
  f (Apply fctname args) =
    returnTypeS fctname args =<< mapM f args

returnTypeS :: FctName -> [Expr] -> [TypeS] -> REnv TypeS
returnTypeS fctname args argtyps =
  do
    fshEnv <- reader fctShEnv
    let maybeType = do
          sig <- Map.lookup fctname fshEnv
          rtyp <- sig (args, argtyps)
          Just $ normalizeShape rtyp
    case maybeType of
      Just typ -> return typ
      Nothing -> error $ typeErrMsg ("function " ++ fctname) args argtyps
  where
    normalizeShape (TypeS et shape) = TypeS et $ map normalizeIExpr shape

drawTypeSB :: VEnv -> DistrExpr -> REnv TypeSB
drawTypeSB venv (DistrExpr dname args) =
  do
    dshEnv <- reader distrShEnv
    argts <- mapM (exprTypeS venv) args
    let maybeType = do
          sig <- Map.lookup dname dshEnv
          rtyp0 <- sig (args, argts)
          Just $ normalizeShapeBnds rtyp0
    case maybeType of
      Just typ -> return typ
      Nothing -> error $ typeErrMsg ("distribution " ++ dname) args argts
  where
    normalizeShapeBnds (TypeSB et shape bnds) =
      TypeSB et (map normalizeIExpr shape) (bmap normalizeRExpr bnds)

typeErrMsg :: String -> [Expr] -> [TypeS] -> String
typeErrMsg name args argts = printf errtpl name sargts sargs where
  errtpl = "InferReturnShapesBounds: no signature for %s takes \
           \argument types: %s\n  arguments: %s"
  sargs = commasep $ map show args
  sargts = commasep $ map show argts
      
-- Not sure we actually need these normalize* functions. If we do, I think
-- we'll want to do at least constant folding and flattening nested usages
-- of + and *.

normalizeIExpr :: Expr -> Expr
normalizeIExpr = id

normalizeRExpr :: Expr -> Expr
normalizeRExpr = id
