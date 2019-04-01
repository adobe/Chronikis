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
module AST
  ( bounds, Bounds(..), BoundsT, drawT, ElemType(..), Expr(..), Expr0, ExprT, FctDef(..), FctDef0, FctDefT, FctTypeEnv, fromET, hasType, hibound, inferredVars, isLit, letT, litI, litI0, litR, litR0, lobound, MaybeType(..), Natural, nobounds, pattern IType, pattern MatType, pattern RArrType, pattern RDistrType, pattern RType, pattern TSDType, pattern VecType, Signatures(..), Type(..), typedExpr, typedFctDef, typeSB, TypeSB(..), TypeSB0, TypeSBT, typeOf, unrollVarDefs, VarDef(..), VarTypeEnv, (<>)
  )
where

import Control.Applicative ((<|>), empty)
import Control.Exception (assert)
import Control.Monad (when)
import qualified Data.Bifunctor as Bifunctor
import Data.List (genericLength)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Semigroup (Semigroup, (<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (first)
import Numeric.Natural (Natural)
import Text.Printf (printf)

import Misc
import NameGen (ContainsVars(..), Str(..))
import Number

---- Basic ----

class Basic a where
  isBasic :: a -> Bool

instance Basic a => Basic (Maybe a) where
  isBasic Nothing = True
  isBasic (Just x) = isBasic x

instance Basic () where
  isBasic _ = True

---- Type ----

data ElemType = IntType | RealType
  deriving (Eq, Ord, Show)

data Type = Type
  { elemType :: ElemType
  , numDim :: Natural
  , isTS :: Bool
  , isDistr :: Bool
  }
  deriving (Eq, Ord)

instance Show Type where
  show t = et ++ nd ++ ts ++ d
    where et = case elemType t of
                 IntType -> "int"
                 RealType -> "real"
          ts = opt (isTS t) '$'
          d  = opt (isDistr t) '~'
          opt b = replicate (fromEnum b)
          numd = numDim t
          nd = replicate (fromIntegral numd) '@'
  -- Originally "real@@@" was "real[3]" (and so on), but this did not
  -- play well with Text.Pretty.Simple, which is very useful for printing
  -- out Haskell data structures.

instance Basic Type where
  isBasic typ = not (isDistr typ)

fromET :: ElemType -> Type
fromET et = Type { elemType = et, numDim = 0, isTS = False, isDistr = False }

pattern IType :: Type
pattern IType = Type{ elemType = IntType, numDim = 0
                    , isTS = False, isDistr = False }

pattern RType :: Type
pattern RType = Type{ elemType = RealType, numDim = 0
                    , isTS = False, isDistr = False }

pattern VecType :: Type
pattern VecType = Type { elemType = RealType, numDim = 1
                       , isTS = False, isDistr = False }

pattern MatType :: Type
pattern MatType = Type { elemType = RealType, numDim = 2
                       , isTS = False, isDistr = False }

pattern RArrType :: Natural -> Type
pattern RArrType n = Type { elemType = RealType, numDim = n
                          , isTS = False, isDistr = False }

pattern RDistrType :: Type
pattern RDistrType = Type { elemType = RealType, numDim = 0
                          , isTS = False, isDistr = True }

pattern TSDType :: Type
pattern TSDType = Type{ elemType = RealType, numDim = 0
                      , isTS = True, isDistr = True }

---- Expr ----

data Expr a
  = Var a VarName
  | Lit a Number
  | Let a VarName (Expr a) (Expr a)
  | Draw a VarName (Expr a) (Expr a)
  | Apply a FctName [Expr a]
  deriving (Eq, Show)

instance ContainsVars (Expr a) where
  vars_in (Var _ v) = Set.singleton v
  vars_in (Lit _ _) = Set.empty
  vars_in (Let _ v edef ebody) =
    Set.insert v $ vars_in edef `Set.union` vars_in ebody
  vars_in (Draw _ v edistr ebody) =
    Set.insert v $ vars_in edistr `Set.union` vars_in ebody
  vars_in (Apply _ _ args) = vars_in args

instance Basic a => Basic (Expr a) where
  isBasic (Var x _) = isBasic x
  isBasic (Lit x _) = isBasic x
  isBasic (Apply x _ args) = isBasic x && all isBasic args
  isBasic _ = False

class MaybeType a where
  clearDistr :: a -> a

instance MaybeType () where
  clearDistr = id

instance MaybeType Type where
  clearDistr typ = typ{isDistr = False}

data VarDef a = LetDef a VarName (Expr a) | DrawDef a VarName (Expr a)
  deriving (Eq, Show)

unrollVarDefs :: MaybeType a => Expr a -> ([VarDef a], Expr a)
unrollVarDefs (Let _ v edef ebody) =
  first ((LetDef (typeOf edef) v edef) :) $ unrollVarDefs ebody
unrollVarDefs (Draw _ v edistr ebody) =
  first ((DrawDef typ v edistr) :) $ unrollVarDefs ebody
  where
    typ = clearDistr $ typeOf edistr
unrollVarDefs e = ([], e)

typeOf :: Expr a -> a
typeOf (Var typ _) = typ
typeOf (Lit typ _) = typ
typeOf (Let typ _ _ _) = typ
typeOf (Draw typ _ _ _) = typ
typeOf (Apply typ _ _) = typ

isLit :: Expr a -> Bool
isLit Lit{} = True
isLit _     = False

inferredVars :: Expr a -> Set VarName
inferredVars (Draw _ v edistr ebody) =
  Set.insert v $ inferredVars edistr `Set.union` inferredVars ebody
inferredVars (Let _ _ edef ebody) =
  inferredVars edef `Set.union` inferredVars ebody
inferredVars (Apply _ _ args) =
  Set.unions $ map inferredVars args
inferredVars (Var _ _) =
  Set.empty
inferredVars (Lit _ _) =
  Set.empty

---- Expr0 ----

type Expr0 = Expr ()

litR0 :: Scientific -> Expr0
litR0 n = Lit () (RealVal n)

litI0 :: Integer -> Expr0
litI0 n = Lit () (IntVal n)

---- ExprT ----

type ExprT = Expr Type

hasType :: Type -> ExprT -> Bool
hasType typ = (== typ) . typeOf

type VarTypeEnv = Map VarName Type

newtype Signatures = Sigs ([Type] -> Maybe Type)

instance Semigroup Signatures where
  Sigs x <> Sigs y = Sigs $ \a -> x a <|> y a

instance Monoid Signatures where
  mempty = Sigs $ const empty
  mappend = (<>)

type FctTypeEnv = Map FctName Signatures

retType :: FctTypeEnv -> FctName -> [ExprT] -> Either ErrorMsg Type
retType env fctName args =
  let argTypes = map typeOf args in
  case Map.lookup fctName env of
    Nothing -> Left $ "Unknown function: " ++ fctName
    Just (Sigs f) ->
      case f argTypes of
        Just typ -> Right typ
        Nothing -> Left $ printf "Cannot apply %s to arguments of types %s"
                          fctName (commasep $ map show argTypes)
        
litI :: Integer -> ExprT
litI = Lit IType . IntVal

litR :: Scientific -> ExprT
litR = Lit RType . RealVal

letT :: VarName -> ExprT -> ExprT -> ExprT
letT v edef ebody =
  Let (typeOf ebody) v edef ebody

drawT :: VarName -> ExprT -> ExprT -> ExprT
drawT v edistr ebody =
  if (not $ isDistr $ typeOf edistr)
  then logicError $ printf "RHS of draw %s ~ ... must be a distribution" v
  else Draw (typeOf ebody) v edistr ebody

typedExpr :: (VarTypeEnv, FctTypeEnv) -> Expr0 -> Either ErrorMsg ExprT
typedExpr (venv, fenv) = f where
  f (Var _ v) = do
    typ <- maybeToEither (Map.lookup v venv) $ "Unknown variable: " ++ v
    return $ Var typ v
    
  f (Lit _ x@(IntVal _)) = return $ Lit IType x

  f (Lit _ x@(RealVal _)) = return $ Lit RType x
  
  f (Let _ v e1 e2) = do
    errIfDefined v
    te1 <- f e1
    let t1 = typeOf te1
    let venv' = Map.insert v t1 venv
    te2 <- typedExpr (venv', fenv) e2
    return $ Let (typeOf te2) v te1 te2
    
  f (Draw _ v e1 e2) = do
    errIfDefined v
    let errmsg s = "In " ++ v ++ " ~ _e1; _e2: " ++ s
    te1 <- f e1
    let t1 = typeOf te1
    when (not $ isDistr t1) $
      Left $ errmsg "_e1 must be a distribution"
    let venv' = Map.insert v t1{isDistr = False} venv
    te2 <- typedExpr (venv', fenv) e2
    let t2 = typeOf te2
    when (not $ isDistr t2) $
      Left $ errmsg "_e2 must be a distribution"
    return $ Draw t2 v te1 te2

  f (Apply _ fct args) = do
    typedArgs <- mapM f args
    typ <- retType fenv fct typedArgs
    return $ Apply typ fct typedArgs

  errIfDefined v =
    when (Map.member v venv) $ Left $ "Redefined variable: " ++ v

  maybeToEither mx e = case mx of
                         Just x -> Right x
                         Nothing -> Left e

---- Bounds ----

data Bounds a = Bounds{ lobnd, hibnd :: Maybe (Expr a) }
  deriving (Eq, Show)

type BoundsT = Bounds Type

instance Basic a => Basic (Bounds a) where
  isBasic b = isBasic (lobnd b) && isBasic (hibnd b)

instance ContainsVars (Bounds a) where
  vars_in (Bounds x y) = vars_in x `Set.union` vars_in y

nobounds :: Bounds a
nobounds = Bounds Nothing Nothing

lobound, hibound :: Basic a => Expr a -> Bounds a
lobound e =
  assert (isBasic e) $
  Bounds (Just e) Nothing
hibound e =
  assert (isBasic e) $
  Bounds Nothing (Just e)

bounds :: Basic a => Expr a -> Expr a -> Bounds a
bounds lo hi =
  assert (isBasic lo && isBasic hi) $
  Bounds (Just lo) (Just hi)

typedBounds :: (VarTypeEnv, FctTypeEnv) -> Bounds () -> Either ErrorMsg BoundsT
typedBounds vfenvs (Bounds lo hi) =
  Bounds <$> (te lo) <*> (te hi)
  where
    te = traverse (typedExpr vfenvs)

---- FctDef ----

data TypeSB a =
  TypeSB { tsbBase :: Type, tsbShape :: [Expr a], tsbBounds :: (Bounds a) }
  deriving (Eq, Show)

type TypeSB0 = TypeSB ()
type TypeSBT = TypeSB Type

instance ContainsVars (TypeSB a) where
  vars_in TypeSB{tsbShape = shape, tsbBounds = bnds} =
    vars_in shape `Set.union` vars_in bnds

instance Basic a => Basic (TypeSB a) where
  isBasic (TypeSB typ shape bnds) =
    isBasic typ && (all isBasic shape) && isBasic bnds

typeSB :: Basic a => Type -> [Expr a] -> (Bounds a) -> TypeSB a
typeSB typ shape bnds =
  assert (all isBasic shape) $
  assert (numDim typ == genericLength shape)
  assert (isBasic bnds) $
  TypeSB typ shape bnds

typedTypeSB :: (VarTypeEnv, FctTypeEnv) -> TypeSB0 -> Either ErrorMsg TypeSBT
typedTypeSB vfenvs (TypeSB typ shape bnds) =
  do
    shape' <- Bifunctor.first ("In shape: " ++) $
              mapM intTypedExpr shape
    bnds' <- Bifunctor.first ("In bounds: " ++) $ sameTypedBounds
    return $ TypeSB typ shape' bnds'
  where
    intTypedExpr e0 = do
      e <- typedExpr vfenvs e0
      if typeOf e == IType
        then Right e
        else Left "Sizes must have type int"
    sameTypedBounds = do
      b'@(Bounds lo hi) <- typedBounds vfenvs bnds
      if all (hasType btyp) (catMaybes [lo, hi])
        then Right b'
        else Left $ "Must have type " ++ show btyp
    btyp = fromET $ elemType typ

type Parms a = [(VarName, TypeSB a)]

data FctDef a =
  FctDef
  { defName :: FctName
  , defParms :: Parms a
  , defRetType :: Type
  , defBody :: Expr a
  }
  deriving (Eq, Show)

instance ContainsVars (FctDef a) where
  vars_in FctDef{defParms = parms, defBody = body} =
    vars_in (map (first Str) parms) `Set.union` vars_in body

type FctDef0 = FctDef ()
type FctDefT = FctDef Type

typedFctDef :: (VarTypeEnv, FctTypeEnv) -> FctDef0 -> Either ErrorMsg FctDefT
typedFctDef (venv, fenv) fd =
  Bifunctor.first ((printf "In function %s: " (defName fd)) ++) $
  do
    (parms', venv') <- typedParms (defParms fd) venv
    body' <- typedExpr (venv', fenv) (defBody fd)
    return $ FctDef { defName = defName fd
                    , defParms = parms'
                    , defRetType = defRetType fd
                    , defBody = body'
                    }
  where
    typedParms :: Parms () -> VarTypeEnv ->
                  Either ErrorMsg (Parms Type, VarTypeEnv)
    typedParms [] ve = return ([], ve)
    typedParms ((v, typ) : rest) ve = do
      typ' <- Bifunctor.first ((printf "In type of parameter %s: " v) ++) $
              typedTypeSB (ve, fenv) typ
      first ((v, typ') :) <$> typedParms rest (Map.insert v (tsbBase typ) ve)
