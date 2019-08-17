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
module Env
  ( baseFctEnv, baseFctShEnv, baseDistrShEnv, baseVarEnv, ShapeSigFct
  )
where

import Control.Monad (guard)
import Data.List (genericLength)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)

import qualified BasicExpr as B
import qualified BasicModel as B
import Misc
import AST

baseVarEnv :: VarTypeEnv
baseVarEnv = Map.empty

baseFctEnv :: FctTypeEnv
baseFctEnv = Map.fromList baseFctSigs where
  baseFctSigs = map (\(k,v1,_) -> (k, v1)) baseFctInfo

type ShapeSigFct a = ([B.Expr], [B.TypeS]) -> Maybe a

data ShapeSignatures
  = FShSigs (ShapeSigFct B.TypeS) -- data function signatures
  | DShSigs (ShapeSigFct B.TypeSB) -- distribution signatures
  | NoShSigs

baseFctShEnv :: Map FctName (ShapeSigFct B.TypeS)
baseFctShEnv = Map.fromList $ mapAndFilter fsh baseFctInfo where
  fsh (fctName, _, FShSigs shSigFct) = Just (fctName, shSigFct)
  fsh _ = Nothing

baseDistrShEnv :: Map FctName (ShapeSigFct B.TypeSB)
baseDistrShEnv = Map.fromList $ mapAndFilter fsh baseFctInfo where
  fsh (fctName, _, DShSigs shSigFct) = Just (fctName, shSigFct)
  fsh _ = Nothing

mapAndFilter :: (a -> Maybe b) -> [a] -> [b]
mapAndFilter f = catMaybes . map f

baseFctInfo :: [(FctName, Signatures, ShapeSignatures)]
baseFctInfo =
  [ ("*",
     
      sig [IType, IType] IType <>
      sig [RType, RType] RType <>
      sig [RType, VecType] VecType <>
      sig [VecType, RType] VecType <>
      sig [RType, MatType] MatType <>
      sig [MatType, RType] MatType <>
      sig [RType, RArrType 3] (RArrType 3) <>
      sig [RArrType 3, RType] (RArrType 3),
      
      FShSigs $ \(_, argts)-> case argts of
        [B.IType, t@B.IType] -> Just t
        [B.RType, t@B.RType] -> Just t
        [B.RType, t@(B.VecType _)] -> Just t
        [t@(B.VecType _), B.RType] -> Just t
        [B.RType, t@(B.MatType _ _)] -> Just t
        [t@(B.MatType _ _), B.RType] -> Just t
        _ -> Nothing
    )

  , ("*_arr3_real",
     nosigs,
     FShSigs $ \(_, argts) -> case argts of
        [t@(B.RArrType [_,_,_]), B.RType] -> Just t
        _ -> Nothing
    )

  , ("*_real_arr3",
     nosigs,
     FShSigs $ \(_, argts) -> case argts of
        [B.RType, t@(B.RArrType [_,_,_])] -> Just t
        _ -> Nothing
    )
    
  , ("/",
     
      sig [RType, RType] RType <>
      sig [RType, VecType] VecType <>
      sig [VecType, RType] VecType <>
      sig [RType, MatType] MatType <>
      sig [MatType, RType] MatType,

      FShSigs $ \(_, argts)-> case argts of
        [B.RType, t@B.RType] -> Just t
        [t@(B.VecType _), B.RType] -> Just t
        [t@(B.MatType _ _), B.RType] -> Just t
        _ -> Nothing
    )

  , ("/_real_vec",
     nosigs,
     FShSigs $ \(_, argts) -> case argts of
        [B.RType, t@(B.VecType _)] -> Just t
        _ -> Nothing
    )

  , ("/_real_mat",
     nosigs,
     FShSigs $ \(_, argts) -> case argts of
        [B.RType, t@(B.MatType _ _)] -> Just t
        _ -> Nothing
    )
    
  , ("+",
     
      sig [IType, IType] IType <>
      sig [RType, RType] RType <>
      sig [RType, VecType] VecType <>
      sig [VecType, RType] VecType <>
      sig [RType, MatType] MatType <>
      sig [MatType, RType] MatType <>
      sigVariableLength [TSDType] TSDType,

      FShSigs $ \(_, argts) -> case argts of
        [B.IType, t@B.IType] -> Just t
        [B.RType, t@B.RType] -> Just t
        [B.RType, t@(B.VecType _)] -> Just t
        [t@(B.VecType _), B.RType] -> Just t
        [B.RType, t@(B.MatType _ _)] -> Just t
        [t@(B.MatType _ _), B.RType] -> Just t
        _ -> Nothing
    )

  , ("-",
     
      sig [IType, IType] IType <>
      sig [RType, RType] RType <>
      sig [RType, VecType] VecType <>
      sig [VecType, RType] VecType <>
      sig [RType, MatType] MatType <>
      sig [MatType, RType] MatType,

      FShSigs $ \(_, argts) -> case argts of
        [B.IType, t@B.IType] -> Just t
        [B.RType, t@B.RType] -> Just t
        [B.RType, t@(B.VecType _)] -> Just t
        [t@(B.VecType _), B.RType] -> Just t
        [B.RType, t@(B.MatType _ _)] -> Just t
        [t@(B.MatType _ _), B.RType] -> Just t
        _ -> Nothing
    )
    
  , ("^",
     
      sig [RType, IType] RType <>
      sig [RType, RType] RType,

      FShSigs $ \(_, argts) -> case argts of
        [B.RType, B.IType] -> Just B.RType
        [B.RType, B.RType] -> Just B.RType
        _ -> Nothing
    )
  
  , ("[]",
      bracketSigs,
      bracketShSigs)

  , ("{}",
     arraySigs,
     arrayShSigs)
    
  , ("accum",
      sig [TSDType, RType, RType] TSDType,
      NoShSigs)

  , ("ar1",
      sig [RType, RType, RType] TSDType,
      NoShSigs)

  , ("blocks4",
     
      sig [RType, VecType, VecType, MatType] MatType <>
      sig [MatType, VecType, VecType, RType] MatType <>
      sig [MatType, MatType, MatType, MatType] MatType,
      
      NoShSigs
    )

  , ("blocks4_mmmm",
     nosigs,
     FShSigs $ \(_, argts) -> case argts of
        [B.MatType m1 n1, B.MatType _ _, B.MatType _ _, B.MatType m2 n2] ->
          Just $ matdsh m1 n1 m2 n2
        _ -> Nothing
    )

  , ("blocks4_rvvm",
      nosigs,
      FShSigs $ \(_, argts) -> case argts of
        [B.RType, B.VecType _, B.VecType _, B.MatType m n] ->
          Just $ mat1sh m n
        _ -> Nothing
    )

  , ("blocks4_mvvr",
      nosigs,
      FShSigs $ \(_, argts) -> case argts of
        [B.MatType m n, B.VecType _, B.VecType _, B.RType] ->
          Just $ mat1sh m n
        _ -> Nothing
    )
    
  , ("certainly",
     certainlySigs,
     NoShSigs)

  , ("const",
      sig [RType] TSDType,
      NoShSigs)
    
  , ("constp",
      sig [RType, RType] TSDType,
      NoShSigs)

  , ("diag",
      diagSigs,
      NoShSigs)

  , ("diag_arr3",
     nosigs,
     FShSigs $ \(_, argts) -> case argts of
        [B.RArrType [l,m,n]] ->
          Just $ B.MatType (B.Apply "*" [l, m]) (B.Apply "*" [l, n])
        _ -> Nothing
    )

  , ("diag_vec",
     nosigs,
     FShSigs $ \(_, argts) -> case argts of
        [B.VecType n] -> Just $ B.MatType n n
        _ -> Nothing
    )

  , ("diag_mats",
     nosigs,
     diagMatsShSigs)
    
  , ("diag_sqr",
      diagSigs,
      NoShSigs)
    
  , ("div",
      sig [IType, IType] IType,
      FShSigs $ shsig [B.IType, B.IType] B.IType)
    
  , ("i2r",
      sig [IType] RType,
      FShSigs $ shsig [B.IType] B.RType)
  
  , ("exponential_m",
      sig [RType] RDistrType,
      DShSigs $ shsig [B.RType] (B.RTypeB lo0))
    
  , ("exponential_mt",

      sig [RType, RType] RDistrType,
      
      DShSigs $ \x -> case x of
        ([_, ub], [B.RType, B.RType]) ->
          Just $ B.RTypeB $ B.bounds (B.litR 0.0) ub
        _ -> Nothing
    )

  , ("exponential_mt_rate",
     nosigs,
     FShSigs $ shsig [B.RType] B.RType)

  , ("exponential_r",
     sig [RType] RDistrType,
     DShSigs $ shsig [B.RType] (B.RTypeB lo0))

  , ("exponential_rt",

      sig [RType, RType] RDistrType,

      DShSigs $ \x -> case x of
        ([_, ub], [B.RType, B.RType]) ->
          Just $ B.RTypeB $ B.bounds (B.litR 0.0) ub
        _ -> Nothing
    )
  
  , ("half_cauchy",
      sig [RType] RDistrType,
      DShSigs $ shsig [B.RType] (B.RTypeB lo0))
    
  , ("half_normal",
      sig [RType] RDistrType,
      DShSigs $ shsig [B.RType] (B.RTypeB lo0))

  , ("log",
     unaryRealSigs,
     FShSigs $ unaryRealShSigs)
     
  , ("mat11",
      sig [RType] MatType,
      FShSigs $ shsig [B.RType] (B.MatType (B.litI 1) (B.litI 1)))

  , ("mat22",
     sig [RType, RType, RType, RType] MatType,
     FShSigs $ shsig [B.RType, B.RType, B.RType, B.RType]
                     (B.MatType (B.litI 2) (B.litI 2)))
    
  , ("negate", negateSig, negateShSig) -- unary "-"
     
  , ("normal",
      sig [RType, RType] RDistrType,
      DShSigs $ shsig [B.RType, B.RType] (B.RTypeB B.nobounds))

  , ("qp",
      sig [RType, RType, IType, RType, RType] TSDType,
      NoShSigs)

  , ("rw",
      sig [RType, RType, RType] TSDType,
      NoShSigs)

  , ("sqrt",
      unaryRealSigs,
      FShSigs $ unaryRealShSigs)

  , ("square",
      unaryRealSigs,
      FShSigs $ unaryRealShSigs)
    
  , ("ssm",
      sig [VecType{-Z-}, RType{-H-}, MatType{-T-}, MatType{-Q-},
           VecType{-a0-}, MatType{-P0-}]
          TSDType,
      NoShSigs)

  , ("to_matrix",

     sig [VecType] MatType,
     
     FShSigs $ \(_, argts) -> case argts of
        [B.VecType m] -> Just $ B.MatType m (B.litI 1)
        _ -> Nothing
    )

  , ("transp",

     sig [MatType] MatType,

     FShSigs $ \(_, argts) -> case argts of
        [B.MatType m n] -> Just $ B.MatType n m
        _ -> Nothing
    )

  , ("vec",
      vecSigs,
      NoShSigs)

  , ("vec_reals",
     nosigs,
     vecRealsShSigs)

  , ("vec_vecs",
     nosigs,
     vecVecsShSigs)

  , ("vec0",
     
      sig [IType] VecType,

      FShSigs $ \(args, argts) -> case (args, argts) of
        ([n], [B.IType]) -> Just $ B.VecType n
        _ -> Nothing
    )

  , ("uniform",
     
      sig [RType, RType] RDistrType,
      
      DShSigs $ \x -> case x of
        ([lo,hi], [B.RType, B.RType]) -> Just $ B.RTypeB (B.bounds lo hi)
        _ -> Nothing
    )
    
  , ("wn",
      sig [RType] TSDType,
      NoShSigs)
  ]

lo0 :: B.Bounds
lo0 = B.lobound (B.litR 0.0)

negateSig :: Signatures
negateSig =
  sig [RType] RType <>
  sig [VecType] VecType <>
  sig [MatType] MatType <>
  sig [IType] IType

negateShSig :: ShapeSignatures
negateShSig =
  FShSigs $ \(_, argts) ->
    case argts of
      [t@B.RType] -> Just t
      [t@(B.VecType _)] -> Just t
      [t@(B.MatType _ _)] -> Just t
      [t@B.IType] -> Just t
      _ -> Nothing

mat1sh :: B.Expr -> B.Expr -> B.TypeS
mat1sh m n =
  B.MatType (B.Apply "+" [m, B.litI 1]) (B.Apply "+" [n, B.litI 1])

matdsh :: B.Expr -> B.Expr -> B.Expr -> B.Expr -> B.TypeS
matdsh m1 n1 m2 n2 =
  B.MatType (B.Apply "+" [m1, m2]) (B.Apply "+" [n1, n2])

sig :: [Type] -> Type -> Signatures
sig argtypes restype = Sigs $ \types -> do
  guard $ types == argtypes
  Just restype

nosigs :: Signatures
nosigs = Sigs $ const Nothing

shsig :: [B.TypeS] -> a -> ShapeSigFct a
shsig argtypes restype = \(_, types) -> do
  guard $ types == argtypes
  Just restype

sigVariableLength :: [Type] -> Type -> Signatures
sigVariableLength argTypes resType = Sigs $ \types -> do
  guard $ all (flip elem argTypes) types
  Just resType

arraySigs :: Signatures
arraySigs =
  Sigs $ \types -> do
    (t1 : rest) <- Just types
    guard $ numDim t1 >= 2  -- avoid complications with vectors and matrices
    guard $ not (isTS t1 || isDistr t1)
    guard $ all (== t1) rest
    Just t1{numDim = numDim t1 + 1}

arrayShSigs :: ShapeSignatures
arrayShSigs =
  FShSigs $ \(_, argts) -> do
    (t1@(B.TypeS et sh) : rest) <- Just argts
    guard $ length sh >= 2  -- avoid complications with vectors and matrices
    guard $ all (== t1) rest
    let n = genericLength argts
    Just $ B.TypeS et (B.litI n : sh)

diagSigs :: Signatures
diagSigs = sigVariableLength [RType, VecType, MatType, RArrType 3] MatType

diagMatsShSigs :: ShapeSignatures
diagMatsShSigs = FShSigs sigfct where
  sigfct (_, []) = Just $ B.MatType (B.litI 0) (B.litI 0)
  sigfct (_, [argt]) = B.MatType <$> nrows argt <*> ncols argt
  sigfct (_, argts) = B.MatType <$> sumnrows argts <*> sumncols argts
  sumnrows :: [B.TypeS] -> Maybe B.Expr
  sumnrows typs = B.Apply "+" <$> mapM nrows typs
  nrows :: B.TypeS -> Maybe B.Expr
  nrows (B.MatType m _) = Just $ m
  nrows _ = Nothing
  sumncols :: [B.TypeS] -> Maybe B.Expr
  sumncols typs = B.Apply "+" <$> mapM ncols typs
  ncols :: B.TypeS -> Maybe B.Expr
  ncols (B.MatType _ n) = Just $ n
  ncols _ = Nothing

vecSigs :: Signatures
vecSigs = sigVariableLength [RType, VecType] VecType

vecRealsShSigs :: ShapeSignatures
vecRealsShSigs = FShSigs sigfct where
  sigfct (_, argtyps)
    | all (== B.RType) argtyps =
      Just $ B.VecType (B.litI $ genericLength argtyps)
  sigfct _ =
    Nothing

vecVecsShSigs :: ShapeSignatures
vecVecsShSigs = FShSigs sigfct where
  sigfct (_, argts) = B.VecType <$> sumlen argts
  sumlen :: [B.TypeS] -> Maybe B.Expr
  sumlen typs = B.Apply "+" <$> mapM len typs
  len :: B.TypeS -> Maybe B.Expr
  len (B.VecType n) = Just n
  len _ = Nothing
            
bracketSigs :: Signatures
bracketSigs = Sigs $ \ts -> do
  (t : ts') <- Just ts
  let n = genericLength ts'
  guard (simpleArrType t && 0 < n && n <= numDim t && all (== IType) ts')
  Just t{numDim = numDim t - n}

bracketShSigs :: ShapeSignatures
bracketShSigs = FShSigs $ \(_, argts) -> do
  (B.TypeS et sh : ts') <- Just argts
  let n = length ts'
  guard (0 < n && n <= length sh && all (== B.IType) ts')
  Just $ B.TypeS et $ drop n sh

simpleArrType :: Type -> Bool
simpleArrType t =
  not (isTS t) && not (isDistr t) && numDim t > 0

unaryRealSigs :: Signatures
unaryRealSigs = Sigs $ \ts -> do
  [t] <- Just ts
  guard $ elemType t == RealType && not (isTS t) && not (isDistr t)
  Just t

certainlySigs :: Signatures
certainlySigs = Sigs $ \ts -> do
  [t] <- Just ts
  guard $ elemType t == RealType && not (isTS t) && not (isDistr t)
  Just t{isDistr = True}

unaryRealShSigs :: ShapeSigFct B.TypeS
unaryRealShSigs = \(_, ts) -> do
  [t@(B.TypeS RealType _)] <- Just ts
  Just t
