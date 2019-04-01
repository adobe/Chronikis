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
module Transforms
  ( xformFctDef, expansionXform
  -- for testing only
  , simplifyDiag, simplifyVec
  )
where

import Control.Exception (assert)
import Control.Monad ((>=>))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)
import Data.Tuple.Extra (first)
import qualified Data.Scientific as Scientific
--import Debug.Trace (trace)

import Misc
import AST
import NameGen
import Number
import Periodic

xformFctDef :: (ExprT -> NameGen ExprT) -> FctDefT -> FctDefT
xformFctDef xform =
  applyNG $ \d -> do
    parms <- mapM xformParm (defParms d)
    body <- xform (defBody d)
    return d{ defParms = parms, defBody = body }
  where
    xformParm (v, tsb) = do
      shape <- mapM xform (tsbShape tsb)
      let bnds = tsbBounds tsb
      lo <- traverse xform (lobnd bnds)
      hi <- traverse xform (hibnd bnds)
      return (v, tsb{ tsbShape = shape, tsbBounds = Bounds lo hi })

expansionXform :: ExprT -> NameGen ExprT
expansionXform =
  bottomUpXform consolidateTSDSums >=>
  -- Not entirely sure the above is necessary
  bottomUpXform xpandTop >=>
  bottomUpXform liftSSMargs

type TransformTop = Type -> FctName -> [ExprT] -> NameGen ExprT

-- DEFINITION: An Expr value e0 is in /linear form/ if it has the form
--   f1 $ f2 $ ... $ fn $ e
-- where each fi has one of the two forms
--   Let ti vi ei
--   Draw ti vi ei
-- and e has no Let or Draw subexpression.

-- REQUIRE: (xformTop typ fctName args) returns a value in linear form if
-- no expression in args contains a Let or Draw subexpression.
--
bottomUpXform :: TransformTop -> ExprT -> NameGen ExprT
bottomUpXform xformTop expr =
  do
    (defsFct, expr') <- xform expr
    return (defsFct expr')
  where
    xform :: ExprT -> NameGen (ExprT -> ExprT, ExprT)
    xform (Apply typ fctName args) = do
      (defsFcts, args') <- unzip <$> mapM xform args
      e <- xformTop typ fctName args'
      let (df, e') = splitDefs e
      assert (hasType typ e') $
        return (compose defsFcts . df, e')
    xform (Let typ v edef ebody) = do
      (defsFct1, edef') <- xform edef
      (defsFct2, ebody') <- xform ebody
      assert (hasType typ ebody') $
        assert (typeOf edef' == typeOf edef) $
        assert (typeOf ebody' == typeOf ebody) $
        return (defsFct1 . letT v edef' . defsFct2, ebody')
    xform (Draw typ v edistr ebody) = do
      (defsFct1, edistr') <- xform edistr
      (defsFct2, ebody') <- xform ebody
      assert (hasType typ ebody') $
        assert (typeOf edistr' == typeOf edistr) $
        assert (typeOf ebody' == typeOf ebody) $
        return (defsFct1 . drawT' v edistr' . defsFct2, ebody')
    xform e =
      return (id, e)
    drawT' v (Apply typ "certainly" [e])
      | isDistr typ = letT v e
    drawT' v edistr
      = drawT v edistr

{-
If splitDefs e returns (f, e') then
- e == f e'
- e' does not have a Draw or Let at the top level
- f == f1 . ... . fn where each fi is of the form Let t v e or Draw t v e
-}
splitDefs :: ExprT -> (ExprT -> ExprT, ExprT)
splitDefs (Let _ v edef ebody) =
  first ((letT v edef) .) $ splitDefs ebody
splitDefs (Draw _ v edistr ebody) =
  first ((drawT v edistr) .) $ splitDefs ebody
splitDefs e = (id, e)

-- Compose a list of functions
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

consolidateTSDSums :: TransformTop
consolidateTSDSums TSDType "+" args
  | (all (hasType TSDType) args) =
    return $ Apply TSDType "+" $ concatMap summands args
  where
    summands (Apply TSDType "+" sargs) = sargs
    summands x                        = [x]
consolidateTSDSums typ fctName args =
  return $ Apply typ fctName args

data SSMArgs = SSMArgs { argZ, argH, argT, argQ, arga0, argP0 :: ExprT }

ssmFrom :: ExprT -> Maybe SSMArgs
ssmFrom (Apply TSDType "ssm" args@[aZ, aH, aT, aQ, aa0, aP0])
  | (map typeOf args == [VecType, RType, MatType, MatType, VecType, MatType]) =
    Just SSMArgs{
      argZ = aZ, argH = aH, argT = aT, argQ = aQ, arga0 = aa0, argP0 = aP0
    }
ssmFrom _ = Nothing

liftLets :: ExprT -> ExprT
liftLets (Apply typ fctName args) =
  let (defsFcts, args') = unzip $ map splitDefs args
  in compose defsFcts $ Apply typ fctName args'
liftLets expr = expr

xpandQPTop :: [ExprT] -> NameGen ExprT
xpandQPTop args@[period_e, ell_e, mindof_e, rho, sigma]
  | (map typeOf args == [RType, RType, IType, RType, RType]) &&
    (all isLit [period_e, ell_e, mindof_e]) =
    xpandQP period ell mindof rho sigma
    where
      period = toDouble(period_e)
      ell    = toDouble(ell_e)
      mindof = toInt(mindof_e)
      toDouble (Lit _ (RealVal x)) = Scientific.toRealFloat x :: Double
      toDouble _ = error "Trying to convert a non-literal to Double"
      toInt (Lit _ (IntVal n)) = fromIntegral n :: Int
      toInt _ = error "Trying to convert a non-literal to Int"
xpandQPTop args
  = return $ Apply TSDType "qp" args

xpandQP :: Double -> Double -> Int -> ExprT -> ExprT -> NameGen ExprT
xpandQP period ell mindof rho sigma =
  do
    -- Let variables
    vrhoSqr <- newName "rhoSqr"
    let rhoSqr = Var RType vrhoSqr
    vcsigma2s <- newName "csigma2s"
    let csigma2s = Var VecType vcsigma2s
    -- SSM arguments
    let aZ = vecExpr zvalues
    let aH = litR 0.0
    let phi = Apply RType "sqrt" [Apply RType "-" [litR 1.0, rhoSqr]]
    let aT = diag [Apply (RArrType 3) "*" [phi, rotMatrices]]
    let aQ = diag [Apply VecType "*" [rhoSqr, csigma2s]]
    (defFct, aa0) <- splitDefs <$> xpandTop VecType "vec0" [litI numCoeffs2]
    let aP0 = diag [csigma2s]
    -- Final expression
    return $
      defFct $
      letT vcsigma2s (Apply VecType "*" [Apply RType "square" [sigma],
                                         vecExpr coeffs2]) $
      letT vrhoSqr (Apply RType "square" [rho]) $
      Apply TSDType "ssm" [aZ, aH, aT, aQ, aa0, aP0]
  where
    coeffs =
       optimized_pcf_coeffs
       Periodic{ p_period = period, p_length_scale = ell, p_min_dof = mindof }
    numCoeffs = length coeffs
    zvalues = concat $ replicate numCoeffs [1.0, 0.0]
    coeffs2 = concatMap (replicate 2) coeffs
    numCoeffs2 = fromIntegral (length coeffs2)
    vecExpr xs = vec (map toLitR xs)
    rotMatrices =
      assert (numCoeffs > 0) $
      Apply (RArrType 3) "{}" $
      map (rotMatrix . (theta0 *) . fromIntegral) [1..numCoeffs]
    theta0 = 2.0 * pi / period
    rotMatrix theta = Apply MatType "mat22" $
                      map toLitR [costh, -sinth, sinth, costh]
      where costh = cos theta
            sinth = sin theta
    toLitR = litR . Scientific.fromFloatDigits

vec :: [ExprT] -> ExprT
vec = Apply VecType "vec"

diag :: [ExprT] -> ExprT
diag = Apply MatType "diag"

{-
RETURNS: normalized form for (Apply VecType "vec" args)
Normalized form for vec: either
1. no top-level "vec" application, or
2. all scalar arguments, or
2. all vector arguments, with
   - at least two arguments
   - no argument of form vec(a1,...,an) unless n > 0 and
     all the ai are scalars, and
   - no two consecutive arguments of form vec(a1,...,an)
-}
simplifyVec :: [ExprT] -> ExprT
-- The algorithm for simplifyVec is described in Notes/simplify_vec.lyx.
-- We represent u and v by reversed lists of their arguments, and
-- w by its argument list.
simplifyVec args = assert ok $ simplify [] [] args where
  ok = all (flip elem [RType, VecType] . typeOf) args
  simplify uargs vargs (s : xs) | hasType RType s
    = simplify uargs (s : vargs) xs
  simplify uargs vargs (Apply VecType "vec" ys : xs)
    = simplify uargs vargs (ys ++ xs)
  simplify uargs vargs@(_:_) wargs
    = simplify (vec (reverse vargs) : uargs) [] wargs
  simplify uargs [] (y : xs)
    = simplify (y : uargs) [] xs
  simplify [uarg] [] []
    = uarg
  simplify uargs [] []
    = vec $ reverse uargs

xpandVec0 :: [ExprT] -> NameGen ExprT
xpandVec0 [Lit IType (IntVal n)]
  | (n <= 10) = return $ vec $ replicate (fromIntegral n) (litR 0.0)
xpandVec0 args
  = return $ Apply VecType "vec0" args

simplifyLength :: [ExprT] -> ExprT
simplifyLength [Apply VecType "vec" args]
  | all (hasType RType) args = litI $ fromIntegral $ length args
simplifyLength args
  = Apply IType "length" args

xpandTSDSum :: [ExprT] -> NameGen ExprT
xpandTSDSum [] =
  xpandTop TSDType "const" [litR 0.0]
xpandTSDSum [arg]
  | (hasType TSDType arg) = return arg
xpandTSDSum args
  | all isJust args' = do
    aZ <- xpandTop VecType "vec" $ map argZ ssms
    aH <- xpandTop RType "+" $ map argH ssms
    aT <- xpandTop MatType "diag" $ map argT ssms
    aQ <- xpandTop MatType "diag" $ map argQ ssms
    aa0 <- xpandTop VecType "vec" $ map arga0 ssms
    aP0 <- xpandTop MatType "diag" $ map argP0 ssms
    return $ liftLets $ Apply TSDType "ssm" [aZ, aH, aT, aQ, aa0, aP0]
  where
    args' = map ssmFrom args
    ssms = map fromJust args'
xpandTSDSum args =
  return $ Apply TSDType "+" args

simplifyRealExpr :: FctName -> [ExprT] -> ExprT
simplifyRealExpr "+" [] = litR 0.0
simplifyRealExpr "+" [e]
  | hasType RType e = e
simplifyRealExpr "+" args
  | any (== (litR 0.0)) args =
    simplifyRealExpr "+" $ filter (/= (litR 0.0)) args
  | otherwise =
    Apply RType "+" args
simplifyRealExpr "-" [e1, e2]
  | (e2 == litR 0) = e1
simplifyRealExpr "*" [] = litR 1.0
simplifyRealExpr "*" [e]
  | hasType RType e = e
simplifyRealExpr "*" args
  | any (== (litR 1.0)) args =
    simplifyRealExpr "*" $ filter (/= (litR 1.0)) args
  | otherwise =
    Apply RType "*" args
simplifyRealExpr "/" [e, one]
  | hasType RType e && (one == litR 1.0) = e
simplifyRealExpr "/" args@[Apply RType "*" [e1, e2], e3]
  | all (hasType RType) [e1, e2, e3] =
    if e2 == e3 then e1
    else if e1 == e3 then e2
    else Apply RType "/" args
simplifyRealExpr fctName args = Apply RType fctName args

xpandAccum :: [ExprT] -> NameGen ExprT
xpandAccum [ssmExpr, mu, sigma]
  | (isJust ssm' && all (hasType RType) [mu, sigma]) =
  do
    let vecZ0 = argZ ssm
    (df1, lenZ0) <- splitDefs <$> xpandTop IType "length" [vecZ0]
    (df2, zeroes) <- splitDefs <$> xpandTop VecType "vec0" [lenZ0]
    aZ <- xpandTop VecType "vec" [vec [litR 1.0], zeroes]
    let aH = litR 0.0
    aT <- xpandTop MatType "blocks4" [litR 1.0, vecZ0, zeroes, argT ssm]
    aQ <- xpandTop MatType "diag" [argH ssm, argQ ssm]
    aa0 <- xpandTop VecType "vec" [mu, arga0 ssm]
    aP0 <- xpandTop MatType "diag" [Apply RType "square" [sigma], argP0 ssm]
    let essm = Apply TSDType "ssm" [aZ, aH, aT, aQ, aa0, aP0]
    return $ df1 $ df2 $ liftLets essm
  where
    ssm' = ssmFrom ssmExpr
    Just ssm = ssm'
xpandAccum args =
  return $ Apply TSDType "accum" args

xpandAr1 :: [ExprT] -> NameGen ExprT
xpandAr1 args@[phi, sigmaQ, sigmaA]
  | all (hasType RType) args = do
      let aZ = vec [litR 1.0]
      let aH = litR 0.0
      let aT = simplifyDiag [phi]
      aQ <- xpandTop MatType "diag_sqr" [sigmaQ]
      let aa0 = vec [litR 0.0]
      aP0 <- xpandTop MatType "diag_sqr" [sigmaA]
      return $ liftLets $ Apply TSDType "ssm" [aZ, aH, aT, aQ, aa0, aP0]
xpandAr1 args
  = return $ Apply TSDType "ar1" args

xpandConst :: [ExprT] -> NameGen ExprT
xpandConst [val]
  | (hasType RType val) = do
      let aZ = vec [litR 1.0]
      let aH = litR 0.0
      let aT = simplifyDiag [litR 1.0]
      let aQ = simplifyDiag [litR 0.0]
      let aa0 = vec [val]
      let aP0 = simplifyDiag [litR 0.0]
      return $ Apply TSDType "ssm" [aZ, aH, aT, aQ, aa0, aP0]
xpandConst args
  = return $ Apply TSDType "const" args

xpandConstp :: [ExprT] -> NameGen ExprT
xpandConstp args@[mu, sigma]
  | (all (hasType RType) args) = do
      let aZ = vec [litR 1.0]
      let aH = litR 0.0
      let aT = simplifyDiag [litR 1.0]
      let aQ = simplifyDiag [litR 0.0]
      let aa0 = vec [mu]
      aP0 <- xpandTop MatType "diag_sqr" [sigma]
      return $ liftLets $ Apply TSDType "ssm" [aZ, aH, aT, aQ, aa0, aP0]
xpandConstp args
  = return $ Apply TSDType "constp" args

xpandRw :: [ExprT] -> NameGen ExprT
xpandRw args@[muA, sigmaA, sigmaQ]
  | (all (hasType RType) args) = do
      let aZ = vec [litR 1.0]
      let aH = litR 0.0
      let aT = diag [litR 1.0]
      aQ <- xpandTop MatType "diag_sqr" [sigmaQ]
      let aa0 = vec [muA]
      aP0 <- xpandTop MatType "diag_sqr" [sigmaA]
      return $ liftLets $ Apply TSDType "ssm" [aZ, aH, aT, aQ, aa0, aP0]
xpandRw args
  = return $ Apply TSDType "rw" args

xpandWn :: [ExprT] -> NameGen ExprT
xpandWn [sigma]
  | (hasType RType sigma) = do
      let aZ = vec []
      let aH = Apply RType "square" [sigma]
      let aT = diag []
      let aQ = diag []
      let aa0 = vec []
      let aP0 = diag []
      return $ Apply TSDType "ssm" [aZ, aH, aT, aQ, aa0, aP0]
xpandWn args
  = return $ Apply TSDType "wn" args

xpandExponentialMT :: [ExprT] -> NameGen ExprT
xpandExponentialMT [mean, ub] =
  case ub of
    Var RType _ -> xpandit id ub
    Lit RType _ -> xpandit id ub
    Var _ _     -> ubTypeError
    Lit _ _     -> ubTypeError
    _           -> do vub <- newName "ub"
                      xpandit (letT vub ub) (Var RType vub)
  where
    ubTypeError = logicError "Wrong type for ub arg of exponential_mt"
    xpandit letfct ub1 = do
      let norm_mean = simplifyRealExpr "/" [mean, ub1]
      let rate = simplifyRealExpr "/"
                 [Apply RType "exponential_mt_rate" [norm_mean], ub1]
      return $ letfct $ Apply RDistrType "exponential_rt" [rate, ub1]
xpandExponentialMT _
  = logicError "exponential_mt must have exactly two arguments"

{-
RETURNS: equivalent normalized form for (Apply MatType "diag" args)
Normalized form for diag is one of these:
- diag().
- diag(s) for a scalar expression s. (This is called scalar-normal.)
- diag(v) for a vector expression v such if v = vec(e_1,...,e_n), then
  v is in normal form and n > 1. (This is called vector-normal.)
- diag(A) for 3D array A. (This is called array-normal.)
- diag(M_1, ..., M_n) for matrices M_1, ..., M_n, n > 1, where
  - if M_i is diagish then it is unary-normal.
  - no two consecutive arguments M_i are sv-normal
where
  - M is diagish if it has form diag(e_1,...,e_n) for n >= 0
  - A diagish M is sv-normal if it is scalar-normal or vector-normal
  - a diagish M is unary-normal if it is scalar-normal, vector-normal, or
    arry-normal.
-}
simplifyDiag :: [ExprT] -> ExprT
-- The algorithm for simplifyDiag is described in Notes/simplify_diag.lyx.
-- We represent A and B by reversed lists of their arguments, and
-- C by its argument list.
simplifyDiag args = assert ok $ simplify [] [] args where
  ok = all (flip elem [RType, VecType, MatType, RArrType 3] . typeOf) args
  simplify argsA argsB (y : xs) | typeOf y `elem` [RType, VecType]
    = simplify argsA (y : argsB) xs
  simplify argsA argsB (Apply MatType "diag" ys : xs)
    = simplify argsA argsB (ys ++ xs)
  simplify argsA argsB (Apply MatType "mat11" [s] : xs)
    = simplify argsA argsB (s : xs)
  simplify argsA argsB@(_:_) argsC
    = simplify (updateA (simplifyVec $ reverse argsB)) [] argsC
    where
      updateA (Apply VecType "vec" [])
        = argsA
      updateA (Apply VecType "vec" [s]) | hasType RType s
        = Apply MatType "mat11" [s] : argsA
      updateA v | hasType VecType v
        = diag [v] : argsA
      updateA _
        = logicError "simplifyVec should have return a vector expression"
  simplify argsA [] (y : xs)
    = simplify (y' : argsA) [] xs
    where
      y' = case typeOf y of
             MatType    -> y
             RArrType 3 -> diag [y]
             _          -> logicError "y should be a matrix or 3D array"
  simplify [argA] [] []
    = argA
  simplify argsA [] []
    = diag $ reverse argsA

simplifyBlocks4 :: [ExprT] -> ExprT
simplifyBlocks4 [Apply MatType "diag" [], b, c, d]
  | all (hasType MatType) [b, c, d] = d  -- assumes shapes match
simplifyBlocks4 [a, b, c, Apply MatType "diag" []]
  | all (hasType MatType) [a, b, c] = a  -- assumes shapes match
simplifyBlocks4 [Apply MatType "mat11" [a], Apply MatType "mat11" [b],
                 Apply MatType "mat11" [c], Apply MatType "mat11" [d]]
  | all (hasType RType) [a, b, c, d]
  = Apply MatType "mat22" [a, b, c, d]
simplifyBlocks4 args@[a, b, c, d]
  | all (hasType MatType) [a, b, c, d] = Apply MatType "blocks4" args
simplifyBlocks4 [a, b, c, d]
  | map typeOf [a, b, c, d] == [RType, VecType, VecType, MatType]
  = simplifyBlocks4 [Apply MatType "mat11" [a],
                     simplifyTransp [simplifyToMatrix [b]],
                     simplifyToMatrix [c],
                     d]
simplifyBlocks4 [a, b, c, d]
  | map typeOf [a, b, c, d] == [MatType, VecType, VecType, RType]
  = simplifyBlocks4 [a,
                     simplifyToMatrix [b],
                     simplifyTransp [simplifyToMatrix [c]],
                     Apply MatType "mat11" [d]]
simplifyBlocks4 args  -- we shouldn't actually get here...
  = Apply MatType "blocks4" args

simplifyToMatrix :: [ExprT] -> ExprT
simplifyToMatrix [Apply VecType "vec" vargs@[varg]]
  | hasType RType varg = Apply MatType "mat11" vargs
simplifyToMatrix args = Apply MatType "to_matrix" args

simplifyTransp :: [ExprT] -> ExprT
simplifyTransp [targ@(Apply MatType "mat11" [marg])]
  | hasType RType marg = targ
simplifyTransp [Apply MatType "transp" [targ]]
  | hasType MatType targ = targ
simplifyTransp args = Apply MatType "transp" args

simplifyPower :: [ExprT] -> ExprT
simplifyPower [x, Lit IType (IntVal 2)]
  | hasType RType x = Apply RType "square" [x]
simplifyPower args
  = Apply RType "^" args

xpandDiagSqr :: [ExprT] -> NameGen ExprT
xpandDiagSqr args =
  do
    (defFcts, args') <- unzip <$> mapM (fmap splitDefs . squaredArg) args
    compose defFcts <$> xpandTop MatType "diag" args'
  where
    squaredArg arg = xpandTop (typeOf arg) "square" [arg]

normalizeScale :: DistrName -> DistrName -> FctName -> [ExprT] -> NameGen ExprT
normalizeScale dname0 dname1 binop = f where
  f [arg] | (arg /= litR 1) =
    do v <- newName "raw"
       let ve = Var RType v
       return $
         drawT v (Apply RDistrType dname1 [litR 1]) $
         Apply RDistrType "certainly" [Apply RType binop [ve, arg]]
  f args =
    return $ Apply RDistrType dname0 args

xpandExponentialM :: [ExprT] -> NameGen ExprT
xpandExponentialM = normalizeScale "exponential_m" "exponential_r" "*"

xpandExponentialR :: [ExprT] -> NameGen ExprT
xpandExponentialR = normalizeScale "exponential_r" "exponential_r" "/"

xpandHalfCauchy :: [ExprT] -> NameGen ExprT
xpandHalfCauchy = normalizeScale "half_cauchy" "half_cauchy" "*"

xpandHalfNormal :: [ExprT] -> NameGen ExprT
xpandHalfNormal = normalizeScale "half_normal" "half_normal" "*"

xpandNormal :: [ExprT] -> NameGen ExprT
xpandNormal [mu, sigma]
  | not (mu == litR 0 && sigma == litR 1) =
    do v <- newName "raw"
       let ve = Var RType v
       let retval = simplifyRealExpr "+" [mu, simplifyRealExpr "*" [ve, sigma]]
       return $
         drawT v (Apply RDistrType "normal" [litR 0, litR 1]) $
         Apply RDistrType "certainly" [retval]
xpandNormal args =
  return $ Apply RDistrType "normal" args

xpandUniform :: [ExprT] -> NameGen ExprT
xpandUniform [lo, hi]
  | not (lo == litR 0 && hi == litR 1) =
    do v <- newName "raw"
       let ve = Var RType v
       let scale = simplifyRealExpr "-" [hi, lo]
       let retval = simplifyRealExpr "+" [lo, simplifyRealExpr "*" [ve, scale]]
       return $
         drawT v (Apply RDistrType "uniform" [litR 0, litR 1]) $
         Apply RDistrType "certainly" [retval]
xpandUniform args =
  return $ Apply RDistrType "uniform" args

xpandTop :: TransformTop
xpandTop typ fctName args =
  case Map.lookup (typ, fctName) xpandMap of
    Just xpandfct -> xpandfct args
    Nothing ->
      case typ of
        RType -> return $ simplifyRealExpr fctName args
        _     -> return $ Apply typ fctName args

xpandMap :: Map.Map (Type, FctName) ([ExprT] -> NameGen ExprT)
xpandMap =
  Map.fromList
  [ (TSDType, "+") .-> xpandTSDSum
  , (RType, "^") .-> return . simplifyPower
  , (TSDType, "accum") .-> xpandAccum
  , (TSDType, "ar1") .-> xpandAr1
  , (TSDType, "const") .-> xpandConst
  , (TSDType, "constp") .-> xpandConstp
  , (IType, "length") .-> return . simplifyLength
  , (TSDType, "rw") .-> xpandRw
  , (TSDType, "wn") .-> xpandWn
  , (TSDType, "qp") .-> xpandQPTop
  , (VecType, "vec") .-> return . simplifyVec
  , (VecType, "vec0") .-> xpandVec0
  , (MatType, "diag_sqr") .-> xpandDiagSqr
  , (MatType, "diag") .-> return . simplifyDiag
  , (MatType, "blocks4") .-> return . simplifyBlocks4
  , (MatType, "to_matrix") .-> return . simplifyToMatrix
  , (MatType, "transp") .-> return . simplifyTransp
  , (RDistrType, "exponential_mt") .-> xpandExponentialMT
  , (RDistrType, "exponential_m") .-> xpandExponentialM
  , (RDistrType, "exponential_r") .-> xpandExponentialR
  , (RDistrType, "half_cauchy") .-> xpandHalfCauchy
  , (RDistrType, "half_normal") .-> xpandHalfNormal
  , (RDistrType, "normal") .-> xpandNormal
  , (RDistrType, "uniform") .-> xpandUniform
  ]

-- A hack to get around a bug in the Stan compiler that manifests when
-- gaussian_dlm_obs has non-trivial arguments
liftSSMargs :: TransformTop
liftSSMargs TSDType "ssm" args =
  (liftLets . Apply TSDType "ssm") <$> mapM lettify (zip argNames args)
  where
    argNames = ["Z", "H", "T", "Q", "a0", "P0"]
    lettify (_, e@Var{}) = return e
    lettify (argName, e) = do
      v <- newName argName
      let ve = Var (typeOf e) v
      return $ letT v e ve
liftSSMargs typ fctName args =
  return $ Apply typ fctName args
