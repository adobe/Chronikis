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
{-# LANGUAGE QuasiQuotes #-}
module TransformsSpec (main, spec) where

import Control.Monad (forM_)
--import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.Hspec
--import Test.HUnit
--import Text.Pretty.Simple (pPrintLightBg)

import Misc
import NameGen
import Number
import AST
import TransformsImpl

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  test_xformFctDef
  regression_test_simplifyVec
  regression_test_simplifyDiag
  test_simplifyDiag
  test_foldConst
  test_substituteConstQpParams
  test_removeUnusedDefs

test_foldConst :: Spec
test_foldConst =
  describe "foldConst" $ do
    let plusR x y = Apply RType "+" [x, y]
        plusI x y = Apply IType "+" [x, y]
        minusR x y = Apply RType "-" [x, y]
        minusI x y = Apply IType "-" [x, y]
        timesR x y = Apply RType "*" [x, y]
        timesI x y = Apply IType "*" [x, y]
        negR x = Apply RType "negate" [x]
        negI x = Apply IType "negate" [x]
        divR x y = Apply RType "/" [x, y]
        divI x y = Apply IType "div" [x, y]
        squareR x = Apply RType "square" [x]
        powR x y = Apply RType "^" [x, y]
        varR x = Var RType x
        varI x = Var IType x
    it "gives up on non-consts" $ do
      foldConst Map.empty (varR "x") `shouldBe` Nothing
      foldConst Map.empty (litI 3 `plusI` varI "n") `shouldBe` Nothing
    it "gives up on functions not on the list" $ do
      foldConst Map.empty (Apply RType "foo" [litI 1]) `shouldBe` Nothing
    it "passes through literals" $ do
      foldConst Map.empty (litI 3) `shouldBe` Just (IVal 3)
      foldConst Map.empty (litR 2.5) `shouldBe` Just (RVal 2.5)
    it "substitutes vars" $ do
      let cmap = Map.fromList [ "n" .-> IVal 5, "x" .-> RVal 3.25 ]
      foldConst cmap (varI "n") `shouldBe` Just (IVal 5)
      foldConst cmap (varR "x") `shouldBe` Just (RVal 3.25)
    it "Applies integer functions" $ do
      let cmap = Map.empty
      foldConst cmap (litI 1 `plusI` litI 2)
        `shouldBe` Just (IVal 3)
      foldConst cmap (litI 2 `timesI` litI 3)
        `shouldBe` Just (IVal 6)
      foldConst cmap (litI 10 `minusI` litI 3)
        `shouldBe` Just (IVal 7)
      foldConst cmap (negI $ litI 4)
        `shouldBe` Just (IVal $ -4)
      foldConst cmap (litI 7 `divI` litI 3)
        `shouldBe` Just (IVal 2)
    it "Applies real functions" $ do
      let cmap = Map.empty
      foldConst cmap (litR 1.25 `plusR` litR 1.75)
        `shouldBe` Just (RVal 3)
      foldConst cmap (litR 2.5 `timesR` litR 3)
        `shouldBe` Just (RVal 7.5)
      foldConst cmap (litR 3 `minusR` litR 1.75)
        `shouldBe` Just (RVal 1.25)
      foldConst cmap (negR $ litR 0.75)
        `shouldBe` Just (RVal $ -0.75)
      foldConst cmap (litR 3.5 `divR` litR 4)
        `shouldBe` Nothing -- We're punting on division, because sometimes
                           -- the result cannot be represented exactly as
                           -- a Scientific value, and the Data.Scientific
                           -- package throws an exception.
      foldConst cmap (squareR $ litR $ -1.5)
        `shouldBe` Just (RVal 2.25)
      foldConst cmap (litR 3 `powR` litI 0) `shouldBe` Just (RVal 1.0)
      foldConst cmap (litR 3 `powR` litI 1) `shouldBe` Just (RVal 3.0)
      foldConst cmap (litR 5 `powR` litI 2) `shouldBe` Just (RVal 25.0)
      foldConst cmap (litR 2 `powR` litI 3) `shouldBe` Just (RVal 8.0)
      foldConst cmap (litR 2 `powR` litI (-3))
        `shouldBe` Nothing -- We're punting on raising real to a
                           -- negative integer, because sometimes the
                           -- result cannot be represented exactly as a
                           -- scientific value.
      foldConst cmap (litR 3 `powR` litR 2)
        `shouldBe` Nothing -- Scientific does not define exponentiation
                           -- with exponenent that is Scientific
      foldConst cmap (Apply RType "i2R" [litI 7])
        `shouldBe` Just (RVal 7.0)
    it "recurses into subexpressions" $ do
      let cmap = Map.fromList
                 ["n" .-> IVal 3, "x" .-> RVal 2.5, "y" .-> RVal (-1.75)]
      foldConst cmap ((litI 2 `timesI` varI "n") `plusI` litI 10)
        `shouldBe` Just (IVal 16)
      foldConst cmap ((varR "x" `plusR` varR "y") `minusR` litR 10)
        `shouldBe` Just (RVal (-9.25))
      foldConst cmap ((litI 5 `timesI` varI "missing") `divI` litI 3)
        `shouldBe` Nothing

test_substituteConstQpParams :: Spec
test_substituteConstQpParams =
  describe "substituteConstQpParams" $ do
    let subc = substituteConstQpParams
    let qp = Apply TSDType "qp"
    let varR = Var RType
    let varI = Var IType
    let lettsd = Let TSDType
    
    it "substitutes into qp" $ do
      let templ e = lettsd "n" (litI 6) $
                    lettsd "l" (litR 0.9) $
                    lettsd "p" (litR 7.0) $
                    e
      subc (templ $ qp [varR "p", varR "l", varI "n", varR "rho", varR "sigma"])
        `shouldBe`
        (templ $ qp [litR 7.0, litR 0.9, litI 6, varR "rho", varR "sigma"])
    
    it "folds constants" $ do
      let templ e = lettsd "dof" (Apply IType "*" [litI 2, litI 3]) $
                    lettsd "ell" (Apply RType "+" [litR 0.5, litR 0.25]) $
                    lettsd "per" (Apply RType "-" [litR 12, litR 3]) $
                    e
      subc (templ $ qp
             [ Apply RType "+" [litR 1.5, varR "per"]
             , Apply RType "*" [varR "ell", litR 0.5]
             , Apply IType "-" [varI "dof", litI 1]
             , litR 0.35, litR 5.23
             ])
        `shouldBe`
        (templ $ qp [litR 10.5, litR 0.375, litI 5, litR 0.35, litR 5.23])
    
    it "recurses into let def body" $ do
      let templ e =
            lettsd "n" (litI 3) $
            lettsd "x" (lettsd "a" (litI 1) $ lettsd "b" (litI 2) $ e) $
            (Apply TSDType "+"
              [ Var TSDType "x"
              , Apply TSDType "wn" [Var RType "sigma"]
              ])
      subc (templ $ qp
             [ litR 4.5, litR 0.5
             , Apply IType "+" [varI "a", varI "b", varI "n"]
             , varR "r", varR "s"])
        `shouldBe`
        (templ $ qp [litR 4.5, litR 0.5, litI 6, varR "r", varR "s"])
    
    it "recurses into draw def body" $ do
      let templ e1 e2 =
            lettsd "k" (litI 2) $
            lettsd "a" (litR 14.3) $
            lettsd "b" (litR 0.825) $
            Draw TSDType "y" e1 $
            Apply TSDType "+" [Var TSDType "y", e2]
      subc (templ (qp [varR "a", varR "b", varI "k", varR "rho", varR "sig"])
                  (qp [ Apply RType "+" [varR "b", litR 2.5]
                      , Apply RType "*" [varR "a", litR 0.05]
                      , Apply IType "-" [litI 5, varI "k"]
                      , litR 0.1, litR 3.5
                      ]))
        `shouldBe`
        (templ (qp [litR 14.3, litR 0.825, litI 2, varR "rho", varR "sig"])
               (qp [litR 3.325, litR 0.715, litI 3, litR 0.1, litR 3.5]))
    
    it "recurses into function args" $ do
      let templ e =
            lettsd "per" (litR 25.5) $
            lettsd "ell" (litR 0.625) $
            lettsd "n"   (litI 15) $
            Apply TSDType "+" [ Apply TSDType "wn" [varR "sigma"], e ]
      subc (templ (qp [varR "per", varR "ell", varI "n", varR "rho", varR "s"]))
        `shouldBe`
        (templ (qp [litR 25.5, litR 0.625, litI 15, varR "rho", varR "s"]))

test_removeUnusedDefs :: Spec
test_removeUnusedDefs =
  describe "removeUnusedDefs" $ do
    let varR = Var RType
    let varI = Var IType
    let lettsd = Let TSDType
    let drawtsd = Draw TSDType
    
    it "Leaves expr unchanged when all vars used" $ do
      let e = lettsd "sigma" (litR 2.0) $
              drawtsd "x" (Apply RDistrType "exponential" [litR 5.5]) $
              Apply TSDType "wn" [ Apply RType "*" [varR "x", varR "sigma"] ]
      removeUnusedDefs e `shouldBe` e

    it "Removes unused let def" $ do
      let ee = drawtsd "x" (Apply RDistrType "exponential" [litR 5.5]) $
               Apply TSDType "wn" [ Apply RType "*" [varR "x", varR "sigma"] ]
          e0 = lettsd "sigma" (litR 2.0) $
               lettsd "foo" (litR 27.5) $
               ee
          e1 = lettsd "sigma" (litR 2.0) $
               ee
      removeUnusedDefs e0 `shouldBe` e1

    it "Removes unused draw" $ do
      let e0 = lettsd "sigma" (litR 2.0) $
               drawtsd "x" (Apply RDistrType "exponential" [litR 5.5]) $
               Apply TSDType "wn" [ varR "sigma" ]
          e1 = lettsd "sigma" (litR 2.0) $
               Apply TSDType "wn" [ varR "sigma" ]
      removeUnusedDefs e0 `shouldBe` e1

    it "Handles nested defs/draws correctly" $ do
      let e0 = lettsd "x" (litR 2.0) $
               drawtsd "y" (Var RDistrType "ydistr") $
               e1
          e1 = lettsd "y" (litI 12) $
               drawtsd "x" (Var RDistrType "xdistr") $
               Apply TSDType "foo" [varI "y", varR "x"]
      removeUnusedDefs e0 `shouldBe` e1
      
test_xformFctDef :: Spec
test_xformFctDef =
  describe "xformFctDef" $ do
    let xform :: ExprT -> NameGen ExprT
        xform e = do
          v <- newName "bar"
          return $ Apply (typeOf e) "foo" [Var IType v, e]
    let parms =
          [ "n" .-> typeSB IType [] (lobound $ litI 0)
          , "a" .-> typeSB VecType
                      [Var IType "n"] (bounds (litR 1.0) (litR 10.0))
          ]
    let parms1 =
          [ "n" .-> typeSB IType []
                    (lobound $ Apply IType "foo" [Var IType "bar_0", litI 0])
          , "a" .-> typeSB VecType
                    [Apply IType "foo" [Var IType "bar_1", Var IType "n"]]
                    (bounds (Apply RType "foo" [Var IType "bar_2", litR 1.0])
                            (Apply RType "foo" [Var IType "bar_3", litR 10.0]))
          ]
    let body =
          Apply TSDType "somefun" [Var IType "n", Var VecType "a"]
    let body1 =
          Apply TSDType "foo" [Var IType "bar_4", body]
    let fd = FctDef
             { defName = "fname"
             , defParms = parms
             , defRetType = TSDType
             , defBody = body
             }
    let fd1 = fd{ defParms = parms1, defBody = body1 }
    it "xforms params and body" $
      xformFctDef xform fd `shouldBe` fd1

test_simplifyDiag :: Spec
test_simplifyDiag =
  describe "simplifyDiag" $ do
    it "handles diag(diag(scalar * vec), diag(vec(scalar, scalar)))" $ do
      let vec1 = Apply VecType "*" [Var RType "c", Var VecType "v"]
          vec2 = Apply VecType "vec" [Var RType "a", Var RType "b"]
          arg1 = Apply MatType "diag" [ vec1 ]
          arg2 = Apply MatType "diag" [ vec2 ]
          expected = Apply MatType "diag" [Apply VecType "vec" [vec1, vec2]]
      simplifyDiag [arg1, arg2] `shouldBe` expected

regression_test_simplifyVec :: Spec
regression_test_simplifyVec =
  describe "simplifyVec (regr)" $ do
    it "passes regression tests" $ do
      length regr_cases_vec `shouldBe` length regr_results_vec
      forM_ (zip regr_cases_vec regr_results_vec) $ \(x,y) -> do
        x `shouldBe` y

regression_test_simplifyDiag :: Spec
regression_test_simplifyDiag =
  describe "simplifyDiag (regr)" $ do
    it "passes regression tests" $ do
      length regr_cases_diag `shouldBe` length regr_results_diag
      forM_ (zip regr_cases_diag regr_results_diag) $ \(x,y) -> do
        x `shouldBe` y

regr_cases_vec :: [ExprT]
regr_cases_vec =
  [ simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0)],Apply VecType "vec" []]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0)]]
  , simplifyVec [Var RType "mu_a",Apply VecType "vec" []]
  , simplifyVec [Var RType "mu_a"]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0)],Apply VecType "vec" [],Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0)],Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]]
  , simplifyVec [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]]
  , simplifyVec [Apply RType "square" [Var RType "sigma_q"],Apply VecType "*" [Var RType "rhoSqr_0",Var VecType "csigma2s_0"]]
  , simplifyVec [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_q"]],Apply VecType "*" [Var RType "rhoSqr_0",Var VecType "csigma2s_0"]]
  , simplifyVec [Apply VecType "vec" [Var RType "mu_a"],Apply VecType "vec" [],Apply VecType "vec" [Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0)]]
  , simplifyVec [Apply VecType "vec" [Var RType "mu_a"],Apply VecType "vec" [Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0)]]
  , simplifyVec [Var RType "mu_a",Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0)]
  , simplifyVec [Apply VecType "vec" [Var RType "mu_a",Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0)]]
  , simplifyVec [Apply RType "square" [Var RType "sigma_a"],Var VecType "csigma2s_0"]
  , simplifyVec [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"]],Var VecType "csigma2s_0"]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0)],Apply VecType "vec" [Lit RType (RealVal 1.0)]]
  , simplifyVec [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0)]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0)]]
  , simplifyVec [Var RType "phi",Lit RType (RealVal 1.0)]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 0.0)],Apply VecType "vec" [Var RType "mu"]]
  , simplifyVec [Lit RType (RealVal 0.0),Var RType "mu"]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 0.0),Var RType "mu"]]
  , simplifyVec [Apply RType "square" [Var RType "sigma"],Apply RType "square" [Apply RType "*" [Lit RType (RealVal 1.0e-2),Var RType "sigma"]]]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0)],Apply VecType "vec" []]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0)]]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 0.0),Var RType "mu"],Apply VecType "vec" []]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 0.0),Var RType "mu"]]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0)],Apply VecType "vec" [Lit RType (RealVal 1.0)]]
  , simplifyVec [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0)]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0)]]
  , simplifyVec [Var RType "phi",Lit RType (RealVal 1.0)]
  , simplifyVec [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma_a"]],Lit RType (RealVal 0.0)]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 0.0)],Apply VecType "vec" [Var RType "mu"]]
  , simplifyVec [Lit RType (RealVal 0.0),Var RType "mu"]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 0.0),Var RType "mu"]]
  , simplifyVec [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigmamu_a"]]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0)],Apply VecType "vec" []]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0)]]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 0.0),Var RType "mu"],Apply VecType "vec" []]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 0.0),Var RType "mu"]]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0)],Apply VecType "vec" []]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0)]]
  , simplifyVec [Var RType "mu_a",Apply VecType "vec" []]
  , simplifyVec [Var RType "mu_a"]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0)],Apply VecType "vec" []]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0)]]
  , simplifyVec [Apply VecType "vec" [Var RType "mu_a"],Apply VecType "vec" []]
  , simplifyVec [Apply VecType "vec" [Var RType "mu_a"]]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0)],Apply VecType "vec" [Lit RType (RealVal 0.0)]]
  , simplifyVec [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]]
  , simplifyVec [Lit RType (RealVal 0.0),Apply RType "square" [Apply RType "*" [Var RType "rho_d",Var RType "sigma_d"]]]
  , simplifyVec [Var RType "mu_a",Apply VecType "vec" [Lit RType (RealVal 0.0)]]
  , simplifyVec [Apply VecType "vec" [Var RType "mu_a"],Apply VecType "vec" [Lit RType (RealVal 0.0)]]
  , simplifyVec [Var RType "mu_a",Lit RType (RealVal 0.0)]
  , simplifyVec [Apply VecType "vec" [Var RType "mu_a",Lit RType (RealVal 0.0)]]
  , simplifyVec [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"]]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)],Apply VecType "vec" [Lit RType (RealVal 1.0)],Apply VecType "vec" []]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)],Apply VecType "vec" [Lit RType (RealVal 1.0)]]
  , simplifyVec [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0)]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0)]]
  , simplifyVec [Lit RType (RealVal 0.0),Apply RType "square" [Apply RType "*" [Var RType "rho_d",Var RType "sigma_d"]],Apply RType "square" [Apply RType "*" [Var RType "rho_b",Var RType "sigma_b"]]]
  , simplifyVec [Apply VecType "vec" [Var RType "mu_a",Lit RType (RealVal 0.0)],Apply VecType "vec" [Lit RType (RealVal 0.0)],Apply VecType "vec" []]
  , simplifyVec [Apply VecType "vec" [Var RType "mu_a",Lit RType (RealVal 0.0)],Apply VecType "vec" [Lit RType (RealVal 0.0)]]
  , simplifyVec [Var RType "mu_a",Lit RType (RealVal 0.0),Lit RType (RealVal 0.0)]
  , simplifyVec [Apply VecType "vec" [Var RType "mu_a",Lit RType (RealVal 0.0),Lit RType (RealVal 0.0)]]
  , simplifyVec [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"],Apply RType "square" [Var RType "sigma_b"]]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0)],Apply VecType "vec" []]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0)]]
  , simplifyVec [Var RType "mu_d",Apply VecType "vec" []]
  , simplifyVec [Var RType "mu_d"]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0)],Apply VecType "vec" []]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0)]]
  , simplifyVec [Apply VecType "vec" [Var RType "mu_d"],Apply VecType "vec" []]
  , simplifyVec [Apply VecType "vec" [Var RType "mu_d"]]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0)],Apply VecType "vec" [Lit RType (RealVal 0.0)]]
  , simplifyVec [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]]
  , simplifyVec [Apply RType "square" [Var RType "sigma_qa"],Apply RType "square" [Var RType "sigma_qd"]]
  , simplifyVec [Var RType "mu_a",Apply VecType "vec" [Var RType "mu_d"]]
  , simplifyVec [Apply VecType "vec" [Var RType "mu_a"],Apply VecType "vec" [Var RType "mu_d"]]
  , simplifyVec [Var RType "mu_a",Var RType "mu_d"]
  , simplifyVec [Apply VecType "vec" [Var RType "mu_a",Var RType "mu_d"]]
  , simplifyVec [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"]]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)],Apply VecType "vec" []]
  , simplifyVec [Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]]
  , simplifyVec [Apply VecType "vec" [Var RType "mu_a",Var RType "mu_d"],Apply VecType "vec" []]
  , simplifyVec [Apply VecType "vec" [Var RType "mu_a",Var RType "mu_d"]]
  , simplifyVec [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma"]],Lit RType (RealVal 0.0)]
  ]

regr_results_vec :: [ExprT]
regr_results_vec =
  [ Apply VecType "vec" [Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Var RType "mu_a"]
  , Apply VecType "vec" [Var RType "mu_a"]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_q"]],Apply VecType "*" [Var RType "rhoSqr_0",Var VecType "csigma2s_0"]]
  , Apply VecType "vec" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_q"]],Apply VecType "*" [Var RType "rhoSqr_0",Var VecType "csigma2s_0"]]
  , Apply VecType "vec" [Var RType "mu_a",Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Var RType "mu_a",Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Var RType "mu_a",Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Var RType "mu_a",Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"]],Var VecType "csigma2s_0"]
  , Apply VecType "vec" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"]],Var VecType "csigma2s_0"]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Var RType "phi",Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Lit RType (RealVal 0.0),Var RType "mu"]
  , Apply VecType "vec" [Lit RType (RealVal 0.0),Var RType "mu"]
  , Apply VecType "vec" [Lit RType (RealVal 0.0),Var RType "mu"]
  , Apply VecType "vec" [Apply RType "square" [Var RType "sigma"],Apply RType "square" [Apply RType "*" [Lit RType (RealVal 1.0e-2),Var RType "sigma"]]]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Lit RType (RealVal 0.0),Var RType "mu"]
  , Apply VecType "vec" [Lit RType (RealVal 0.0),Var RType "mu"]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Var RType "phi",Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma_a"]],Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Lit RType (RealVal 0.0),Var RType "mu"]
  , Apply VecType "vec" [Lit RType (RealVal 0.0),Var RType "mu"]
  , Apply VecType "vec" [Lit RType (RealVal 0.0),Var RType "mu"]
  , Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigmamu_a"]]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Lit RType (RealVal 0.0),Var RType "mu"]
  , Apply VecType "vec" [Lit RType (RealVal 0.0),Var RType "mu"]
  , Apply VecType "vec" [Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Var RType "mu_a"]
  , Apply VecType "vec" [Var RType "mu_a"]
  , Apply VecType "vec" [Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Var RType "mu_a"]
  , Apply VecType "vec" [Var RType "mu_a"]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Lit RType (RealVal 0.0),Apply RType "square" [Apply RType "*" [Var RType "rho_d",Var RType "sigma_d"]]]
  , Apply VecType "vec" [Var RType "mu_a",Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Var RType "mu_a",Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Var RType "mu_a",Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Var RType "mu_a",Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"]]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Lit RType (RealVal 0.0),Apply RType "square" [Apply RType "*" [Var RType "rho_d",Var RType "sigma_d"]],Apply RType "square" [Apply RType "*" [Var RType "rho_b",Var RType "sigma_b"]]]
  , Apply VecType "vec" [Var RType "mu_a",Lit RType (RealVal 0.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Var RType "mu_a",Lit RType (RealVal 0.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Var RType "mu_a",Lit RType (RealVal 0.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Var RType "mu_a",Lit RType (RealVal 0.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"],Apply RType "square" [Var RType "sigma_b"]]
  , Apply VecType "vec" [Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Var RType "mu_d"]
  , Apply VecType "vec" [Var RType "mu_d"]
  , Apply VecType "vec" [Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0)]
  , Apply VecType "vec" [Var RType "mu_d"]
  , Apply VecType "vec" [Var RType "mu_d"]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Apply RType "square" [Var RType "sigma_qa"],Apply RType "square" [Var RType "sigma_qd"]]
  , Apply VecType "vec" [Var RType "mu_a",Var RType "mu_d"]
  , Apply VecType "vec" [Var RType "mu_a",Var RType "mu_d"]
  , Apply VecType "vec" [Var RType "mu_a",Var RType "mu_d"]
  , Apply VecType "vec" [Var RType "mu_a",Var RType "mu_d"]
  , Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"]]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Lit RType (RealVal 1.0),Lit RType (RealVal 0.0)]
  , Apply VecType "vec" [Var RType "mu_a",Var RType "mu_d"]
  , Apply VecType "vec" [Var RType "mu_a",Var RType "mu_d"]
  , Apply VecType "vec" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma"]],Lit RType (RealVal 0.0)]
  ]

regr_cases_diag :: [ExprT]
regr_cases_diag =
  [ simplifyDiag [Apply RType "square" [Var RType "sigma_q"],Apply MatType "diag" []]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_q"]],Apply MatType "diag" []]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_q"]]]
  , simplifyDiag [Apply RType "square" [Var RType "sigma_a"],Apply MatType "diag" []]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"]],Apply MatType "diag" []]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"]]]
  , simplifyDiag [Apply MatType "mat11" [Lit RType (RealVal 1.0)],Apply MatType "diag" [],Apply MatType "diag" [Apply (RArrType 3) "*" [Apply RType "sqrt" [Apply RType "-" [Lit RType (RealVal 1.0),Var RType "rhoSqr_0"]],Apply (RArrType 3) "{}" [Apply MatType "mat22" [Lit RType (RealVal 0.6234898018587336),Lit RType (RealVal (-0.7818314824680297)),Lit RType (RealVal 0.7818314824680297),Lit RType (RealVal 0.6234898018587336)],Apply MatType "mat22" [Lit RType (RealVal (-0.22252093395631434)),Lit RType (RealVal (-0.9749279121818236)),Lit RType (RealVal 0.9749279121818236),Lit RType (RealVal (-0.22252093395631434))],Apply MatType "mat22" [Lit RType (RealVal (-0.900968867902419)),Lit RType (RealVal (-0.43388373911755823)),Lit RType (RealVal 0.43388373911755823),Lit RType (RealVal (-0.900968867902419))]]]]]
  , simplifyDiag [Apply MatType "mat11" [Lit RType (RealVal 1.0)],Apply MatType "diag" [Apply (RArrType 3) "*" [Apply RType "sqrt" [Apply RType "-" [Lit RType (RealVal 1.0),Var RType "rhoSqr_0"]],Apply (RArrType 3) "{}" [Apply MatType "mat22" [Lit RType (RealVal 0.6234898018587336),Lit RType (RealVal (-0.7818314824680297)),Lit RType (RealVal 0.7818314824680297),Lit RType (RealVal 0.6234898018587336)],Apply MatType "mat22" [Lit RType (RealVal (-0.22252093395631434)),Lit RType (RealVal (-0.9749279121818236)),Lit RType (RealVal 0.9749279121818236),Lit RType (RealVal (-0.22252093395631434))],Apply MatType "mat22" [Lit RType (RealVal (-0.900968867902419)),Lit RType (RealVal (-0.43388373911755823)),Lit RType (RealVal 0.43388373911755823),Lit RType (RealVal (-0.900968867902419))]]]]]
  , simplifyDiag [Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_q"]],Apply MatType "diag" [],Apply MatType "diag" [Apply VecType "*" [Var RType "rhoSqr_0",Var VecType "csigma2s_0"]]]
  , simplifyDiag [Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_q"]],Apply MatType "diag" [Apply VecType "*" [Var RType "rhoSqr_0",Var VecType "csigma2s_0"]]]
  , simplifyDiag [Apply VecType "vec" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_q"]],Apply VecType "*" [Var RType "rhoSqr_0",Var VecType "csigma2s_0"]]]
  , simplifyDiag [Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_a"]],Apply MatType "diag" [],Apply MatType "diag" [Var VecType "csigma2s_0"]]
  , simplifyDiag [Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_a"]],Apply MatType "diag" [Var VecType "csigma2s_0"]]
  , simplifyDiag [Apply VecType "vec" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"]],Var VecType "csigma2s_0"]]
  , simplifyDiag [Var RType "phi"]
  , simplifyDiag [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma"]]]
  , simplifyDiag [Apply RType "square" [Var RType "sigma"]]
  , simplifyDiag [Lit RType (RealVal 1.0)]
  , simplifyDiag [Lit RType (RealVal 0.0)]
  , simplifyDiag [Apply RType "square" [Apply RType "*" [Lit RType (RealVal 1.0e-2),Var RType "sigma"]]]
  , simplifyDiag [Apply MatType "mat11" [Var RType "phi"],Apply MatType "mat11" [Lit RType (RealVal 1.0)]]
  , simplifyDiag [Apply VecType "vec" [Var RType "phi",Lit RType (RealVal 1.0)]]
  , simplifyDiag [Apply MatType "mat11" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma"]]],Apply MatType "mat11" [Lit RType (RealVal 0.0)]]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma"]],Lit RType (RealVal 0.0)]]
  , simplifyDiag [Apply MatType "mat11" [Apply RType "square" [Var RType "sigma"]],Apply MatType "mat11" [Apply RType "square" [Apply RType "*" [Lit RType (RealVal 1.0e-2),Var RType "sigma"]]]]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma"],Apply RType "square" [Apply RType "*" [Lit RType (RealVal 1.0e-2),Var RType "sigma"]]]]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Var RType "phi",Lit RType (RealVal 1.0)]],Apply MatType "diag" []]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Var RType "phi",Lit RType (RealVal 1.0)]]]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma"]],Lit RType (RealVal 0.0)]],Apply MatType "diag" []]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma"]],Lit RType (RealVal 0.0)]]]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma"],Apply RType "square" [Apply RType "*" [Lit RType (RealVal 1.0e-2),Var RType "sigma"]]]],Apply MatType "diag" []]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma"],Apply RType "square" [Apply RType "*" [Lit RType (RealVal 1.0e-2),Var RType "sigma"]]]]]
  , simplifyDiag [Var RType "phi"]
  , simplifyDiag [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma_a"]]]
  , simplifyDiag [Apply RType "square" [Var RType "sigma_a"]]
  , simplifyDiag [Lit RType (RealVal 1.0)]
  , simplifyDiag [Lit RType (RealVal 0.0)]
  , simplifyDiag [Apply RType "square" [Var RType "sigmamu_a"]]
  , simplifyDiag [Apply MatType "mat11" [Var RType "phi"],Apply MatType "mat11" [Lit RType (RealVal 1.0)]]
  , simplifyDiag [Apply VecType "vec" [Var RType "phi",Lit RType (RealVal 1.0)]]
  , simplifyDiag [Apply MatType "mat11" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma_a"]]],Apply MatType "mat11" [Lit RType (RealVal 0.0)]]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma_a"]],Lit RType (RealVal 0.0)]]
  , simplifyDiag [Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_a"]],Apply MatType "mat11" [Apply RType "square" [Var RType "sigmamu_a"]]]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigmamu_a"]]]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Var RType "phi",Lit RType (RealVal 1.0)]],Apply MatType "diag" []]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Var RType "phi",Lit RType (RealVal 1.0)]]]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma_a"]],Lit RType (RealVal 0.0)]],Apply MatType "diag" []]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma_a"]],Lit RType (RealVal 0.0)]]]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigmamu_a"]]],Apply MatType "diag" []]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigmamu_a"]]]]
  , simplifyDiag [Apply RType "square" [Var RType "sigma_q"],Apply MatType "diag" []]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_q"]],Apply MatType "diag" []]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_q"]]]
  , simplifyDiag [Apply RType "square" [Var RType "sigma_a"],Apply MatType "diag" []]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"]],Apply MatType "diag" []]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"]]]
  , simplifyDiag [Apply MatType "mat11" [Lit RType (RealVal 1.0)],Apply MatType "diag" []]
  , simplifyDiag [Apply MatType "mat11" [Lit RType (RealVal 1.0)]]
  , simplifyDiag [Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_q"]],Apply MatType "diag" []]
  , simplifyDiag [Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_q"]]]
  , simplifyDiag [Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_a"]],Apply MatType "diag" []]
  , simplifyDiag [Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_a"]]]
  , simplifyDiag [Var RType "phi_d"]
  , simplifyDiag [Apply RType "square" [Apply RType "*" [Var RType "rho_d",Var RType "sigma_d"]]]
  , simplifyDiag [Apply RType "square" [Var RType "sigma_d"]]
  , simplifyDiag [Lit RType (RealVal 0.0),Apply MatType "mat11" [Apply RType "square" [Apply RType "*" [Var RType "rho_d",Var RType "sigma_d"]]]]
  , simplifyDiag [Apply VecType "vec" [Lit RType (RealVal 0.0)],Apply MatType "mat11" [Apply RType "square" [Apply RType "*" [Var RType "rho_d",Var RType "sigma_d"]]]]
  , simplifyDiag [Apply VecType "vec" [Lit RType (RealVal 0.0),Apply RType "square" [Apply RType "*" [Var RType "rho_d",Var RType "sigma_d"]]]]
  , simplifyDiag [Apply RType "square" [Var RType "sigma_a"],Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_d"]]]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"]],Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_d"]]]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"]]]
  , simplifyDiag [Var RType "phi_b"]
  , simplifyDiag [Apply RType "square" [Apply RType "*" [Var RType "rho_b",Var RType "sigma_b"]]]
  , simplifyDiag [Apply RType "square" [Var RType "sigma_b"]]
  , simplifyDiag [Apply MatType "mat22" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Var RType "phi_d"],Apply MatType "mat11" [Var RType "phi_b"],Apply MatType "diag" []]
  , simplifyDiag [Apply MatType "mat22" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Var RType "phi_d"],Apply MatType "mat11" [Var RType "phi_b"]]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Lit RType (RealVal 0.0),Apply RType "square" [Apply RType "*" [Var RType "rho_d",Var RType "sigma_d"]]]],Apply MatType "mat11" [Apply RType "square" [Apply RType "*" [Var RType "rho_b",Var RType "sigma_b"]]],Apply MatType "diag" []]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Lit RType (RealVal 0.0),Apply RType "square" [Apply RType "*" [Var RType "rho_d",Var RType "sigma_d"]]]],Apply MatType "mat11" [Apply RType "square" [Apply RType "*" [Var RType "rho_b",Var RType "sigma_b"]]]]
  , simplifyDiag [Apply VecType "vec" [Lit RType (RealVal 0.0),Apply RType "square" [Apply RType "*" [Var RType "rho_d",Var RType "sigma_d"]],Apply RType "square" [Apply RType "*" [Var RType "rho_b",Var RType "sigma_b"]]]]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"]]],Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_b"]],Apply MatType "diag" []]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"]]],Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_b"]]]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"],Apply RType "square" [Var RType "sigma_b"]]]
  , simplifyDiag [Apply RType "square" [Var RType "sigma_qd"],Apply MatType "diag" []]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_qd"]],Apply MatType "diag" []]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_qd"]]]
  , simplifyDiag [Apply RType "square" [Var RType "sigma_d"],Apply MatType "diag" []]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_d"]],Apply MatType "diag" []]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_d"]]]
  , simplifyDiag [Apply MatType "mat11" [Lit RType (RealVal 1.0)],Apply MatType "diag" []]
  , simplifyDiag [Apply MatType "mat11" [Lit RType (RealVal 1.0)]]
  , simplifyDiag [Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_qd"]],Apply MatType "diag" []]
  , simplifyDiag [Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_qd"]]]
  , simplifyDiag [Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_d"]],Apply MatType "diag" []]
  , simplifyDiag [Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_d"]]]
  , simplifyDiag [Apply RType "square" [Var RType "sigma_qa"],Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_qd"]]]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_qa"]],Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_qd"]]]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_qa"],Apply RType "square" [Var RType "sigma_qd"]]]
  , simplifyDiag [Apply RType "square" [Var RType "sigma_a"],Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_d"]]]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"]],Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_d"]]]
  , simplifyDiag [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"]]]
  , simplifyDiag [Apply MatType "mat22" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0)],Apply MatType "diag" []]
  , simplifyDiag [Apply MatType "mat22" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0)]]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_qa"],Apply RType "square" [Var RType "sigma_qd"]]],Apply MatType "diag" []]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_qa"],Apply RType "square" [Var RType "sigma_qd"]]]]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"]]],Apply MatType "diag" []]
  , simplifyDiag [Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"]]]]
  ]

regr_results_diag :: [ExprT]
regr_results_diag =
  [ Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_q"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_q"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_q"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_a"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_a"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_a"]]
  , Apply MatType "diag" [Apply MatType "mat11" [Lit RType (RealVal 1.0)],Apply MatType "diag" [Apply (RArrType 3) "*" [Apply RType "sqrt" [Apply RType "-" [Lit RType (RealVal 1.0),Var RType "rhoSqr_0"]],Apply (RArrType 3) "{}" [Apply MatType "mat22" [Lit RType (RealVal 0.6234898018587336),Lit RType (RealVal (-0.7818314824680297)),Lit RType (RealVal 0.7818314824680297),Lit RType (RealVal 0.6234898018587336)],Apply MatType "mat22" [Lit RType (RealVal (-0.22252093395631434)),Lit RType (RealVal (-0.9749279121818236)),Lit RType (RealVal 0.9749279121818236),Lit RType (RealVal (-0.22252093395631434))],Apply MatType "mat22" [Lit RType (RealVal (-0.900968867902419)),Lit RType (RealVal (-0.43388373911755823)),Lit RType (RealVal 0.43388373911755823),Lit RType (RealVal (-0.900968867902419))]]]]]
  , Apply MatType "diag" [Apply MatType "mat11" [Lit RType (RealVal 1.0)],Apply MatType "diag" [Apply (RArrType 3) "*" [Apply RType "sqrt" [Apply RType "-" [Lit RType (RealVal 1.0),Var RType "rhoSqr_0"]],Apply (RArrType 3) "{}" [Apply MatType "mat22" [Lit RType (RealVal 0.6234898018587336),Lit RType (RealVal (-0.7818314824680297)),Lit RType (RealVal 0.7818314824680297),Lit RType (RealVal 0.6234898018587336)],Apply MatType "mat22" [Lit RType (RealVal (-0.22252093395631434)),Lit RType (RealVal (-0.9749279121818236)),Lit RType (RealVal 0.9749279121818236),Lit RType (RealVal (-0.22252093395631434))],Apply MatType "mat22" [Lit RType (RealVal (-0.900968867902419)),Lit RType (RealVal (-0.43388373911755823)),Lit RType (RealVal 0.43388373911755823),Lit RType (RealVal (-0.900968867902419))]]]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_q"]],Apply VecType "*" [Var RType "rhoSqr_0",Var VecType "csigma2s_0"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_q"]],Apply VecType "*" [Var RType "rhoSqr_0",Var VecType "csigma2s_0"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_q"]],Apply VecType "*" [Var RType "rhoSqr_0",Var VecType "csigma2s_0"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"]],Var VecType "csigma2s_0"]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"]],Var VecType "csigma2s_0"]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"]],Var VecType "csigma2s_0"]]
  , Apply MatType "mat11" [Var RType "phi"]
  , Apply MatType "mat11" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma"]]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma"]]
  , Apply MatType "mat11" [Lit RType (RealVal 1.0)]
  , Apply MatType "mat11" [Lit RType (RealVal 0.0)]
  , Apply MatType "mat11" [Apply RType "square" [Apply RType "*" [Lit RType (RealVal 1.0e-2),Var RType "sigma"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Var RType "phi",Lit RType (RealVal 1.0)]]
  , Apply MatType "diag" [Apply VecType "vec" [Var RType "phi",Lit RType (RealVal 1.0)]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma"]],Lit RType (RealVal 0.0)]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma"]],Lit RType (RealVal 0.0)]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma"],Apply RType "square" [Apply RType "*" [Lit RType (RealVal 1.0e-2),Var RType "sigma"]]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma"],Apply RType "square" [Apply RType "*" [Lit RType (RealVal 1.0e-2),Var RType "sigma"]]]]
  , Apply MatType "diag" [Apply VecType "vec" [Var RType "phi",Lit RType (RealVal 1.0)]]
  , Apply MatType "diag" [Apply VecType "vec" [Var RType "phi",Lit RType (RealVal 1.0)]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma"]],Lit RType (RealVal 0.0)]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma"]],Lit RType (RealVal 0.0)]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma"],Apply RType "square" [Apply RType "*" [Lit RType (RealVal 1.0e-2),Var RType "sigma"]]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma"],Apply RType "square" [Apply RType "*" [Lit RType (RealVal 1.0e-2),Var RType "sigma"]]]]
  , Apply MatType "mat11" [Var RType "phi"]
  , Apply MatType "mat11" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma_a"]]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_a"]]
  , Apply MatType "mat11" [Lit RType (RealVal 1.0)]
  , Apply MatType "mat11" [Lit RType (RealVal 0.0)]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigmamu_a"]]
  , Apply MatType "diag" [Apply VecType "vec" [Var RType "phi",Lit RType (RealVal 1.0)]]
  , Apply MatType "diag" [Apply VecType "vec" [Var RType "phi",Lit RType (RealVal 1.0)]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma_a"]],Lit RType (RealVal 0.0)]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma_a"]],Lit RType (RealVal 0.0)]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigmamu_a"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigmamu_a"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Var RType "phi",Lit RType (RealVal 1.0)]]
  , Apply MatType "diag" [Apply VecType "vec" [Var RType "phi",Lit RType (RealVal 1.0)]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma_a"]],Lit RType (RealVal 0.0)]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Apply RType "*" [Var RType "rho",Var RType "sigma_a"]],Lit RType (RealVal 0.0)]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigmamu_a"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigmamu_a"]]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_q"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_q"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_q"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_a"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_a"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_a"]]
  , Apply MatType "mat11" [Lit RType (RealVal 1.0)]
  , Apply MatType "mat11" [Lit RType (RealVal 1.0)]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_q"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_q"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_a"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_a"]]
  , Apply MatType "mat11" [Var RType "phi_d"]
  , Apply MatType "mat11" [Apply RType "square" [Apply RType "*" [Var RType "rho_d",Var RType "sigma_d"]]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_d"]]
  , Apply MatType "diag" [Apply VecType "vec" [Lit RType (RealVal 0.0),Apply RType "square" [Apply RType "*" [Var RType "rho_d",Var RType "sigma_d"]]]]
  , Apply MatType "diag" [Apply VecType "vec" [Lit RType (RealVal 0.0),Apply RType "square" [Apply RType "*" [Var RType "rho_d",Var RType "sigma_d"]]]]
  , Apply MatType "diag" [Apply VecType "vec" [Lit RType (RealVal 0.0),Apply RType "square" [Apply RType "*" [Var RType "rho_d",Var RType "sigma_d"]]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"]]]
  , Apply MatType "mat11" [Var RType "phi_b"]
  , Apply MatType "mat11" [Apply RType "square" [Apply RType "*" [Var RType "rho_b",Var RType "sigma_b"]]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_b"]]
  , Apply MatType "diag" [Apply MatType "mat22" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Var RType "phi_d"],Apply MatType "mat11" [Var RType "phi_b"]]
  , Apply MatType "diag" [Apply MatType "mat22" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Var RType "phi_d"],Apply MatType "mat11" [Var RType "phi_b"]]
  , Apply MatType "diag" [Apply VecType "vec" [Lit RType (RealVal 0.0),Apply RType "square" [Apply RType "*" [Var RType "rho_d",Var RType "sigma_d"]],Apply RType "square" [Apply RType "*" [Var RType "rho_b",Var RType "sigma_b"]]]]
  , Apply MatType "diag" [Apply VecType "vec" [Lit RType (RealVal 0.0),Apply RType "square" [Apply RType "*" [Var RType "rho_d",Var RType "sigma_d"]],Apply RType "square" [Apply RType "*" [Var RType "rho_b",Var RType "sigma_b"]]]]
  , Apply MatType "diag" [Apply VecType "vec" [Lit RType (RealVal 0.0),Apply RType "square" [Apply RType "*" [Var RType "rho_d",Var RType "sigma_d"]],Apply RType "square" [Apply RType "*" [Var RType "rho_b",Var RType "sigma_b"]]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"],Apply RType "square" [Var RType "sigma_b"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"],Apply RType "square" [Var RType "sigma_b"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"],Apply RType "square" [Var RType "sigma_b"]]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_qd"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_qd"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_qd"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_d"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_d"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_d"]]
  , Apply MatType "mat11" [Lit RType (RealVal 1.0)]
  , Apply MatType "mat11" [Lit RType (RealVal 1.0)]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_qd"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_qd"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_d"]]
  , Apply MatType "mat11" [Apply RType "square" [Var RType "sigma_d"]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_qa"],Apply RType "square" [Var RType "sigma_qd"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_qa"],Apply RType "square" [Var RType "sigma_qd"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_qa"],Apply RType "square" [Var RType "sigma_qd"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"]]]
  , Apply MatType "mat22" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0)]
  , Apply MatType "mat22" [Lit RType (RealVal 1.0),Lit RType (RealVal 1.0),Lit RType (RealVal 0.0),Lit RType (RealVal 1.0)]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_qa"],Apply RType "square" [Var RType "sigma_qd"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_qa"],Apply RType "square" [Var RType "sigma_qd"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"]]]
  , Apply MatType "diag" [Apply VecType "vec" [Apply RType "square" [Var RType "sigma_a"],Apply RType "square" [Var RType "sigma_d"]]]
  ]
