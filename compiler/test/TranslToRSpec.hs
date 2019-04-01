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
module TranslToRSpec (main, spec) where

import Test.Hspec

import AST
import RAST
import TranslToR

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  test_translateSimpleExprToR
  test_translateExprToR
  test_ssmCreationFct1

test_translateSimpleExprToR :: Spec
test_translateSimpleExprToR =
  describe "translate simple expressions" $ do
    let transl = translateSimpleExprToR
    it "translates empty vector" $
      transl (Apply VecType "vec" []) `shouldBe` RApply "numeric" [rlitI 0]
    it "translates some random expressions" $ do
      transl (Apply VecType "vec" [litR 1.0]) `shouldBe`
        RApply "c" [rlitR 1.0]
      transl (Apply RType "*" [Var RType "x", Var RType "s"]) `shouldBe`
        RApply "*" [RVar "x", RVar "s"]
      transl (Apply MatType "mat11" [Apply RType "square" [Var RType "h"]])
        `shouldBe` RApply "as.matrix" [RApply "^" [RVar "h", rlitI 2]]
      transl (Apply RType "sqrt" [Apply RType "-" [litR 1.0, Var RType "x"]])
        `shouldBe` RApply "sqrt" [RApply "-" [rlitR 1.0, RVar "x"]]
      transl (Apply MatType "diag"
              [Apply VecType "vec"
               [Apply RType "+" [Var RType "x", Var RType "y"]]]) `shouldBe`
        RApply "chronikis::diagv" [RApply "c" [RApply "+" [RVar "x", RVar "y"]]]

test_translateExprToR :: Spec
test_translateExprToR =
  describe "translate unrolled expressions" $ do
  it "passes regression test" $ do
    let e = Let RType "sigma_h"
              (Apply RType "*" [Var RType "raw_0", Var RType "sigma_h_scale"])
            $
            Let RType "phi_b"
              (Apply RType "sqrt"
               [Apply RType "-"
                [litR 1.0, Apply RType "square" [Var RType "rho_b"]]])
            $
            Let RType "sigma_b"
              (Apply RType "*" [Var RType "raw_1", Var RType "sigma_b_scale"])
            $
            Let RType "phi_d"
              (Apply RType "sqrt"
               [Apply RType "-"
                [litR 1.0, Apply RType "square" [Var RType "rho_d"]]])
            $
            Let VecType "Z"
              (Apply VecType "vec" [litR 1.0, litR 0.0, litR 1.0])
            $
            Let RType "H"
              (Apply RType "square" [Var RType "sigma_h"])
            $
            Let MatType "T"
             (Apply MatType "diag"
              [ Apply MatType "mat22"
                [litR 1.0, litR 1.0, litR 0.0, Var RType "phi_d"]
              , Apply MatType "mat11" [Var RType "phi_b"]
              ])
            $
            Let MatType "Q"
             (Apply MatType "diag"
              [Apply VecType "vec"
               [ litR 0.0
               , Apply RType "square"
                 [Apply RType "*" [Var RType "rho_d", Var RType "sigma_d"]]
               , Apply RType "square"
                 [Apply RType "*" [Var RType "rho_b", Var RType "sigma_b"]]
               ]])
            $
            Let VecType "a0"
             (Apply VecType "vec" [Var RType "mu_a", litR 0.0, litR 0.0])
            $
            Let MatType "P0"
             (Apply MatType "diag"
              [Apply VecType "vec"
               [ Apply RType "square" [Var RType "sigma_a"]
               , Apply RType "square" [Var RType "sigma_d"]
               , Apply RType "square" [Var RType "sigma_b"]
               ]])
            $
            Apply TSDType "ssm"
            [Var VecType "Z", Var RType "H", Var MatType "T", Var MatType "Q",
             Var VecType "a0", Var MatType "P0"]
    translateExprToR e `shouldBe`
      ( [ RLet "sigma_h" (RApply "*" [RVar "raw_0", RVar "sigma_h_scale"])
        , RLet "phi_b"
          (RApply "sqrt" [RApply "-"
                          [rlitR 1.0, RApply "^" [RVar "rho_b", rlitI 2]]])
        , RLet "sigma_b"
          (RApply "*" [RVar "raw_1", RVar "sigma_b_scale"])
        , RLet "phi_d"
          (RApply "sqrt" [RApply "-"
                          [rlitR 1.0, RApply "^" [RVar "rho_d", rlitI 2]]])
        , RLet "Z"
          (RApply "c" [rlitR 1.0, rlitR 0.0, rlitR 1.0])
        , RLet "H"
          (RApply "^" [RVar "sigma_h", rlitI 2])
        , RLet "T"
          (RApply "dlm::bdiag"
           [ RApply' "matrix"
               [RApply "c" [rlitR 1.0, rlitR 1.0, rlitR 0.0, RVar "phi_d"]]
               [("nrow", rlitI 2)]
           , RApply "as.matrix" [RVar "phi_b"]
           ])
        , RLet "Q"
          (RApply "chronikis::diagv"
           [RApply "c"
            [ rlitR 0.0
            , RApply "^" [RApply "*" [RVar "rho_d", RVar "sigma_d"], rlitI 2]
            , RApply "^" [RApply "*" [RVar "rho_b", RVar "sigma_b"], rlitI 2]
            ]])
        , RLet "a0"
          (RApply "c" [RVar "mu_a", rlitR 0.0, rlitR 0.0])
        , RLet "P0"
          (RApply "chronikis::diagv"
           [RApply "c"
            [ RApply "^" [RVar "sigma_a", rlitI 2]
            , RApply "^" [RVar "sigma_d", rlitI 2]
            , RApply "^" [RVar "sigma_b", rlitI 2]
            ]])
        ]
      , RApply' "dlm::dlm" []
        [ ("FF", RApply' "matrix" [RVar "Z"] [("nrow", rlitI 1)])
        , ("V", RApply "as.matrix" [RVar "H"])
        , ("GG", RVar "T")
        , ("W", RVar "Q")
        , ("m0", RVar "a0")
        , ("C0", RVar "P0")
        ]
      )

test_ssmCreationFct1 :: Spec
test_ssmCreationFct1 =
  describe "ssmCreationFct1" $ do
    it "passes regression test" $ do
      let stmts = [RLet "H" (RApply "^" [RVar "h", rlitI 2])]
          retVal = RApply "foo"
                   [RVar "H", RVar "a", RVar "b",
                    RVar "x", RVar "y", RVar "z"]
          known_vars = ["a", "b"]
          inferred_vars = [("x", 0), ("y", 1), ("z", 2)]
      ssmCreationFct1 (stmts, retVal) known_vars inferred_vars `shouldBe`
        RFctDef
        { rfctParms = ["data", "posterior_draws", "i"]
        , rfctStmts =
          [ RLet "a" (RField (RVar "data") "a_")
          , RLet "b" (RField (RVar "data") "b_")
          , RLet "x" (RIndex True (RField (RVar "posterior_draws") "x")
                                  [Just $ RVar "i"])
          , RLet "y" (RIndex True (RField (RVar "posterior_draws") "y")
                                  [Just $ RVar "i", Nothing])
          , RLet "z" (RIndex True (RField (RVar "posterior_draws") "z")
                                  [Just $ RVar "i", Nothing, Nothing])
          , RLet "H" (RApply "^" [RVar "h", rlitI 2])
          ]
        , rfctReturn = RApply "foo" [RVar "H", RVar "a", RVar "b",
                                     RVar "x", RVar "y", RVar "z"]
        }
        
