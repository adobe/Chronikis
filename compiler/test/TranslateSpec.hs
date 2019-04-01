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
module TranslateSpec (main, spec) where

import Control.Monad
--import Data.Scientific (scientific)
import qualified Data.Set as Set
import Test.Hspec
--import Test.HUnit

import BasicExpr
import BasicModel (TypeS(..), TypeSB(..), Decl(..), DistrExpr(..))
import AST (ElemType(..))
import SSM
import qualified StanAST as S
import StanAST (StanAST(..), StanModelBlock(..))
import TranslateImpl

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  test_translate_type
  test_translate_expr
  test_translate

test_translate_type :: Spec
test_translate_type =
  describe "translate types" $ do
    it "translates TypeS" $ do
      translateTypeS (TypeS RealType []) `shouldBe` S.Type S.RealType []
      translateTypeS (TypeS IntType []) `shouldBe` S.Type S.IntType []
      translateTypeS (TypeS RealType [Var "k"]) `shouldBe`
        S.Type (S.VectorType (Var "k")) []
      translateTypeS (TypeS IntType [Var "k"]) `shouldBe`
        S.Type S.IntType [Var "k"]
      translateTypeS (TypeS RealType [litI 2, Var "n"]) `shouldBe`
        S.Type (S.MatrixType (litI 2) (Var "n")) []
      translateTypeS (TypeS IntType [litI 2, Var "n"]) `shouldBe`
        S.Type S.IntType [litI 2, Var "n"]
      translateTypeS (TypeS RealType [Var "a", litI 5, Var "b"]) `shouldBe`
        S.Type (S.MatrixType (Var "a") (litI 5)) [Var "b"]
      translateTypeS (TypeS IntType [Var "a", litI 5, Var "b"]) `shouldBe`
        S.Type S.IntType [Var "a", litI 5, Var "b"]
    it "translates TypeSB" $ do
      let shapes = [ [], [Var "n"], [litI 2, Var "k"]
                   , [Var "a", litI 3, litI 5] ]
      let bnds = [ nobounds, lobound (litR 0), hibound (litR 1)
                 , bounds (litR 0) (litR 8) ]
      forM_ [IntType, RealType] $ \et->
        forM_ shapes $ \shape->
          forM_ bnds $ \bnd-> do
            let (typb, b) = translateTypeSB $ TypeSB et shape bnd
                typu      = translateTypeS $ TypeS et shape
            typb `shouldBe` typu
            b `shouldBe` bnd

test_translate_expr :: Spec
test_translate_expr =
  describe "translate_expr" $ do
    it "translates simple expressions" $ do
      translateExpr (Lit $ IntVal 3) `shouldBe` (Lit $ IntVal 3)
      translateExpr (Lit $ RealVal 2.5) `shouldBe` (Lit $ RealVal 2.5)
      translateExpr (Var "foo") `shouldBe` (Var "foo")
    it "translates function applications" $ do
      translateExpr (Apply "+" [Var "a", Var "b", Var "c"]) `shouldBe`
        Apply "+" [Apply "+" [Var "a", Var "b"], Var "c"]
      translateExpr (Apply "*" [Var "a", Var "b", Var "c"]) `shouldBe`
        Apply "*" [Apply "*" [Var "a", Var "b"], Var "c"]
      translateExpr (Apply "diag_mats" [Var "m", Var "n", Var "p"]) `shouldBe`
        Apply "block_diag" [Apply "{}" [Var "m", Var "n", Var "p"]]

test_translate :: Spec
test_translate =
  describe "translate" $ do
    let lo0 = Bounds (Just $ litR 0) Nothing
        nobnds = Bounds Nothing Nothing    
    it "translates wn" $ do
      let model =
            Model
              [ Decl "sigma_scale" $ TypeSB RealType [] lo0 ] $
              Draw (Decl "sigma_raw" $ TypeSB RealType [] lo0)
                   (DistrExpr "half_normal" [litR 1]) $
              Let (Decl "sigma" $ TypeS RealType [])
                  (Apply "*" [Var "sigma_scale", Var "sigma_raw"]) $
              Let (Decl "m00" $ TypeS RealType [litI 0, litI 0])
                  (Apply "diag_mats" []) $
              Let (Decl "v0" $ TypeS RealType [litI 0])
                  (Apply "vec_reals" []) $
              TSDistr SSM
              { vecZ = Var "v0"
              , scalH = Apply "square" [Var "sigma"]
              , matT = Var "m00"
              , matQ = Var "m00"
              , veca0 = Apply "vec_reals" []
              , matP0 = Var "m00"
              }
          expected0 =
            StanAST
            { stdata = [("sigma_scale", (S.Type S.RealType [], lo0))]
            , stxdata = [ ( ("m00", (S.Type (S.MatrixType (litI 0) (litI 0)) [],
                                     nobnds))
                          , Apply "rep_matrix" [litR 0, litI 0, litI 0]
                          )
                        , ( ("v0", (S.Type (S.VectorType (litI 0)) [],
                                     nobnds))
                          , Apply "rep_vector" [litR 0, litI 0]
                          )
                        ]
            , stparms = [("sigma_raw", (S.Type S.RealType [], lo0))]
            , stxparms = []
            , stmodel =
              StanModelBlock
              { medefs = [( ("sigma", S.Type S.RealType [])
                          , Apply "*" [Var "sigma_scale", Var "sigma_raw"]
                          )
                         ]
              , medraws =
                [("sigma_raw", S.DistrExpr "normal" [litR 0, litR 1] lo0)]
              , messm = SSM
                { vecZ = Var "v0"
                , scalH = Apply "square" [Var "sigma"]
                , matT = Var "m00"
                , matQ = Var "m00"
                , veca0 = Apply "rep_vector" [litR 0, litI 0]
                , matP0 = Var "m00"
                }
              }
            }
          expected1 =
            expected0
            { stxparms = [( ("sigma", (S.Type S.RealType [], nobnds))
                          , Apply "*" [Var "sigma_scale", Var "sigma_raw"]
                          )
                         ]
            , stmodel = (stmodel expected0){ medefs = [] }
            }
      translate Set.empty model `shouldBe` expected0
      translate (Set.singleton "sigma") model `shouldBe` expected1
    it "handles draw-mediating vars correctly" $ do
      -- Don't take this model seriously.
      let model =
            Model
              [ Decl "prior_scales" $ TypeSB RealType [litI 3] lo0 ] $
              Draw (Decl "sigma_prior_raw" $ TypeSB RealType [] lo0)
                   (DistrExpr "half_normal" [litR 1]) $
              Let (Decl "sigma_prior" $ TypeS RealType [])
                  (Apply "*" [Apply "[]" [Var "prior_scales", litI 1],
                              Var "sigma_prior_raw"]) $
              Let (Decl "q_scale" $ TypeS RealType [])
                  (Apply "[]" [Var "prior_scales", litI 2]) $
              Let (Decl "mu_q" $ TypeS RealType [])
                  (Apply "*" [Var "q_scale", Var "sigma_prior"]) $
              Draw (Decl "sigma_q" $ TypeSB RealType []
                               (bounds (litR 0) (Var "sigma_prior")))
                   (DistrExpr "exponential_rt"
                              [Var "mu_q", Var "sigma_prior"]) $
              Let (Decl "eps_scale" $ TypeS RealType [])
                  (Apply "[]" [Var "prior_scales", litI 3]) $
              Let (Decl "ub_eps" $ TypeS RealType [])
                  (Apply "*" [litR 2, Var "eps_scale", Var "q_scale",
                              Var "sigma_prior"]) $
              Let (Decl "mu_eps" $ TypeS RealType [])
                  (Apply "*" [Var "eps_scale", Var "mu_q"]) $
              Draw (Decl "sigma_eps" $ TypeSB RealType []
                                 (bounds (litR 0) (Var "ub_eps")))
                  (DistrExpr "exponential_rt"
                             [Var "mu_eps", Apply "*" [litR 2, Var "mu_eps"]]) $
              Let (Decl "I1" $ TypeS RealType [litI 1, litI 1])
                  (Apply "mat11" [litR 1]) $
              -- prior_scales : real{0,}[3]
              -- sigma_prior_raw : real{0,} ~ half_normal(1)
              -- sigma_prior : real = prior_scales[1] * sigma_prior_raw
              -- q_scale : real = prior_scales[2]
              -- mu_q : real = q_scale * sigma_prior
              -- sigma_q : real{0,sigma_prior} ~
              --   exponential_mt(mu_q, sigma_prior)
              -- eps_scale : real = prior_scales[3]
              -- ub_eps : real = 2 * eps_scale * q_scale * sigma_prior
              -- mu_eps : real = eps_scale * mu_q
              -- sigma_eps : real{0, ub_eps} ~
              --   exponential_mt(mu_eps, 2 * mu_eps)
              -- I1 : real[1,1] = mat11(1)
              TSDistr SSM
              { vecZ = Var "I1"
              , scalH  = Apply "vec_reals" [Apply "square" [Var "sigma_eps"]]
              , matT  = Var "I1"
              , matQ = Apply "mat11" [Apply "square" [Var "sigma_q"]]
              , veca0 = Apply "vec_reals" [Var "mu_prior"]
              , matP0 = Apply "mat11" [Apply "square" [Var "sigma_prior"]]
              }
          expected0 =
            StanAST
            { stdata =
              [("prior_scales", (S.Type (S.VectorType (litI 3)) [], lo0))]
            , stxdata =
              [(("q_scale", (S.Type S.RealType [], nobounds))
               ,Apply "[]" [Var "prior_scales", litI 2])
              ,(("eps_scale", (S.Type S.RealType [], nobounds))
               ,Apply "[]" [Var "prior_scales", litI 3])
              ,(("I1", (S.Type (S.MatrixType (litI 1) (litI 1)) [], nobounds))
               ,Apply "rep_matrix" [litR 1, litI 1, litI 1])
              ]
            , stparms =
              [("sigma_prior_raw", (S.Type S.RealType [], lo0))
              ,("sigma_q"
               ,(S.Type S.RealType []
                ,bounds (litR 0)
                 (Apply "*" [Apply "[]" [Var "prior_scales", litI 1]
                            ,Var "sigma_prior_raw"])))
              ,("sigma_eps"
               ,(S.Type S.RealType []
                ,bounds (litR 0)
                  (Apply "*" [Apply "*" [Apply "*"
                    [litR 2, Var "eps_scale"], Var "q_scale"],
                    Apply "*" [Apply "[]" [Var "prior_scales", litI 1]
                              ,Var "sigma_prior_raw"]])))
                -- or should we apply associativity here so that everything
                -- associates to the left?
              ]
            , stxparms =
              []
            , stmodel =
              StanModelBlock
              { medefs =
                [(("sigma_prior", S.Type S.RealType [])
                 ,Apply "*" [Apply "[]" [Var "prior_scales", litI 1]
                            ,Var "sigma_prior_raw"])
                ,(("mu_q", S.Type S.RealType [])
                 ,Apply "*" [Var "q_scale", Var "sigma_prior"])

                ,(("mu_eps", S.Type S.RealType [])
                 ,Apply "*" [Var "eps_scale", Var "mu_q"])
                ]
              , medraws =
                [("sigma_prior_raw"
                 ,S.DistrExpr "normal" [litR 0, litR 1] (lobound (litR 0)))
                ,("sigma_q"
                 ,S.DistrExpr "exponential" [Var "mu_q"]
                  (hibound $ Var "sigma_prior"))
                ,("sigma_eps"
                 ,S.DistrExpr "exponential" [Var "mu_eps"]
                  (hibound $ Apply "*" [litR 2, Var "mu_eps"]))
                ]
              , messm = SSM
                { vecZ = Var "I1"
                , scalH = Apply "rep_vector" [Apply "square" [Var "sigma_eps"]
                                            ,litI 1]
                , matT = Var "I1"
                , matQ = Apply "rep_matrix" [Apply "square" [Var "sigma_q"]
                                              ,litI 1, litI 1]
                , veca0 = Apply "rep_vector" [Var "mu_prior", litI 1]
                , matP0 = Apply "rep_matrix" [Apply "square" [Var "sigma_prior"]
                                             ,litI 1, litI 1]
                }
              }
            }
          expected1 =
            expected0
            { stxparms =
              [(("sigma_prior", (S.Type S.RealType [], nobnds))
               ,Apply "*" [Apply "[]" [Var "prior_scales", litI 1]
                          ,Var "sigma_prior_raw"])
              ,(("mu_q", (S.Type S.RealType [], nobnds))
               ,Apply "*" [Var "q_scale", Var "sigma_prior"])
              ]
            , stmodel =
              (stmodel expected0)
              { medefs =
                [(("mu_eps", S.Type S.RealType [])
                 ,Apply "*" [Var "eps_scale", Var "mu_q"])
                ]
              }
            }
      translate Set.empty model `shouldBe` expected0
      translate (Set.singleton "mu_q") model `shouldBe` expected1


