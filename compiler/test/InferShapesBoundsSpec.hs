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
module InferShapesBoundsSpec (main, spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec
--import Test.HUnit

import AST (FctDef(..), ElemType(..){-, Type(isDistr)-})
import qualified AST as A
import qualified BasicExpr as B
import qualified BasicModel as B
import Env (ShapeSigFct)
import InferShapesBoundsImpl (toTranslateModel1, TTMEnv(..),
                              Disambig(..), disamb1)
import Misc
import SSM
import qualified Translate as T

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  test_disamb
  test_toTranslateModel

test_disamb :: Spec
test_disamb =
  describe "Disambig" $ do
    specify "disamb1" $ do
      let Disambig f = disamb1 [] "foo" in do
        f [] `shouldBe` Just "foo"
        f [A.IType] `shouldBe` Nothing
      let Disambig f = disamb1 [A.RType, A.IType] "bar" in do
        f [] `shouldBe` Nothing
        f [A.RType] `shouldBe` Nothing
        f [A.IType] `shouldBe` Nothing
        f [A.RType, A.IType] `shouldBe` Just "bar"

    specify "<>" $ do
      let Disambig f = disamb1 [A.RType] "r" <>
                     disamb1 [A.IType] "i" <>
                     disamb1 [A.RType] "error"
        in do
          f [] `shouldBe` Nothing
          f [A.RType] `shouldBe` Just "r"
          f [A.IType] `shouldBe` Just "i"      

test_toTranslateModel :: Spec
test_toTranslateModel =
  describe "toTranslateModel" $ do
    converts_trivial_model
    converts_parameters
    converts_body
    checks_for_draw_dep_shape

emptyEnvs :: TTMEnv
emptyEnvs = TTMEnv Map.empty Map.empty Map.empty

converts_trivial_model :: Spec
converts_trivial_model =
  it "converts trivial model" $ do
    let fd = FctDef
             { defName = "main"
             , defParms = [ ]
             , defRetType = A.TSDType
             , defBody =
               A.Apply A.TSDType "ssm"
                 [ A.Var A.VecType "Z"
                 , A.Var A.RType   "H"
                 , A.Var A.MatType "T"
                 , A.Var A.MatType "Q"
                 , A.Var A.VecType "a0"
                 , A.Var A.MatType "P0"
                 ]
             }
    let mdl = T.Model [ ] $
              T.TSDistr $
              SSM { vecZ = B.Var "Z"
                  , scalH = B.Var "H"
                  , matT = B.Var "T"
                  , matQ = B.Var "Q"
                  , veca0 = B.Var "a0"
                  , matP0 = B.Var "P0"
                  }
    toTranslateModel1 emptyEnvs fd `shouldBe` mdl

converts_parameters :: Spec
converts_parameters =
  it "converts parameters" $ do
    let ttmenv = emptyEnvs {
                   disambigEnv = Map.fromList
                     [ "foo" .-> disamb1 [A.RType] "fooconv" ]
                 }
    let parms = 
          [ "n" .->
            A.typeSB A.IType [ ] (A.lobound $ A.litI 1)
          , "a" .->
            A.typeSB A.RType [ ] A.nobounds
          , "x" .->
            A.typeSB A.VecType
                     [ A.Apply A.IType "+" [A.litI 2, A.Var A.IType "n"] ]
                     (A.hibound $ A.Apply A.RType "foo" [A.Var A.RType "a"])
          ]
    let parms1 =
          [ B.Decl "n" $
            B.TypeSB IntType [ ] (B.lobound $ B.litI 1)
          , B.Decl "a" $
            B.TypeSB RealType [ ] B.nobounds
          , B.Decl "x" $
            B.TypeSB RealType
                     [ B.Apply "+" [B.litI 2, B.Var "n"] ]
                     (B.hibound $ B.Apply "fooconv" [B.Var "a"])
          ]
    let fd = FctDef
             { defName = "main"
             , defParms = parms
             , defRetType = A.TSDType
             , defBody =
               A.Apply A.TSDType "ssm"
                 [ A.Var A.VecType "Z"
                 , A.Var A.RType   "H"
                 , A.Var A.MatType "T"
                 , A.Var A.MatType "Q"
                 , A.Var A.VecType "a0"
                 , A.Var A.MatType "P0"
                 ]
             }
    let mdl = T.Model parms1 $
              T.TSDistr $
                SSM { vecZ = B.Var "Z"
                    , scalH = B.Var "H"
                    , matT = B.Var "T"
                    , matQ = B.Var "Q"
                    , veca0 = B.Var "a0"
                    , matP0 = B.Var "P0"
                    }
    toTranslateModel1 ttmenv fd `shouldBe` mdl

shsig_t :: [B.TypeS] -> a -> ShapeSigFct a
shsig_t atyps rtyp (_, atyps') =
  justWhen (atyps' == atyps) rtyp

shsig_at :: [B.Expr] -> [B.TypeS] -> a -> ShapeSigFct a
shsig_at aexprs atyps rtyp (aexprs', atyps') =
  justWhen (aexprs' == aexprs && atyps' == atyps) rtyp

converts_body :: Spec
converts_body =
  it "converts the body" $ do
    let daEnv =
          Map.fromList
          [ "f1" .-> disamb1 [A.RType] "f1r" <>
                     disamb1 [A.IType] "f1i"
          , "f2" .-> disamb1 [A.IType, A.VecType] "f2m"
          , "df" .-> disamb1 [A.IType, A.MatType] "dfm"
          , "Hfct" .-> disamb1 [A.VecType, A.MatType] "Hvm"
          ]
    let fshEnv =
          Map.fromList
          [ "f1r" .-> shsig_t [B.RType] B.RType
          , "g1"  .-> shsig_at [B.Apply "f1r" [B.litR 2.5]] [B.RType]
                               (B.VecType $ B.litI 5)
          , "f1i" .-> shsig_t [B.IType] B.IType
          , "g2"  .-> shsig_at [B.Var "x"] [B.VecType $ B.litI 5]
                               (B.VecType $ B.Var "n")
          , "f2m" .-> shsig_t [B.IType, B.VecType $ B.Var "n"]
                              (B.MatType (B.Var "n") (B.litI 2))
          , "Hvm" .-> shsig_t [B.VecType $ B.litI 5,
                               B.MatType (B.litI 3) (B.Var "n")]
                              B.RType
          ]
    let dshEnv =
          Map.fromList
          [ "df"  .-> shsig_t [B.IType, B.MatType (B.Var "n") (B.litI 2)]
                              (B.TypeSB RealType
                                 [B.litI 3, B.Var "n"]
                                 (B.lobound $ B.litR 1.0))
          ]
    let ttmenv = TTMEnv{ fctShEnv = fshEnv, distrShEnv = dshEnv
                       , disambigEnv = daEnv }
    let body =
          A.Let A.TSDType "x"
            (A.Apply A.VecType "g1" [A.Apply A.RType "f1" [A.litR 2.5]]) $
          A.Draw A.TSDType "y"
            (A.Apply (A.MatType){A.isDistr=True} "df"
               [A.Apply A.IType "f1" [A.litI 3],
                A.Apply A.MatType "f2"
                  [A.litI 15, A.Apply A.VecType "g2" [A.Var A.VecType "x"]]]) $
          A.Apply A.TSDType "ssm"
          [ A.Var A.VecType "Z1"
          , A.Apply A.RType "Hfct" [ A.Var A.VecType "x", A.Var A.MatType "y" ]
          , A.Var A.MatType "T1"
          , A.Var A.MatType "Q1"
          , A.Var A.VecType "a01"
          , A.Var A.MatType "P01"
          ]
    let body1 =
          T.Let (B.Decl "x" $ B.VecType $ B.litI 5)
                (B.Apply "g1" [B.Apply "f1r" [B.litR 2.5]])
                $
          T.Draw (B.Decl "y" $
                  B.TypeSB RealType
                    [B.litI 3, B.Var "n"]
                    (B.lobound $ B.litR 1.0))
                 (B.DistrExpr "df"
                    [B.Apply "f1i" [B.litI 3],
                     B.Apply "f2m" [B.litI 15, B.Apply "g2" [B.Var "x"]]])
                 $
          T.TSDistr $
            SSM{ vecZ = B.Var "Z1"
               , scalH = B.Apply "Hvm" [B.Var "x", B.Var "y"]
               , matT = B.Var "T1"
               , matQ = B.Var "Q1"
               , veca0 = B.Var "a01"
               , matP0 = B.Var "P01"
               }
    let fd = FctDef
             { defName = "main"
             , defParms = [ ]
             , defRetType = A.TSDType
             , defBody = body
             }
    let mdl = T.Model [ ] body1
    toTranslateModel1 ttmenv fd `shouldBe` mdl

forceIO :: Show a => a -> IO ()
forceIO x =
-- Turn x into an IO action that forces the evaluation of x
  seq (show x == "") (return ())

checks_for_draw_dep_shape :: Spec
checks_for_draw_dep_shape =
  it "checks for drawn vars whose shape depends on another drawn var" $ do
    let daEnv = Map.empty
    let fshEnv = Map.fromList
          [ "+" .-> shsig_t [B.IType, B.IType] B.IType ]
    let dshEnv = Map.fromList
          [ "poisson" .-> shsig_t [B.RType]
                            (B.TypeSB IntType [ ] $ B.lobound $ B.litI 0)
          , "grok" .-> shsig_at [B.Var "n1"] [B.IType]
                         (B.TypeSB RealType
                            [B.Apply "foo" [B.Var "n1"]] B.nobounds)
          ]
    let ttmenv = TTMEnv{ fctShEnv = fshEnv, distrShEnv = dshEnv
                       , disambigEnv = daEnv }
    let intDistr = (A.IType){ A.isDistr = True }
    let vecDistr = (A.VecType){ A.isDistr = True }
    let body =
          A.Draw A.TSDType "n"
            (A.Apply intDistr "poisson" [A.litR 3.25]) $
          A.Let A.TSDType "n1"
            (A.Apply A.IType "+" [A.litI 1, A.Var A.IType "n"]) $
          A.Draw A.TSDType "a0"
            (A.Apply vecDistr "grok" [A.Var A.IType "n1"]) $
          A.Apply A.TSDType "ssm"
          [ A.Var A.VecType "Z"
          , A.Var A.RType "H"
          , A.Var A.MatType "T"
          , A.Var A.MatType "Q"
          , A.Var A.VecType "a0"
          , A.Var A.MatType "P0"
          ]
    let fd = FctDef
             { defName = "main"
             , defParms = [ ]
             , defRetType = A.TSDType
             , defBody = body
             }
    
    (forceIO $ toTranslateModel1 ttmenv fd) `shouldThrow` errorCall "Shape of a0 depends on drawn variable(s)"
