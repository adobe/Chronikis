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
module ParserSpec (main, spec) where

import Control.Monad (forM_)
import Data.CallStack (HasCallStack)
import Data.Either (isLeft)
import Data.String.QQ
import Test.Hspec

import AST
import Number
import Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  test_number
  test_term
  test_opExpr
  test_expr
  test_ptype_parser
  test_type_parser
  test_fctDef
  test_program

shouldBeR :: (HasCallStack, Eq a, Show a, Eq b, Show b) =>
             Either a b -> b -> Expectation
shouldBeR x y = x `shouldBe` (Right y)

typeS :: Type -> [Expr ()] -> TypeSB ()
typeS typ shape = typeSB typ shape nobounds

pitype :: TypeSB ()
prtype :: TypeSB ()
pitype = typeS IType []
prtype = typeS RType []

test_number :: Spec
test_number =
  describe "number parser" $ do
    it "parses 0" $
      parse number "0" `shouldBeR` IntVal 0
    it "parses integers" $
      parse number "123" `shouldBeR` IntVal 123
    it "parses 0-prefixed integers" $
      parse number "0456" `shouldBeR` IntVal 456
    it "parses floats with no exponent" $
      parse number "3.7" `shouldBeR` RealVal 3.7
    it "parses floats with no fraction" $
      parse number "452e-2" `shouldBeR` RealVal 4.52
    it "parses floats with fraction and lower-case exponent" $
      parse number "3.75e2" `shouldBeR` RealVal 375
    it "parses floats with fraction and upper-case exponent" $
      parse number "2.63E2" `shouldBeR` RealVal 263
    it "parses floats with fraction and negative exponent" $
      parse number "23.85e-4" `shouldBeR` RealVal 0.002385
    it "parses floats with fraction and explicitly positive exponent" $
      parse number "23.85e+4" `shouldBeR` RealVal 238500
    it "complains if exponent is too large (positive)" $ do
      let Left x = parse number "2e12345678912345678912 "
      show x `shouldContain` "Exponent of floating-point number is too large"
    it "complains if exponent is too large (negative)" $ do
      let Left x = parse number "2e-12345678912345678912 "
      show x `shouldContain` "Exponent of floating-point number is too large"

test_term :: Spec
test_term =
  describe "term parser" $ do
    it "parses alphabetic identifiers" $
      parse term "v" `shouldBeR` Var () "v"
    it "parses alphanumeric identifiers" $
      parse term "x37" `shouldBeR` Var () "x37"
    it "parses identifiers with alphanumerics and underscore" $
      parse term "ar1_3" `shouldBeR` Var () "ar1_3"
    it "parses real numbers" $
      parse term "2.3" `shouldBeR` (litR0 2.3)
    it "parses integers" $
      parse term "3287" `shouldBeR` (litI0 3287)
    it "parses zero-arg function calls" $
      parse term "diag()" `shouldBeR` Apply () "diag" []
    it "parses one-arg function calls" $
      parse term "sin(x)" `shouldBeR` Apply () "sin" [Var () "x"]
    it "parses fct(-e) correctly" $ -- This is a regression test
      parse term "sin(-x)" `shouldBeR`
        Apply () "sin" [Apply () "negate" [Var () "x"]]
    it "parses two-arg function calls" $
      parse term "foo (26, y)" `shouldBeR`
      Apply () "foo" [litI0 26, Var () "y"]
    it "parses multi-arg function calls" $
      parse term "bar(var, cos(theta), 24.5)" `shouldBeR`
      Apply () "bar" [ Var () "var"
                   , Apply () "cos" [Var () "theta"]
                   , litR0 24.5 ]
    it "parses array expresssions" $ do
      parse term "{ 1 }" `shouldBeR`
        Apply () "{}" [ litI0 1 ]
      parse term "{ x + y }" `shouldBeR`
        Apply () "{}" [ Apply () "+" [ Var () "x", Var () "y" ] ]
      parse term "{ x, 3.5 }" `shouldBeR`
        Apply () "{}" [ Var () "x", litR0 3.5 ]
      parse term "{ }" `shouldSatisfy` isLeft

test_opExpr :: Spec
test_opExpr =
  describe "opExpr parser" $ do
    it "parses unary '-'" $
      parse opExpr "-x" `shouldBeR` Apply () "negate" [Var () "x"]
    it "parses unary '+'" $
      parse opExpr "+x" `shouldBeR` Var () "x"
    it "parses '^'" $
      parse opExpr "v^x^2" `shouldBeR`
      Apply () "^" [Var () "v", Apply () "^" [Var () "x", litI0 2]]
    it "parses '*'" $
      parse opExpr "3 * a * b" `shouldBeR`
      Apply () "*" [Apply () "*" [litI0 3, Var () "a"], Var () "b"]
    it "parses '/'" $
      parse opExpr "3.0 / a / b" `shouldBeR`
      Apply () "/" [Apply () "/" [litR0 3.0, Var () "a"], Var () "b"]
    it "parses 'div'" $
      parse opExpr "3 div a div b" `shouldBeR`
      Apply () "div" [Apply () "div" [litI0 3, Var () "a"], Var () "b"]
    it "parses '*' and '/' together" $
      parse opExpr "3 * a / b" `shouldBeR`
      Apply () "/" [Apply () "*" [litI0 3, Var () "a"], Var () "b"]
    it "parses binary '+'" $
      parse opExpr "3 + a + b" `shouldBeR`
      Apply () "+" [Apply () "+" [litI0 3, Var () "a"], Var () "b"]
    it "parses binary '-'" $
      parse opExpr "3 - a - b" `shouldBeR`
      Apply () "-" [Apply () "-" [litI0 3, Var () "a"], Var () "b"]
    it "parses with correct precedence" $
      parse opExpr "x^2 * y - a * b + c*d^n" `shouldBeR`
      Apply () "+"
      [ Apply () "-"
        [ Apply () "*"
          [ Apply () "^" [ Var () "x", litI0 2 ]
          , Var () "y"
          ]
        , Apply () "*" [ Var () "a", Var () "b" ]
        ]
      , Apply () "*"
        [ Var () "c"
        , Apply () "^" [ Var () "d", Var () "n" ]
        ]
      ]
    it "parses parenthesized expressions" $
      parse opExpr "a * (b + 2) + c" `shouldBeR`
      Apply () "+"
      [ Apply () "*"
        [ Var () "a"
        , Apply () "+" [Var () "b", litI0 2]
        ]
      , Var () "c"
      ]
    it "parses single-index expressions" $
      parse opExpr "arr[i]" `shouldBeR`
      Apply () "[]" [Var () "arr", Var () "i"]
    it "parses double-index expressions" $
      parse opExpr "arr[x + 1, n ^ 2]" `shouldBeR`
      Apply () "[]" [ Var () "arr"
                  , Apply () "+" [Var () "x", litI0 1]
                  , Apply () "^" [Var () "n", litI0 2]
                  ]
    it "parses index expressions with computed array" $
      parse opExpr "(a + b)[i]" `shouldBeR`
      Apply () "[]" [Apply () "+" [Var () "a", Var () "b"], Var () "i"]

test_expr :: Spec
test_expr =
  describe "expr parser" $ do
    it "parses let expressions" $
      parse expr "x = 3; foo(x)" `shouldBeR`
      Let () "x" (litI0 3) (Apply () "foo" [Var () "x"])
    it "parses draw expressions" $
      parse expr "x ~ normal(mu, sigma); x ^ 4" `shouldBeR`
      Draw () "x"
       (Apply () "normal" [Var () "mu", Var () "sigma"])
       (Apply () "^" [Var () "x", litI0 4])
    it "parses terms" $
      parse expr "2.83" `shouldBeR` (litR0 2.83)
    it "parses op exprs" $
      parse expr "x - y" `shouldBeR` Apply () "-" [Var () "x", Var () "y"]
    it "parses complex expressions" $
      parse expr "x ~ expon(mu); y = 3*x; sum(1, n, z[j] / y)" `shouldBeR`
      (Draw () "x" (Apply () "expon" [Var () "mu"]) $
        Let () "y" (Apply () "*" [litI0 3, Var () "x"]) $
          Apply () "sum"
            [ litI0 1, Var () "n"
            , Apply () "/" [ Apply () "[]" [Var () "z", Var () "j"]
                        , Var () "y"
                        ]
            ])
    it "parses parenthesized exprs" $
      parse expr "(w = y; 1.0 / w) - x[1, k] + (z ~ foo(th); sin(z))"
      `shouldBeR`
      Apply () "+"
      [ Apply () "-"
        [ Let () "w" (Var () "y") (Apply () "/" [litR0 1, Var () "w"])
        , Apply () "[]" [Var () "x", litI0 1, Var () "k"]
        ]
      , Draw () "z" (Apply () "foo" [Var () "th"]) (Apply () "sin" [Var () "z"])
      ]

test_fctDef :: Spec
test_fctDef =
  describe "definition parser" $ do
    it "parses zero-arg function def" $
      parse fctDef "def foo() : int = 1" `shouldBeR`
      FctDef{ defName = "foo", defParms = [], defRetType = IType,
              defBody = litI0 1 }
    it "parses one-arg function def" $
      parse fctDef
        [s|def bar(x : real{0,}) : real$~ =
           u ~ exponential_m(x);
           ar1(1 / (1 + u))
        |]
      `shouldBeR`
      FctDef{ defName = "bar"
            , defParms = [("x", typeSB RType [] (lobound $ litI0 0))]
            , defRetType = (RType){ isTS = True, isDistr = True }
            , defBody =
              Draw () "u" (Apply () "exponential_m" [Var () "x"])
                        (Apply () "ar1"
                          [Apply () "/"
                            [ litI0 1
                            , Apply () "+" [litI0 1, Var () "u"]
                            ]
                          ])
            }
    it "parses multi-arg functions" $
      parse fctDef
        [s|def tvlr(X : real[1]$, phi, mu, sigma : real, Sigma: real[1,1])
               : real$~
               = lin(X, ar1(phi, mu, Sigma)) + wn(sigma)
        |]
      `shouldBeR`
      FctDef { defName = "tvlr"
              , defParms = [ ("X", typeS (VecType){isTS = True} [litI0 1])
                           , ("phi", prtype)
                           , ("mu", prtype)
                           , ("sigma", prtype)
                           , ("Sigma", typeS MatType [litI0 1, litI0 1])
                           ]
              , defRetType = (RType){ isTS = True, isDistr = True }
              , defBody =
                Apply () "+"
                [ Apply () "lin"
                  [ Var () "X"
                  , Apply () "ar1" [ Var () "phi", Var () "mu", Var () "Sigma" ]
                  ]
                , Apply () "wn" [ Var () "sigma" ]
                ]
              }

test_program :: Spec
test_program =
  describe "program parser" $ do
    it "parses single definition" $
      parse program [s|
        def main(phi, sigma2 : real) : real$~ =
        ar1(phi, 0.0, sigma2)
      |]
      `shouldBeR`
      [ FctDef { defName = "main"
               , defParms = [("phi", prtype), ("sigma2", prtype)]
               , defRetType = (RType){ isTS = True, isDistr = True }
               , defBody = Apply () "ar1" [ Var () "phi"
                                        , litR0 0.0
                                        , Var () "sigma2"
                                        ]
               }
      ]
    it "allow omission of main return type" $ do
      parse program [s|
        def main(phi, mu, sigma2 : real) =
        ar1(phi, mu, sigma2)
      |]
      `shouldBe`
      parse program [s|
        def main(phi, mu, sigma2 : real) : real$~ =
        ar1(phi, mu, sigma2)
      |]
    it "parses multiple definitions" $
      parse program [s|
        def sqrint(n : int) : int = n^2
        
        def main(n : int, scale : real) : real$~ =
          sigma ~ exponential_m(scale);
          ind(1, n, normal(0, sigma))
      |]
      `shouldBeR`
      [ FctDef { defName = "sqrint", defParms = [("n", pitype)]
               , defRetType = IType
               , defBody = Apply () "^" [Var () "n", litI0 2]
               }
      , FctDef{ defName = "main"
              , defParms = [("n", pitype), ("scale", prtype)]
              , defRetType = (RType){ isTS = True, isDistr = True }
              , defBody =
                Draw () "sigma" (Apply () "exponential_m" [Var () "scale"]) $
                Apply () "ind"
                [ litI0 1
                , Var () "n"
                , Apply () "normal" [litI0 0, Var () "sigma"]
                ]
              }
      ]

test_ptype_parser :: Spec
test_ptype_parser =
  describe "ptype parser" $ do
    let parsept = parse parm_type_spec
    it "complains if no size expression" $ do
      parsept "real[]" `shouldSatisfy` isLeft
    it "parses element types" $ do
      parsept "int" `shouldBeR` pitype
      parsept "real" `shouldBeR` prtype
    it "parses array types" $ do
      parsept "int[n]" `shouldBeR`
        typeS (IType){numDim=1} [Var () "n"]
      parsept "real[2,k]" `shouldBeR`
        typeS MatType [litI0 2, Var () "k"]
      parsept "real[2 * N]" `shouldBeR`
        typeS VecType [Apply () "*" [litI0 2, Var () "N"]]
    it "parses time-series types" $ do
      parsept "real$" `shouldBeR`
        typeS (RType){ isTS = True } []
      parsept "real[m]$" `shouldBeR`
        typeS (VecType){ isTS = True } [Var () "m"] 
      parsept "int$" `shouldBeR`
        typeS (IType){ isTS = True } []
    it "parses distribution types" $ do
      parsept "real~" `shouldBeR`
        typeS (RType){ isDistr = True } []
      parsept "real[K]~" `shouldBeR`
        typeS (VecType){ isDistr = True } [Var () "K"]
      parsept "real$~" `shouldBeR`
        typeS (RType){ isDistr = True, isTS = True } []
    it "parses bounds on scalar types" $ do
      parsept "real{0, }" `shouldBeR`
        typeSB RType [] (lobound $ litI0 0)
      parsept "int{ , 1}" `shouldBeR`
        typeSB IType [] (hibound $ litI0 1)
      let bnds_ab = bounds (Var () "a") (Var () "b")
      parsept "real{a, b}" `shouldBeR`
        typeSB RType [] bnds_ab
      parsept "real{a, b} $~" `shouldBeR`
        typeSB (RType){isTS=True, isDistr=True} [] bnds_ab
    it "parses bounds on array types" $
      parsept "int {0, } [m]" `shouldBeR`
        typeSB (IType){numDim=1} [Var () "m"] (lobound $ litI0 0)
    it "allows whitespace in appropriate places" $
      parsept "int{0,}[m]$~" `shouldBe`
        parsept "int { 0 , } [ m ] $~"
    it "is a token parser" $ do
      let type2 = do t1 <- parm_type_spec
                     t2 <- parm_type_spec
                     return (t1, t2)
      parse type2 "int real" `shouldBeR` (pitype, prtype)
      parse type2 "real[m]$~ \tint[n]" `shouldBeR`
        ( typeS (VecType){ isTS = True, isDistr = True } [Var () "m"]
        , typeS (IType){numDim=1} [Var () "n"]
        )

test_type_parser :: Spec
test_type_parser =
  describe "type parser" $ do
    it "parses element types" $ do
      parse type_spec "int" `shouldBeR` IType
      parse type_spec "real" `shouldBeR` RType
    it "parses array types" $ do
      parse type_spec "int[]" `shouldBeR` (IType){ numDim = 1 }
      parse type_spec "real[,]" `shouldBeR` (RType){ numDim = 2 }
      parse type_spec "real[]" `shouldBeR` (RType){ numDim = 1}
    it "parses time-series types" $ do
      parse type_spec "real$" `shouldBeR` (RType){ isTS = True }
      parse type_spec "real[]$" `shouldBeR` (RType){ isTS = True, numDim = 1 }
      parse type_spec "int$" `shouldBeR` (IType){ isTS = True }
    it "parses distribution types" $ do
      parse type_spec "real~" `shouldBeR` (RType){ isDistr = True }
      parse type_spec "real[]~" `shouldBeR` (RType){ isDistr = True, numDim = 1 }
      parse type_spec "real$~" `shouldBeR` (RType){ isDistr = True, isTS = True }
    it "is a token parser" $ do
      let type2 = do { t1 <- type_spec; t2 <- type_spec; return (t1, t2) }
      parse type2 "int real" `shouldBeR` (IType, RType)
      parse type2 "real[]$~ \tint[]" `shouldBeR`
        ((RType){ numDim = 1, isTS = True, isDistr = True },
          (IType){ numDim = 1 })
    it "is semi-consistent with show" $ do
      let cases =
            [("int", "int"), ("real", "real"), ("int[]", "int@"),
             ("real[,]", "real@@"), ("real[]", "real@"), ("real$", "real$"),
             ("real[]$", "real@$"), ("int$", "int$"), ("real~", "real~"),
             ("real[]~", "real@~"), ("real$~", "real$~"),
             ("real[]$~", "real@$~"), ("real[,,]~", "real@@@~")]
      forM_ cases $ \(str, expected) ->
        (show <$> parse type_spec str) `shouldBeR` expected
