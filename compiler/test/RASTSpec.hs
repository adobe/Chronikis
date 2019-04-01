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
module RASTSpec (main, spec) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.String.QQ
import Test.Hspec
import Text.Printf (printf)

import Number
import RAST

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  test_printRExpr
  test_printRFctDef

test_printRExpr :: Spec
test_printRExpr = do
  it "prints vars" $
    printRExpr (RVar "foo") `shouldBe` "foo_"
  it "prints literals" $ do
    printRExpr (RLit $ IntVal 27) `shouldBe` "27L"
    printRExpr (RLit $ IntVal 0) `shouldBe` "0L"
    printRExpr (RLit $ RealVal 5) `shouldBe` "5.0"
    printRExpr (RLit $ RealVal $ scientific 1234 100) `shouldBe` "1.234e103"
    printRExpr (RLit $ RealVal $ scientific 567 (-200)) `shouldBe` "5.67e-198"
    printRExpr (RLit $ IntVal (-1)) `shouldBe` "(-1L)"
    printRExpr (RLit $ RealVal (-1.0)) `shouldBe` "(-1.0)"
  it "prints function calls" $ do
    printRExpr (RApply "foo" []) `shouldBe` "foo()"
    printRExpr (RApply "sqrt" [RVar "x"]) `shouldBe` "sqrt(x_)"
    printRExpr (RApply "min" [RVar "a", RVar "b"]) `shouldBe` "min(a_, b_)"
  it "prints function calls with named arguments" $ do
    printRExpr (RApply' "foo" [] []) `shouldBe` "foo()"
    printRExpr (RApply' "bar" [RVar "x"] []) `shouldBe` "bar(x_)"
    printRExpr (RApply' "bar" [RVar "x", rlitI 2] []) `shouldBe` "bar(x_, 2L)"
    printRExpr (RApply' "baz" [] [("a", rlitI 1)]) `shouldBe` "baz(a = 1L)"
    printRExpr (RApply' "baz" [] [("a", rlitI 1), ("b", rlitR 3.5)])
      `shouldBe` "baz(a = 1L, b = 3.5)"
    printRExpr (RApply' "bip" [rlitI 3, rlitI 7]
                              [("c", rlitI 2), ("d", rlitI 1)])
      `shouldBe` "bip(3L, 7L, c = 2L, d = 1L)"
  it "prints index expressions with no indices" $ do
    printRExpr (RIndex False (RVar "w") []) `shouldBe` "w_[, drop=FALSE]"
    printRExpr (RIndex True (RVar "w") []) `shouldBe` "w_[]"
    printRExpr (RIndex False (RVar "w") [Nothing]) `shouldBe` "w_[, drop=FALSE]"
    printRExpr (RIndex True (RVar "w") [Nothing]) `shouldBe` "w_[]"
    printRExpr (RIndex False (RVar "w") [Nothing, Nothing])
      `shouldBe` "w_[, , drop=FALSE]"
    printRExpr (RIndex False (RVar "w") [Nothing, Nothing])
      `shouldBe` "w_[, , drop=FALSE]"
    printRExpr (RIndex True (RVar "w") [Nothing, Nothing])
      `shouldBe` "w_[, ]"
  it "prints index expressions with one index" $ do
    printRExpr (RIndex False (RVar "x") [Just $ RVar "n"])
      `shouldBe` "x_[n_, drop=FALSE]"
    printRExpr (RIndex True (RVar "x") [Just $ RVar "n"])
      `shouldBe` "x_[n_]"
    printRExpr (RIndex False (RVar "y") [Nothing, Just $ RVar "n"])
      `shouldBe` "y_[, n_, drop=FALSE]"
    printRExpr (RIndex True (RVar "y") [Nothing, Just $ RVar "n"])
      `shouldBe` "y_[, n_]"
    printRExpr (RIndex False (RVar "y") [Just $ RVar "m", Nothing])
      `shouldBe` "y_[m_, , drop=FALSE]"
    printRExpr (RIndex True (RVar "y") [Just $ RVar "m", Nothing])
      `shouldBe` "y_[m_, ]"
  it "prints index expressions with two indices" $ do
    printRExpr (RIndex False (RVar "y") [Just $ RVar "m", Just $ RVar "n"])
      `shouldBe` "y_[m_, n_, drop=FALSE]"
    printRExpr (RIndex True (RVar "y") [Just $ RVar "m", Just $ RVar "n"])
      `shouldBe` "y_[m_, n_]"
    printRExpr (RIndex True (RVar "z")
                       [Just $ RVar "m", Just $ RVar "n", Nothing])
      `shouldBe` "z_[m_, n_, ]"
    printRExpr (RIndex True (RVar "z")
                       [Just $ RVar "m", Nothing, Just $ RVar "p"])
      `shouldBe` "z_[m_, , p_]"
    printRExpr (RIndex True (RVar "z")
                       [Nothing, Just $ RVar "n", Just $ RVar "p"])
      `shouldBe` "z_[, n_, p_]"
  it "prints [[ expressions" $
    printRExpr (RApply "[[" [RVar "x", RVar "n"]) `shouldBe` "x_[[n_]]"
  it "prints $ expressions" $ do
    printRExpr (RField (RVar "x") "f")
      `shouldBe` "x_$f"
    printRExpr (RField (RField (RVar "y") "ay") "bee")
      `shouldBe` "y_$ay$bee"
    printRExpr (RField (RApply "foo" []) "bar")
      `shouldBe` "(foo())$bar"
    printRExpr (RField (RIndex True (RVar "a") [Just $ RVar "n"]) "foo")
      `shouldBe` "(a_[n_])$foo"
    printRExpr (RField (RApply "[[" [RVar "lst", RVar "m"]) "bar")
      `shouldBe` "(lst_[[m_]])$bar"
  it "prints lambda expressions" $ do
    printRExpr (RLambda [] [] (RVar "x"))
      `shouldBe` "function(){ x_ }"
    printRExpr (RLambda [] [RLet "x" $ rlitR 3.5] $
                RApply "+" [RVar "x", rlitR 2.0])
      `shouldBe` "function(){ x_ <- 3.5; x_ + 2.0 }"
    printRExpr (RLambda ["x"] [] $ RApply "+" [RVar "x", rlitR 1.3])
      `shouldBe` "function(x_){ x_ + 1.3 }"
    printRExpr (RLambda ["x", "y"] [] $ RApply "+" [RVar "x", RVar "y"])
      `shouldBe` "function(x_, y_){ x_ + y_ }"
    printRExpr (RApply "+"
                [rlitI 0, RLambda ["a"] [] $ RApply "*" [RVar "a", RVar "a"]])
      `shouldBe` "0L + (function(a_){ a_ * a_ })"
  it "prints unary operator expressions" $ do
    printRExpr (RApply "unary-" [RVar "x"]) `shouldBe` "- x_"
    printRExpr (RApply "unary+" [RVar "x"]) `shouldBe` "+ x_"
    printRExpr (RApply "!" [RVar "x"]) `shouldBe` "! x_"
  it "prints right-associative operator calls" $ do
    printRExpr (RApply "^" [RVar "a", RVar "b"]) `shouldBe` "a_ ^ b_"
    printRExpr (RApply "^" [RVar "a", RVar "b", RVar "c"])
      `shouldBe` "a_ ^ b_ ^ c_"
    printRExpr (RApply "^" [RVar "a", RApply "^" [RVar "b", RVar "c"]])
      `shouldBe` "a_ ^ b_ ^ c_"
  it "prints left-associative operator calls" $ do
    forM_ [":", "%/%", "*", "/", "+", "-", "<", ">", "<=", ">=", "==", "!=",
           "&", "&&", "|", "||", "~"] $ \op -> do
      let str1 = printf "a_ %s b_" op
          str2 = printf "a_ %s b_ %s c_" op op
      printRExpr (RApply op [RVar "a", RVar "b"]) `shouldBe` str1
      printRExpr (RApply op [RVar "a", RVar "b", RVar "c"]) `shouldBe` str2
      printRExpr (RApply op [RApply op [RVar "a", RVar "b"], RVar "c"])
        `shouldBe` str2
  it "requires binary operators to have at least two arguments" $ do
    evaluate (printRExpr $ RApply "-" [RVar "x"]) `shouldThrow` anyException
  it "omits parentheses when operator precedence allows" $ do
    let e  = RApply "~" [eL, eR]
        eL = RApply "|" [RApply "||" [RVar "a", RVar "b"], RVar "c"]
        eR = RApply "|" [eRL, eR2]
        eRL = RApply "&" [RApply "&&" [RVar "d", RVar "e"], RVar "f"]
        eR2 = RApply "&" [RApply "!" [eR2L], RApply "!" [eR3]]
        eR2L = RApply "<" [RApply ">" [RApply "<=" [RApply ">=" [RApply "=="
                 [RApply "!=" [RVar "h", RVar "i"], RVar "j"], RVar "k"],
                 RVar "l"], RVar "m"], RVar "m1"]
        eR3 = RApply "<" [eR3L, eR4]
        eR3L = RApply "+" [RApply "-" [RVar "n", RVar "o"], RVar "p"]
        eR4 = RApply "+" [eR4L, eR5]
        eR4L = RApply "*" [RApply "/" [RVar "q", RVar "r"], RVar "s"]
        eR5 = RApply "*" [eR5L, eR6]
        eR5L = RApply "%/%" [RVar "t", RVar "u"]
        eR6 = RApply "%/%" [eR6L, eR7]
        eR6L = RApply ":"
               [RApply "unary+" [RVar "v"], RApply "unary-" [RVar "w"]]
        eR7 = RApply ":" [RApply "unary+" [eR7L], RApply "unary-" [eR8]]
        eR7L = RApply "^" [RVar "x", RVar "y"]
        eR8 = RApply "^" [eR8L, eR9]
        eR8L = RIndex True (RApply "[[" [RVar "z", RVar "aa"])
                      [Just $ RVar "bb"]
        eR9 = RIndex True (RVar "cc") [Just $ RVar "dd"]
      in printRExpr e `shouldBe`
         "a_ || b_ | c_ ~ d_ && e_ & f_ | " ++
         "! h_ != i_ == j_ >= k_ <= l_ > m_ < m1_ & " ++
         "! n_ - o_ + p_ < q_ / r_ * s_ + t_ %/% u_ * " ++
         "+ v_ : - w_ %/% + x_ ^ y_ : - z_[[aa_]][bb_] ^ cc_[dd_]"
    printRExpr (RApply "!" [RApply "unary-" [RVar "x"]])
      `shouldBe` "! - x_"
  it "inserts parentheses when operator precedence requires" $ do
    let e = RApply "[[" [eL, eR]
        eL = RApply "^" [RApply "unary-" [eL2], RApply "unary+" [eLR]]
        eR = RIndex True eRL [Just eR2]
        eRL = RApply "^" [RVar "c", RVar "d"]
        eR2 = RVar "e"
        eL2 = RVar "f"
        eLR = RApply ":" [eLRL, eLR2]
        eLRL = RApply "%/%" [RVar "g", RVar "h"]
        eLR2 = RApply "%/%" [eLR2L, eLR3]
        eLR2L = RApply "*" [RVar "i", RVar "j"]
        eLR3 = RApply "/" [eLR3L, eLR4]
        eLR3L = RApply "+" [RVar "k", RVar "l"]
        eLR4 = RApply "-" [eLR4L, eLR5]
        eLR4L = RApply "<" [RVar "m", RVar "n"]
        eLR5 = RApply ">" [RApply "!" [eLR5L], RApply "!" [eLR6]]
        eLR5L = RApply "&" [RVar "o", RVar "p"]
        eLR6 = RApply "&&" [eLR6L, eLR7]
        eLR6L = RApply "|" [RVar "q", RVar "r"]
        eLR7 = RApply "||" [eLR7L, eLR8]
        eLR7L = RApply "~" [RVar "s", RVar "t"]
        eLR8 = RApply "~" [RVar "u", RVar "v"]
    printRExpr e `shouldBe`
      "((- f_) ^ (+ ((g_ %/% h_) : ((i_ * j_) %/% ((k_ + l_) / ((m_ < n_) " ++
      "- ((! (o_ & p_)) > (! ((q_ | r_) && ((s_ ~ t_) || " ++
      "(u_ ~ v_)))))))))))[[(c_ ^ d_)[e_]]]"

test_printRFctDef :: Spec
test_printRFctDef = do
  it "prints a function definition" $ do
    let parms = ["a", "b"]
        stmts = [ RLet "c" (RApply "+" [RVar "a", RLit (IntVal 1)])
                , RCheck (RApply ">" [RVar "b", RLit (IntVal 0)])
                --, RDraw "d" something...
                ]
        ret = RApply "+" [RVar "a", RVar "b", RVar "c"]
        fd = RFctDef{ rfctParms = parms, rfctStmts = stmts, rfctReturn = ret }
        expected = [s|
function(a_, b_) {
  c_ <- a_ + 1L
  stopifnot(b_ > 0L)
  a_ + b_ + c_
}
|]
    printRFctDef fd `shouldBe` expected

          
        
