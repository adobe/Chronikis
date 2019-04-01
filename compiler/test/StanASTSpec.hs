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
module StanASTSpec (main, spec) where

import Control.Monad (forM_, when)
--import Data.List (intercalate) -- remove
import Unique (allUnique)
import qualified Data.Map.Strict as Map
import Data.Scientific (scientific, Scientific)
import Data.String.QQ
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempFile)
import System.Process as P
import System.IO (hPutStr, hClose)
import Test.Hspec
import Text.Printf (printf)

import Misc
import Number
import BasicExpr
import SSM
import StanASTImpl

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  test_topo_sort_map
  test_wrap
  test_normlines
  test_ecode
  test_bcode
  test_etcode
  test_decode
  test_stan_var_decl_def
  test_modelblock
  test_function_defs
  test_function_defs_long
  test_used_fcts
  test_compileStanAST

liti :: Integer -> Expr
litr :: Scientific -> Expr
liti n = Lit $ IntVal n
litr x = Lit $ RealVal x

test_topo_sort_map :: Spec
test_topo_sort_map =
  describe "topo_sort_map" $ do
    let diamond = Map.fromList
                  [ ("a", (["b", "c"], 10))
                  , ("b", (["d"], 20))
                  , ("c", (["d"], 30))
                  , ("d", ([], 40))
                  ]
        emptyList = [] :: [(String, Int)]
    it "Handles empty map" $
      topo_sort_map (Map.empty) ["a"] `shouldBe` emptyList
    it "Handles unknown keys" $
      topo_sort_map diamond ["e", "f"] `shouldBe` emptyList
    it "Handles nontrivial cases" $ do
      topo_sort_map diamond ["d"] `shouldBe` [("d", 40)]
      topo_sort_map diamond ["c"] `shouldBe` [("c", 30), ("d", 40)]
      topo_sort_map diamond ["a"] `shouldSatisfy`
        flip elem [ [("a", 10), ("b", 20), ("c", 30), ("d", 40)]
                  , [("a", 10), ("c", 30), ("b", 20), ("d", 40)]
                  ]
    it "Handles complex case" $ do
      let m = Map.fromList
              [ ("a", (["e"], 10))
              , ("b", (["e"], 20))
              , ("c", (["f"], 30))
              , ("d", (["f"], 40))
              , ("e", (["g", "h"], 50))
              , ("f", (["h", "i"], 60))
              , ("g", ([], 70))
              , ("h", ([], 80))
              , ("i", ([], 90))
              , ("j", ([], 100))
              ]
      let res = topo_sort_map m ["j", "b", "c"] :: [(String, Int)]
      Map.fromList res `shouldBe`
        Map.fromList
        [ ("b", 20)
        , ("c", 30)
        , ("e", 50)
        , ("f", 60)
        , ("g", 70)
        , ("h", 80)
        , ("i", 90)
        , ("j", 100)
        ]
      let nodes = map fst res
          pos = Map.fromList $ zip nodes [(1::Int)..]
          ok = do
            bpos <- Map.lookup "b" pos
            cpos <- Map.lookup "c" pos
            epos <- Map.lookup "e" pos
            fpos <- Map.lookup "f" pos
            gpos <- Map.lookup "g" pos
            hpos <- Map.lookup "h" pos
            ipos <- Map.lookup "i" pos
            return $ bpos < epos && cpos < fpos &&
                     epos < gpos && epos < hpos &&
                     fpos < hpos && fpos < ipos
      ok `shouldBe` Just True

test_wrap :: Spec
test_wrap =
  describe "wrap" $ do
    it "Empty body" $
      wrap "foo" [] `shouldBe` ["foo {", "}"]
    it "One line" $
      wrap "foo" ["bar"] `shouldBe` ["foo {", "  bar", "}"]
    it "Two lines" $
      wrap "foo" ["a", "b"] `shouldBe` ["foo {", "  a", "  b", "}"]

test_normlines :: Spec
test_normlines =
  describe "normlines" $ do
    it "Empty string" $
      normlines "" `shouldBe` []
    it "Whitespace only" $ do
      normlines "\n  \n" `shouldBe` []
    it "Single unindented" $
      normlines "foo\n" `shouldBe` ["foo"]
    it "Single indented" $
      normlines "  hrung" `shouldBe` ["hrung"]
    it "Multiple indented" $
      normlines "  foo\n   bar\n  baz\n    bipple" `shouldBe`
      ["foo", " bar", "baz", "  bipple"]
    it "Whitespace-only internal line" $
      normlines " a\n  \n be\n\n sea" `shouldBe` ["a", "", "be", "", "sea"]
    it "Whitespace-only at beginning and end" $
      normlines "  \n\n  a\n  be\n \n         \n   sea\n\n  \n  " `shouldBe`
      ["a", "be",  "", "", " sea"]

test_ecode :: Spec
test_ecode =
  describe "ecode" $ do
    it "Codes vars" $
      ecode (Var "foo") `shouldBe` "foo_"
    it "Codes literals" $ do
      ecode (Lit $ IntVal 27) `shouldBe` "27"
      ecode (Lit $ IntVal 0) `shouldBe` "0"
      ecode (Lit $ RealVal 5) `shouldBe` "5.0"
      ecode (Lit $ RealVal $ scientific 1234 100) `shouldBe` "1.234e103"
      ecode (Lit $ RealVal $ scientific 567 (-200)) `shouldBe` "5.67e-198"
      ecode (Lit $ IntVal (-1)) `shouldBe` "(-1)"
      ecode (Lit $ RealVal (-1.0)) `shouldBe` "(-1.0)"
    it "Codes normal functions" $ do
      ecode (Apply "pi" []) `shouldBe` "pi()"
      ecode (Apply "sqrt" [Var "x"]) `shouldBe` "sqrt(x_)"
      ecode (Apply "min" [Var "a", Var "b"]) `shouldBe` "min(a_, b_)"
      ecode (Apply "vec_append" [Var "v"]) `shouldBe` "vec_append(v_)"
    it "Codes postfix operators" $ do
      ecode (Apply "[]" [Var "a", Var "i1"]) `shouldBe` "a_[i1_]"
      ecode (Apply "[]" [Var "a", Var "i1", Var "i2"])
        `shouldBe` "a_[i1_, i2_]"
      ecode (Apply "'" [Var "M"]) `shouldBe` "M_'"
    it "Codes ^" $ do
      ecode (Apply "^" [Var "x", Var "n"]) `shouldBe` "x_ ^ n_"
      ecode (Apply "^" [Var "y", Var "m", Var "n"]) `shouldBe` "y_ ^ m_ ^ n_"
      ecode (Apply "^" [Var "z", Apply "^" [Var "j", Var "k"]])
        `shouldBe` "z_ ^ j_ ^ k_"
    it "Codes unary operators" $ do
      ecode (Apply "negate" [Var "a"]) `shouldBe` "-a_"
      ecode (Apply "!" [Var "b"]) `shouldBe` "!b_"
    it "Codes left-associative binary operators" $ do
      forM_ [ "./", ".*", "\\", "%", "/", "*", "-", "+", ">=", ">", "<=", "<"
            , "!=", "==", "&&", "||" ] $ \op -> do
        let str1 = printf "a_ %s b_" op
            str2 = printf "a_ %s b_ %s c_" op op
        ecode (Apply op [Var "a", Var "b"]) `shouldBe` str1
        ecode (Apply op [Var "a", Var "b", Var "c"]) `shouldBe` str2
        ecode (Apply op [Apply op [Var "a", Var "b"], Var "c"]) `shouldBe` str2
    it "Codes array expressions" $ do
      ecode (Apply "{}" []) `shouldBe` "{}"
      ecode (Apply "{}" [Var "a"]) `shouldBe` "{a_}"
      ecode (Apply "{}" [Var "x", Var "y"]) `shouldBe` "{x_, y_}"
      ecode (Apply "{}" [Var "x", Var "y", Var "z"])
        `shouldBe` "{x_, y_, z_}"
    it "Omits parentheses when operator precedence allows" $ do
      let e1 = Apply "+" [Var "x", liti 2]
          e2 = Apply "{}" [e1, Var "y"]
          e3 = Apply "[]" [e2, Var "n"]
        in ecode e3 `shouldBe` "{x_ + 2, y_}[n_]"
      let e1 = Apply "+" [Var "x", liti 2]
          e2 = Apply "to_matrix" [Apply "{}" [e1]]
          e3 = Apply "[]" [Apply "[]" [e2, liti 1], liti 2]
          e4 = Apply "'" [e3]
          e4a = Apply "[]" [Apply "to_vector" [Var "v"], Var "n"]
          e5 = Apply "^" [e4, e4a]
        in ecode e5 `shouldBe` "to_matrix({x_ + 2})[1][2]' ^ to_vector(v_)[n_]"
      let e1 = Apply "negate" [Apply "^" [Var "x", Var "n"]]
          e1a = Apply "!" [Apply "^" [Var "y", Var "m"]]
          e2 = Apply ".*" [e1, e1a]
          e3 = Apply "./" [e2, Apply "!" [liti 1]]
        in ecode e3 `shouldBe` "-(x_ ^ n_) .* !(y_ ^ m_) ./ !1"
      let e1 = Apply ".*" [Var "x", Var "y"]
          e1a = Apply "./" [Var "v", Var "w"]
          e2 = Apply "\\" [e1, e1a]
          e2a = Apply "\\" [Var "a", Var "b"]
          e2b = Apply "\\" [Var "c", Var "d"]
          e3 = Apply "*" [e2, e2a]
          e4 = Apply "/" [e3, e2b]
          e5 = Apply "%" [e4, e2a]
        in ecode e5 `shouldBe`
           "x_ .* y_ \\ v_ ./ w_ * a_ \\ b_ / c_ \\ d_ % a_ \\ b_"
      let e1 = Apply "*" [Var "a", liti 2]
          e1a = Apply "/" [Var "b", liti 2]
          e1b = Apply "%" [Var "c", liti 1]
          e2 = Apply "-" [Apply "+" [e1, e1a], e1b]
        in ecode e2 `shouldBe` "a_ * 2 + b_ / 2 - c_ % 1"
      let e1a = Apply "+" [Var "x", liti 1]
          e1b = Apply "-" [Var "y", liti 2]
          e1c = Apply "+" [Var "z", liti 1]
          e1d = Apply "-" [Var "w", liti 2]
          e1e = Apply "+" [Var "v", liti 1]
          e2a = Apply "<" [e1a, e1b]
          e2b = Apply "<=" [e2a, e1c]
          e2c = Apply ">" [e2b, e1d]
          e2d = Apply ">=" [e2c, e1e]
        in ecode e2d `shouldBe` "x_ + 1 < y_ - 2 <= z_ + 1 > w_ - 2 >= v_ + 1"
      let e1a = Apply "<" [Var "a", Var "b"]
          e1b = Apply "<=" [Var "c", Var "d"]
          e1c = Apply ">" [Var "e", liti 1]
          e1d = Apply ">=" [Var "f", liti 2]
          e2a = Apply "!=" [e1a, e1b]
          e2b = Apply "==" [e2a, e1c]
          e3  = Apply "&&" [e2b, Apply "==" [Var "x", e1d]]
        in ecode e3 `shouldBe` "a_ < b_ != c_ <= d_ == e_ > 1 && x_ == f_ >= 2"
      let e1a = Apply "&&" [Var "a", Var "b"]
          e2a = Apply "&&" [Var "x", Var "y"]
          e3 = Apply "||" [e1a, e2a]
        in ecode e3 `shouldBe` "a_ && b_ || x_ && y_"
    it "Inserts parentheses when operator precedence requires" $ do
      let e1 = Apply "'" [e2]
          e2 = Apply "^" [e3a, e3b]
          e3a = Apply "negate" [Var "x"]
          e3b = Apply "!" [liti 1]
        in ecode e1 `shouldBe` "((-x_) ^ (!1))'"
      let e1 = Apply "!" [e2]
          e2 = Apply ".*" [Var "x", e3]
          e3 = Apply "./" [Var "y", liti 0]
        in ecode e1 `shouldBe` "!(x_ .* (y_ ./ 0))"
      let e1 = Apply "negate" [Apply ".*" [Var "z", e2]]
          e2 = Apply "./" [Var "y", e3]
          e3 = Apply ".*" [Var "x", liti 1]
        in ecode e1 `shouldBe` "-(z_ .* (y_ ./ (x_ .* 1)))"
      let e1a = Apply "*" [liti 1, Var "a"]
          e1b = Apply "/" [liti 2, Var "b"]
          e1c = Apply "%" [Var "c", liti 2]
          e2 = Apply "\\" [e1a, e1b]
          e3 = Apply "\\" [e1c, e2]
        in ecode e3 `shouldBe` "(c_ % 2) \\ ((1 * a_) \\ (2 / b_))"
      let e1 = Apply "*" [e2a, Apply "/" [e2b, Apply "%" [e2c, e2d]]]
          e2a = Apply "+" [liti 2, liti 3]
          e2b = Apply "-" [Var "a", Var "b"]
          e2c = Apply "+" [Var "c", Var "d"]
          e2d = Apply "-" [liti 0, liti 5]
        in ecode e1 `shouldBe` "(2 + 3) * ((a_ - b_) / ((c_ + d_) % (0 - 5)))"
      let e1 = Apply "-" [e2a, Apply "-" [e2b, Apply "+" [e2c, e2d]]]
          e2a = Apply "<" [Var "a", Var "b"]
          e2b = Apply "<=" [Var "c", liti 2]
          e2c = Apply ">" [Var "d", liti 3]
          e2d = Apply ">=" [liti 0, Var "e"]
        in ecode e1 `shouldBe`
           "(a_ < b_) - ((c_ <= 2) - ((d_ > 3) + (0 >= e_)))"
      let e1 = Apply "<"
               [e2a, Apply "<=" [e2b, Apply ">" [e2c, Apply ">=" [e2d, e2e]]]]
          e2a = Apply "==" [Var "a", liti 1]
          e2b = Apply "!=" [Var "b", liti 2]
          e2c = Apply "==" [Var "c", liti 3]
          e2d = Apply "!=" [Var "d", liti 4]
          e2e = Apply "==" [Var "e", liti 5]
        in ecode e1 `shouldBe`
           "(a_ == 1) < ((b_ != 2) <= ((c_ == 3) > ((d_ != 4) >= (e_ == 5))))"
      let e1 = Apply "==" [e2a, Apply "!=" [e2b, e2c]]
          e2a = Apply "&&" [Var "a", Var "b"]
          e2b = Apply "&&" [Var "c", Var "d"]
          e2c = Apply "&&" [Var "e", Var "f"]
        in ecode e1 `shouldBe` "(a_ && b_) == ((c_ && d_) != (e_ && f_))"
      let e1 = Apply "&&" [e2a, Apply "&&" [e2b, e2c]]
          e2a = Apply "||" [Var "a", Var "b"]
          e2b = Apply "||" [Var "c", Var "d"]
          e2c = Apply "||" [Var "e", Var "f"]
        in ecode e1 `shouldBe` "(a_ || b_) && ((c_ || d_) && (e_ || f_))"
                   
test_bcode :: Spec
test_bcode = 
  describe "bcode" $ do
    it "Codes no-bounds" $
      bcode nobounds `shouldBe` ""
    it "Codes lower bound" $ do
      bcode (lobound $ Apply "+" [Var "a", Var "b"]) `shouldBe`
        "<lower = a_ + b_>"
      bcode (lobound $ Apply "<" [Var "x", Var "y"]) `shouldBe`
        "<lower = (x_ < y_)>"
    it "Codes upper bound" $ do
      bcode (hibound $ Apply "+" [Var "a", Var "b"]) `shouldBe`
        "<upper = a_ + b_>"
      bcode (hibound $ Apply "<" [Var "x", Var "y"]) `shouldBe`
        "<upper = (x_ < y_)>"
    it "Codes lower/upper bound" $ do
      bcode (bounds (Apply "-" [Var "a", Var "b"])
                    (Apply "+" [Var "a", Var "b"]))
        `shouldBe` "<lower = a_ - b_, upper = a_ + b_>"
      bcode (bounds (Apply "<" [Var "x", Var "y"])
                    (Apply ">" [Var "x", Var "y"]))
        `shouldBe` "<lower = (x_ < y_), upper = (x_ > y_)>"

test_etcode :: Spec
test_etcode =
  describe "etcode" $ do
    it "Handles no-bounds case" $
      let cases =
            [ (IntType, "int"), (RealType, "real")
            , (VectorType (Var "n"), "vector[n_]")
            , (RowVectorType (Var "m"), "row_vector[m_]")
            , (MatrixType (Var "m") (Var "n"), "matrix[m_, n_]")
            ]
      in forM_ cases $ \(et, str) -> etcode et nobounds `shouldBe` str
    it "Handles non-trivial expressions" $
      let cases =
            [ (VectorType (Apply "+" [Apply "*" [liti 3, Var "n"], liti 2]),
               "vector[3 * n_ + 2]")
            , (RowVectorType (Apply "-" [Var "m", liti 3]),
               "row_vector[m_ - 3]")
            , (MatrixType (Apply "!" [Var "m"]) (Apply "/" [Var "n", liti 2]),
               "matrix[!m_, n_ / 2]")
            ]
      in forM_ cases $ \(et, str) -> etcode et nobounds `shouldBe` str
    it "Handles bounds" $
      let cases =
            [ (VectorType (Var "n"), lobound (Apply "+" [Var "x", liti 2]),
               "vector<lower = x_ + 2>[n_]")
            , (RowVectorType (Var "n"), hibound (Apply "-" [liti 7, liti 3]),
               "row_vector<upper = 7 - 3>[n_]")
            , (MatrixType (Var "m") (Var "n"), bounds (liti 0) (liti 1),
               "matrix<lower = 0, upper = 1>[m_, n_]")
            ]
      in forM_ cases $ \(et, bnd, str) -> etcode et bnd `shouldBe` str

test_decode :: Spec
test_decode =
  describe "decode" $ do
    it "has no truncation" $
      decode (DistrExpr "normal" [liti 0, liti 1] nobounds) `shouldBe`
      "normal(0, 1)"
    it "has upper truncation" $
      decode (DistrExpr "exponential" [Var "mu"] (hibound (Var "hi")))
      `shouldBe`
      "exponential(mu_) T[, hi_]"
    it "has lower truncation" $
      decode (DistrExpr "cauchy" [Var "scale"] (lobound $ liti 0))
      `shouldBe`
      "cauchy(scale_) T[0, ]"
    it "has two-sided truncation" $
      decode (DistrExpr "gamma" [Var "a", Var "b"] (bounds (liti 1) (liti 2)))
      `shouldBe`
      "gamma(a_, b_) T[1, 2]"

test_stan_var_decl_def :: Spec
test_stan_var_decl_def =
  describe "stan_var_decl and stan_var_def" $ do
    let test_cases ::
          HasCallStack => [(VarName, (TypeS, Bounds), Expr, String, String)]
          -> Expectation
        test_cases cases =
          forM_ cases $ \(v, typ, e, str1, str2) -> do
                          stan_var_decl (v, typ) `shouldBe` str1
                          stan_var_def ((v, typ), e) `shouldBe` str2
    it "Handles non-arrays" $
      let cases =
            [ ( "x"
              , (Type IntType [], nobounds)
              , Apply "/" [Var "m", liti 2]
              , "int x_;"
              , "int x_ = m_ / 2;")
            , ( "y"
              , (Type RealType [], lobound $ liti 0)
              , Apply "negate" [litr $ scientific 237 (-3)]
              , "real<lower = 0> y_;"
              , "real<lower = 0> y_ = -0.237;")
            , ( "z"
              , (Type (VectorType $ Var "n") [], hibound $ liti 7)
              , Apply "to_vector" [Apply "{}" [Var "a", Var "b"]]
              , "vector<upper = 7>[n_] z_;"
              , "vector<upper = 7>[n_] z_ = to_vector({a_, b_});")
            , ( "M"
              , (Type (MatrixType (Var "m") (Var "n")) [],
                 bounds (liti 0) (liti 1))
              , Apply ".*" [Var "A", Var "B"] 
              , "matrix<lower = 0, upper = 1>[m_, n_] M_;"
              , "matrix<lower = 0, upper = 1>[m_, n_] M_ = A_ .* B_;")
            ]
      in test_cases cases
    it "Handles 1D arrays" $
      let cases =
            [ ( "x"
              , (Type IntType [Apply "+" [Var "n", liti 2]], nobounds)
              , Apply "{}" [liti 3, liti 1, liti 0]
              , "int x_[n_ + 2];"
              , "int x_[n_ + 2] = {3, 1, 0};")
            , ( "y"
              , (Type (VectorType $ Var "n") [Var "k"], lobound $ liti 0)
              , Apply "{}" [Var "y1", Var "y2", Var "y3"]
              , "vector<lower = 0>[n_] y_[k_];"
              , "vector<lower = 0>[n_] y_[k_] = {y1_, y2_, y3_};")
            ]
      in test_cases cases
    it "Handles 2D arrays" $
      let cases =
            [ ( "x"
              , (Type RealType [Apply "+" [Var "n", liti 2], Var "N"],
                 nobounds)
              , Apply "+" [Var "y", litr 3]
              , "real x_[n_ + 2, N_];"
              , "real x_[n_ + 2, N_] = y_ + 3.0;")
            , ( "y"
              , (Type (MatrixType (Var "m") (Var "n"))
                      [Var "k", Apply "*" [liti 3, Var "k"]],
                 hibound $ Var "top")
              , Var "z"
              , "matrix<upper = top_>[m_, n_] y_[k_, 3 * k_];"
              , "matrix<upper = top_>[m_, n_] y_[k_, 3 * k_] = z_;")
            ]
      in test_cases cases

test_modelblock :: Spec
test_modelblock =
  describe "modelblock" $ do
    it "handles general case" $
      let ssm = SSM{ vecZ = Var "Z", scalH = Var "H"
                   , matT = Var "T", matQ = Var "Q"
                   , veca0 = Apply "rep_vector" [liti 0, liti 2]
                   , matP0 = Apply "diag_matrix"
                             [Apply "{}" [Var "pva", Var "pvb"]]
                   }
          defs = [ ( ("T", Type (MatrixType (liti 2) (liti 2)) [])
                   , Apply "to_matrix"
                     [Apply "{}" [ Apply "{}" [liti 1, liti 1]
                                 , Apply "{}" [liti 0, liti 1]]]
                   )
                 , ( ("Z", Type (VectorType (liti 2)) [])
                   , Apply "rep_vector" [liti 1, liti 2]
                   )
                 , ( ("H", Type RealType [])
                   , Apply "square" [Var "hsd"]
                   )
                 , ( ("Q", Type (MatrixType (liti 2) (liti 2)) [])
                   , Apply "diag_matrix"
                     [Apply "{}" [Var "sigma2_a", Var "sigma2_b"]]
                   )
                 ]
          draws = [ ("hsd", DistrExpr "normal" [liti 0, Var "scale_h"]
                          (lobound $ liti 0))
                  , ("sigma_a", distrExpr "exponential"
                                [Apply "inv" [Var "scale_a"]])
                  , ("sigma_b", DistrExpr "cauchy" [liti 0, Var "scale_b"]
                                (lobound $ liti 0))
                  ]
          model = StanModelBlock { medefs = defs, medraws = draws, messm = ssm }
      in
        modelblock model `shouldBe`
        [ "matrix[2, 2] T_ = to_matrix({{1, 1}, {0, 1}});"
        , "vector[2] Z_ = rep_vector(1, 2);"
        , "real H_ = square(hsd_);"
        , "matrix[2, 2] Q_ = diag_matrix({sigma2_a_, sigma2_b_});"
        , "hsd_ ~ normal(0, scale_h_) T[0, ];"
        , "sigma_a_ ~ exponential(inv(scale_a_));"
        , "sigma_b_ ~ cauchy(0, scale_b_) T[0, ];"
        , "yy ~ gaussian_dlm_obs(to_matrix(Z_), T_, rep_vector(H_, 1), Q_, rep_vector(0, 2), " ++
          "diag_matrix({pva_, pvb_}));"
        ]

test_function_defs :: Spec
test_function_defs =
  describe "function_defs" $ do
    it "Empty output" $ do
      function_defs [] `shouldBe` []
      function_defs ["^", "cos", "sin"] `shouldBe` []
    -- The remaining tests are more like regression tests, as there is
    -- more than one acceptable result.
    it "No dependencies" $ do
      function_defs ["sum_nrows"] `shouldBe`
        [[ "int sum_nrows(matrix [] M) {"
         , "  int n = 0;"
         , "  int N = size(M);"
         , "  for (i in 1:N)"
         , "    n += rows(M[i]);"
         , "  return n;"
         , "}"
         ]]
      function_defs ["sum_ncols", "sum_lengths"] `shouldBe`
        [ [ "int sum_ncols(matrix [] M) {"
          , "  int n = 0;"
          , "  int N = size(M);"
          , "  for (i in 1:N)"
          , "    n += cols(M[i]);"
          , "  return n;"
          , "}"
          ]
        , [ "int sum_lengths(vector [] va) {"
          , "  int n = 0;"
          , "  for (i in 1:size(va))"
          , "    n += num_elements(va[i]);"
          , "  return n;"
          , "}"
          ] ]
    it "With dependencies" $ do
      function_defs ["vec_append"] `shouldBe`
        [ [ "int sum_lengths(vector [] va) {"
          , "  int n = 0;"
          , "  for (i in 1:size(va))"
          , "    n += num_elements(va[i]);"
          , "  return n;"
          , "}"
          ]
        , [ "vector vec_append(vector [] va) {"
          , "  int n = size(va);"
          , "  if (n == 0) return rep_vector(0.0, 0);"
          , "  if (n == 1) return va[1];"
          , "  {"
          , "    int sz = sum_lengths(va);"
          , "    vector[sz] v;"
          , "    int bi;"
          , "    int ei = 0;"
          , "    for (k in 1:n) {"
          , "      bi = ei + 1;"
          , "      ei += num_elements(va[k]);"
          , "      v[bi:ei] = va[k];"
          , "    }"
          , "    return v;"
          , "  }"
          , "}"
          ] ]
    it "Does not repeat functions" $ do
      (function_defs $ Map.keys extra_fcts_map) `shouldSatisfy` allUnique

test_function_defs_long :: Spec
test_function_defs_long =
  describe "function_defs_long" $ do
    it "compiles and runs" $ do
      withSystemTempFile "temp.stan" $ \path_s handle_s -> do
        hPutStr handle_s extra_fcts_stanprog
        hClose handle_s
        withSystemTempFile "temp.R" $ \path_r handle_r -> do
          hPutStr handle_r r_code
          hClose handle_r
          let cmd = printf "Rscript --vanilla %s %s" path_r path_s
          (exit_code, _out, _err) <-
            P.readCreateProcessWithExitCode (P.shell cmd) ""
          when (exit_code /= ExitSuccess) $
            print_output _out _err
          exit_code `shouldBe` ExitSuccess
  where
    print_output out err = do
      putStrLn "--- STDOUT ---"
      putStrLn out
      putStrLn "--- STDERR ---"
      putStrLn err
      putStrLn "--- extra_fcts_stanprog ---"
      putStr extra_fcts_stanprog
    r_code = [s|
library(rstan)
sm <- stan_model(commandArgs(trailingOnly = TRUE))
expose_stan_functions(sm)

cat("Starting R tests\n", file=stdout())

mat1 <- rbind(1:2, 3:4)
mat2 <- rbind(7:8, 5:6)
result <- scale_matrices(2, list(mat1, mat2))
expected <- list(2 * mat1, 2 * mat2)
stopifnot(identical(result, expected))

mat1 <- matrix(0, nrow=3, ncol=5)
mat2 <- matrix(0, nrow=2, ncol=1)
mat3 <- matrix(0, nrow=1, ncol=6)
stopifnot(identical(sum_nrows(list()), 0L))
stopifnot(identical(sum_nrows(list(mat1)), 3L))
stopifnot(identical(sum_nrows(list(mat1, mat2, mat3)), 6L))
stopifnot(identical(sum_ncols(list()), 0L))
stopifnot(identical(sum_ncols(list(mat1)), 5L))
stopifnot(identical(sum_ncols(list(mat1, mat2, mat3)), 12L))

mat1 <- rbind(5, 7)
mat2 <- cbind(2, 6)
mat3 <- rbind(4:5, 8:9)
stopifnot(identical(block_diag(list()), matrix(0, nr=0, nc=0)))
stopifnot(identical(block_diag(list(mat1)), mat1))
result <- block_diag(list(mat1, mat2, mat3))
expected <- rbind(c(5,0,0,0,0),
                  c(7,0,0,0,0),
                  c(0,2,6,0,0),
                  c(0,0,0,4,5),
                  c(0,0,0,8,9))
stopifnot(identical(result, expected))

stopifnot(identical(sum_lengths(list()), 0L))
stopifnot(identical(sum_lengths(list(c(1:2))), 2L))
stopifnot(identical(sum_lengths(list(c(1), c(2:4), c(5:6))), 6L))

stopifnot(identical(vec_append(list()), numeric(0)))
vec1 <- as.double(1:3)
stopifnot(identical(vec_append(list(vec1)), vec1))
vec2 <- as.double(4)
vec3 <- as.double(5:9)
stopifnot(identical(vec_append(list(vec1, vec2, vec3)), as.double(1:9)))

xs <- c(0.5, 1, 1.7)
coeffs <- c(5.8, -3.6, 12.8, 4.2)
for (x in xs) {
  y <- polynomial(x, coeffs)
  expected <- coeffs[1] + coeffs[2] * x + coeffs[3] * x^2 + coeffs[4] * x^3
  stopifnot(isTRUE(all.equal(y, expected)))
}

xs <- c(2e-3, 4.5e-2, 4.6e-2, 2.10e-1, 2.11e-1,
        seq(5e-2, 0.5, by=5e-2))
xs <- c(xs, 1-xs)
xs[abs(xs - 0.5) < 1e-2] <- 0.5
stopifnot(any(xs == 0.5))
meanfct <- function(th) {
  f <- function(x){th * exp(-th*x)}
  Z <- cubature::adaptIntegrate(f, 0, 1)$integral
  g <- function(x){x * f(x)}
  cubature::adaptIntegrate(g, 0, 1)$integral / Z
}
for (x in xs) {
  y <- exponential_mt_rate(x)
  mu <- meanfct(y)
  stopifnot(abs(mu - x)/x <= 1e-6)
}

mat00 <- matrix(0.0, nr=0, nc=0)
stopifnot(identical(mat00, blocks4(mat00, mat00, mat00, mat00)))

mat10 <- matrix(0.0, nr=1, nc=0)
mat01 <- matrix(0.0, nr=0, nc=1)
mat11 <- matrix(5.0, nr=1, nc=1)
stopifnot(identical(mat11, blocks4(mat00, mat01, mat10, mat11)))
stopifnot(identical(mat11, blocks4(mat11, mat10, mat01, mat00)))

stopifnot(identical(rbind(1:3, 4:6, 7:9)+0.0,
                    blocks4(
                      matrix(c(1,2), nr=1, nc=2),
                      matrix(3, nr=1, nc=1)+0.0,
                      matrix(c(4,5, 7,8), nr=2, nc=2, byrow=TRUE),
                      matrix(c(6,9), nr=2, nc=1))))
|]

test_used_fcts :: Spec
test_used_fcts =
  describe "used_fcts" $ do
    let arr es = Apply "{}" es
        diagmat e = Apply "diag_matrix" [e]
        repvec e n = Apply "rep_vector" [e, n]
        times e1 e2 = Apply "*" [e1, e2]
        plus e1 e2 = Apply "+" [e1, e2]
        minus e1 e2 = Apply "-" [e1, e2]
        pow e1 e2 = Apply "^" [e1, e2]
        one = liti 1
        zero = liti 0
        ssm0 = SSM zero zero zero zero zero zero
        model0 = StanModelBlock { medefs = [], medraws = [], messm = ssm0 }
        x0 = StanAST { stdata = [], stxdata = [], stparms = [], stxparms = []
                     , stmodel = model0 }
    it "no results" $
      used_fcts x0 `shouldBe` ["rep_vector", "to_matrix"]
    it "model block only, SSM only" $
      let ssm = SSM{ vecZ = repvec one (times one (Var "n"))
                   , scalH = (Var "h")
                   , matT = diagmat (repvec (Var "phi") (Var "n"))
                   , matQ = diagmat (repvec (Var "q") (Var "n"))
                   , veca0 = repvec zero (Var "n")
                   , matP0 = diagmat (repvec (Var "sigmap") (Var "n"))
                   }
          x = x0{ stmodel = model0{ messm = ssm } }
      in used_fcts x `shouldBe` ["*", "diag_matrix", "rep_vector", "to_matrix"]
    it "model block only, let and draw" $
      let x = x0{ stmodel = StanModelBlock
                  { medefs =
                    [( ( "v"
                       , Type (VectorType (plus (Var "n") one))
                              [Apply "size" [Var "foo"]]
                       )
                     , Apply "vec_append" [arr [Var "bar"]]
                     )]
                  , medraws =
                    [( "x"
                     , DistrExpr "normal" [zero, one]
                       (lobound $ minus (Var "k") one)
                     )]
                  , messm = ssm0
                  }
                }
      in used_fcts x `shouldBe`
         [ "+", "-", "normal", "rep_vector", "size", "to_matrix", "vec_append", "{}" ]
    it "includes functions used in data block" $
      let x = x0{ stdata = [ ("a", (Type (VectorType (minus one zero))
                                         [Apply "max" [one, zero]],
                                    lobound (Apply "abs" [zero])))
                           , ("b", (Type IntType [pow one one, one],
                                    nobounds))
                           ]
                }
      in used_fcts x `shouldBe` [ "-", "^", "abs", "max", "rep_vector", "to_matrix"]
    it "includes functions used in transformed data block" $
      let x = x0{ stxdata = [ ( ("a", (Type (VectorType (minus one zero))
                                            [Apply "max" [one, zero]],
                                       lobound (Apply "abs" [zero])))
                              , Apply "to_vector" [one]
                              )
                            , ( ("b", (Type IntType [pow one one, one],
                                       nobounds))
                              , arr [Apply "%" [liti 8, liti 2]]
                              )
                            ]
                }
      in used_fcts x `shouldBe`
         [ "%", "-", "^", "abs", "max", "rep_vector", "to_matrix", "to_vector", "{}" ]
    it "includes functions used in parameters block" $
      let x = x0{ stparms = [ ("a", (Type (VectorType (minus one zero))
                                          [Apply "max" [one, zero]],
                                     lobound (Apply "abs" [zero])))
                            , ("b", (Type IntType [pow one one, one],
                                     nobounds))
                            ]
                }
      in used_fcts x `shouldBe` [ "-", "^", "abs", "max", "rep_vector", "to_matrix"]
    it "includes functions used in transformed parameters block" $
      let x = x0{stxparms = [ ( ("a", (Type (VectorType (minus one zero))
                                            [Apply "max" [one, zero]],
                                       lobound (Apply "abs" [zero])))
                              , Apply "to_vector" [one]
                              )
                            , ( ("b", (Type IntType [pow one one, one],
                                       nobounds))
                              , arr [Apply "%" [liti 8, liti 2]]
                              )
                            ]
                }
      in used_fcts x `shouldBe`
         [ "%", "-", "^", "abs", "max", "rep_vector", "to_matrix", "to_vector", "{}" ]

test_compileStanAST :: Spec
test_compileStanAST =
  describe "compileStanAST" $ do
    let r0 = litr 0
        i0 = liti 0
        sqr e = Apply "square" [e]
        vec0 = Apply "rep_vector" [r0, i0]
        mat00 = Apply "rep_matrix" [r0, i0, i0]
        times es = foldl1 (\e1 e2 -> Apply "*" [e1, e2]) es
        to_matrix e = Apply "to_matrix" [e]
        to_vector e = Apply "to_vector" [e]
        diag_matrix e = Apply "diag_matrix" [e]
        arr es = Apply "{}" es
        pos_real = (Type RealType [], lobound r0)
        unit_real = (Type RealType [], bounds (litr 0) (litr 1))
        all_real = (Type RealType [], nobounds)
        all_vector n = (Type (VectorType n) [], nobounds)
        all_matrix nr nc = (Type (MatrixType nr nc) [], nobounds)
        ub_vector e = Type (VectorType e) []
        ub_matrix nr nc = Type (MatrixType nr nc) []
    it "Compiles a minimal program" $
      let st = StanAST
            { stdata = []
            , stxdata = []
            , stparms = [("sigma", pos_real)]
            , stxparms = []
            , stmodel = StanModelBlock
              { medefs = []
              , medraws =
                [( "sigma"
                 , DistrExpr "normal" [litr 0, litr 1] (lobound $ litr 0))]
              , messm = SSM
                { vecZ = vec0
                , scalH = (sqr $ Var "sigma")
                , matT = mat00
                , matQ = mat00
                , veca0 = vec0
                , matP0 = mat00 }
              }
            }
          expected = [s|
functions {
}
data {
  int<lower = 1> N;
  vector[N] y;
}
transformed data {
  matrix[1, N] yy = to_matrix(y');
}
parameters {
  real<lower = 0.0> sigma_;
}
transformed parameters {
}
model {
  sigma_ ~ normal(0.0, 1.0) T[0.0, ];
  yy ~ gaussian_dlm_obs(to_matrix(rep_vector(0.0, 0)), rep_matrix(0.0, 0, 0), rep_vector(square(sigma_), 1), rep_matrix(0.0, 0, 0), rep_vector(0.0, 0), rep_matrix(0.0, 0, 0));
}
|]
      in do
        --putStrLn $ compileStanAST st
        compileStanAST st `shouldBe` expected
    
    it "Compiles local linear trend" $
      let st = StanAST
            { stdata =
              [ ("scale_sigma_eps", pos_real)
              , ("scale_sigma_eta_level", unit_real)
              , ("scale_sigma_eta_trend", unit_real)
              , ("mu_level", all_real)
              , ("sigma_level", pos_real)
              , ("sigma_trend", pos_real)
              ]
            , stxdata =
              [ (("Zllt", all_vector (liti 2)),
                 to_vector (arr [litr 1, litr 0]))
              , (("Tllt", all_matrix (liti 2) (liti 2)),
                 to_matrix(arr [arr [litr 1, litr 1], arr [litr 0, litr 1]]))
              ]
            , stparms =
              [ ("sigma_eps_raw", pos_real)
              , ("sigma_eta_level_raw", pos_real)
              , ("sigma_eta_trend_raw", pos_real)
              ]
            , stxparms =
              [ (("sigma_eps", pos_real),
                  times [Var "scale_sigma_eps", Var "sigma_eps_raw"])
              , (("sigma_eta_level", pos_real),
                  times [Var "sigma_eps", Var "scale_sigma_eta_level",
                         Var "sigma_eta_level_raw"])
              , (("sigma_eta_trend", pos_real),
                  times [Var "sigma_eta_level", Var "scale_sigma_eta_trend",
                         Var "sigma_eta_trend_raw"])
              ]
            , stmodel = StanModelBlock
              { medefs =
                  [ (("H", Type RealType []),
                      (sqr $ Var "sigma_eps"))
                  , (("Qllt", ub_matrix (liti 2) (liti 2)),
                     diag_matrix $ to_vector $ arr
                     [sqr $ Var "sigma_eta_level", sqr $ Var "sigma_eta_trend"])
                  , (("P0llt", ub_matrix (liti 2) (liti 2)),
                     diag_matrix $ to_vector $ arr
                     [sqr $ Var "sigma_level", sqr $ Var "sigma_trend"])
                  , (("a0llt", ub_vector (liti 2)),
                     to_vector (arr [Var "mu_level", litr 0]))
                  ]
              , medraws =
                [ ("sigma_eps_raw",
                    distrExpr "exponential" [litr 1])
                , ("sigma_eta_level_raw",
                   DistrExpr "normal" [litr 0, litr 1] (lobound r0))
                , ("sigma_eta_trend_raw",
                   DistrExpr "cauchy" [litr 0, litr 1] (lobound r0))
                ]
              , messm = SSM
                { vecZ = Var "Zllt"
                , scalH = Var "H"
                , matT = Var "Tllt"
                , matQ = Var "Qllt"
                , veca0 = Var "a0llt"
                , matP0 = Var "P0llt"
                }
              }
            }
          expected = [s|
functions {
}
data {
  int<lower = 1> N;
  vector[N] y;
  real<lower = 0.0> scale_sigma_eps_;
  real<lower = 0.0, upper = 1.0> scale_sigma_eta_level_;
  real<lower = 0.0, upper = 1.0> scale_sigma_eta_trend_;
  real mu_level_;
  real<lower = 0.0> sigma_level_;
  real<lower = 0.0> sigma_trend_;
}
transformed data {
  matrix[1, N] yy = to_matrix(y');
  vector[2] Zllt_ = to_vector({1.0, 0.0});
  matrix[2, 2] Tllt_ = to_matrix({{1.0, 1.0}, {0.0, 1.0}});
}
parameters {
  real<lower = 0.0> sigma_eps_raw_;
  real<lower = 0.0> sigma_eta_level_raw_;
  real<lower = 0.0> sigma_eta_trend_raw_;
}
transformed parameters {
  real<lower = 0.0> sigma_eps_ = scale_sigma_eps_ * sigma_eps_raw_;
  real<lower = 0.0> sigma_eta_level_ = sigma_eps_ * scale_sigma_eta_level_ * sigma_eta_level_raw_;
  real<lower = 0.0> sigma_eta_trend_ = sigma_eta_level_ * scale_sigma_eta_trend_ * sigma_eta_trend_raw_;
}
model {
  real H_ = square(sigma_eps_);
  matrix[2, 2] Qllt_ = diag_matrix(to_vector({square(sigma_eta_level_), square(sigma_eta_trend_)}));
  matrix[2, 2] P0llt_ = diag_matrix(to_vector({square(sigma_level_), square(sigma_trend_)}));
  vector[2] a0llt_ = to_vector({mu_level_, 0.0});
  sigma_eps_raw_ ~ exponential(1.0);
  sigma_eta_level_raw_ ~ normal(0.0, 1.0) T[0.0, ];
  sigma_eta_trend_raw_ ~ cauchy(0.0, 1.0) T[0.0, ];
  yy ~ gaussian_dlm_obs(to_matrix(Zllt_), Tllt_, rep_vector(H_, 1), Qllt_, a0llt_, P0llt_);
}
|]
      in do
        compileStanAST st `shouldBe` expected

