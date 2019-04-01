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
module BasicModelSpec (main, spec) where

import Control.Monad (forM_)
import qualified Data.Set as Set
import Test.Hspec
--import Test.QuickCheck
--import Test.Hspec.QuickCheck (prop)
--import TestUtils

import AST(ElemType(..))
import BasicExpr
import BasicModel
import NameGen (ContainsVars(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  test_vars_in

test_vars_in :: Spec
test_vars_in =
  describe "vars_in" $ do
    it "works for TypeS" $
      forM_ [IntType, RealType] $ \et-> do
        vars_in (TypeS et []) `shouldBe` Set.fromList []
        vars_in (TypeS et [Var "x", Lit (IntVal 3), Var "y"])
          `shouldBe` Set.fromList ["x", "y"]
    it "works for TypeSB" $
      forM_ [IntType, RealType] $ \et-> do
        vars_in (TypeSB et [] nobounds) `shouldBe` Set.fromList []
        vars_in (TypeSB et [Var "x", Lit (IntVal 3), Var "y"] nobounds)
          `shouldBe` Set.fromList ["x", "y"]
        vars_in (TypeSB et [] $ bounds (Var "a") (Var "b"))
          `shouldBe` Set.fromList ["a", "b"]
    it "works for Decl t" $ do
      vars_in (Decl "x" ()) `shouldBe` Set.fromList ["x"]
      vars_in (Decl "a" (TypeS IntType [Var "n"])) `shouldBe`
        Set.fromList ["a", "n"]
    it "works for DistrExpr" $ do
      vars_in (DistrExpr "dname" [Var "alpha", Var "beta"]) `shouldBe`
        Set.fromList ["alpha", "beta"]

