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
module BasicExprSpec (main, spec) where

import qualified Data.Set as Set
import Test.Hspec
--import Test.QuickCheck
--import Test.Hspec.QuickCheck (prop)
--import TestUtils

import BasicExpr
import NameGen (ContainsVars(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  test_vars_in
  test_Bounds
  test_bmap

test_vars_in :: Spec
test_vars_in =
  describe "vars_in" $ do
    it "returns singleton for Var v" $
      vars_in (Var "foo") `shouldBe` Set.fromList ["foo"]
    it "returns empty for literals" $
      vars_in (Lit $ IntVal 37) `shouldBe` Set.fromList []
    it "handles apply" $
      vars_in (Apply "+"
               [Var "a", Lit (IntVal 0), Var "b"])
        `shouldBe`
        Set.fromList ["a", "b"]
    it "is defined for Bounds" $ do
      vars_in (Bounds Nothing Nothing) `shouldBe` Set.fromList []
      vars_in (Bounds Nothing (Just $ Var "b"))
        `shouldBe` Set.fromList ["b"]
      vars_in (Bounds (Just $ Var "a") Nothing)
        `shouldBe` Set.fromList ["a"]
      vars_in (Bounds (Just $ Var "a") (Just $ Var "b"))
        `shouldBe` Set.fromList ["a", "b"]

test_Bounds :: Spec
test_Bounds =
  describe "Bounds" $ do
    it "has nobounds ctor" $
      nobounds `shouldBe` Bounds Nothing Nothing
    it "has lobound ctor" $
      lobound (Var "x") `shouldBe` (Bounds (Just $ Var "x") Nothing)
    it "has hibound ctor" $
      hibound (Var "x") `shouldBe` (Bounds Nothing (Just $ Var "x"))
    it "has bounds ctor" $
      bounds (Var "x") (Var "y") `shouldBe`
        (Bounds (Just $ Var "x") (Just $ Var "y"))

test_bmap :: Spec
test_bmap =
  describe "bmap" $ do
    let f x = Apply "f" [x]
    let lo = Var "lo"
    let hi = Var "hi"
    it "handles Nothing/Nothing" $
      bmap f (Bounds Nothing Nothing) `shouldBe` Bounds Nothing Nothing
    it "handles Nothing/Just" $
      bmap f (Bounds Nothing (Just hi)) `shouldBe`
        Bounds Nothing (Just $ Apply "f" [hi])
    it "handles Just/Nothing" $
      bmap f (Bounds (Just lo) Nothing) `shouldBe`
        Bounds (Just $ Apply "f" [lo]) Nothing
    it "handles Just/Just" $
      bmap f (Bounds (Just lo) (Just hi)) `shouldBe`
        Bounds (Just $ Apply "f" [lo]) (Just $ Apply "f" [hi])

    
