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
module NameGenSpec (main, spec) where

import qualified Data.Set as Set
import Test.Hspec

import NameGen

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  test_ContainsVars
  test_newName
  test_applyNG

test_ContainsVars :: Spec
test_ContainsVars =
  describe "ContainsVars" $ do
    it "recurses structure" $ do
      let [a, b, c, d, e, f] = map Str ["a", "b", "c", "d", "e", "f"]
      let x = ((), [ Just (a, (b, c)), Just (d, (e, f)), Nothing ])
      vars_in x `shouldBe` Set.fromList ["a", "b", "c", "d", "e", "f"]

test_newName :: Spec
test_newName =
  describe "newName" $ do
    it "works" $ do
      let case1 = do
            x1 <- newName "x"
            y1 <- newName "y"
            x2 <- newName "x"
            y2 <- newName y1
            x3 <- newName x1
            y3 <- newName "y"
            return ((x1, x2, x3), (y1, y2, y3))
      let init0 = Set.empty
      let res0 = evalState case1 init0
      let init1 = Set.fromList ["x_1", "y_0"]
      let res1 = evalState case1 init1
      res0 `shouldBe` (("x_0", "x_1", "x_0_0"), ("y_0", "y_0_0", "y_1"))
      res1 `shouldBe` (("x_0", "x_2", "x_0_0"), ("y_1", "y_1_0", "y_2"))

test_applyNG :: Spec
test_applyNG =
  describe "applyNG" $ do
    it "works" $ do
      let strs = map Str ["a", "b", "c", "a_0", "c_1", "a", "b", "c"]
      let res = applyNG (mapM $ newName . unStr) strs
      res `shouldBe`
        ["a_1", "b_0", "c_0", "a_0_0", "c_1_0", "a_2", "b_1", "c_2" ]
