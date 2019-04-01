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
module NumberSpec (main, spec) where

import Test.Hspec

import Number

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  test_is_representable_number

test_is_representable_number :: Spec
test_is_representable_number =
  describe "is_representable_number" $ do
    it "handles low integers" $ do
      is_representable_number (IntVal $ -(2^31 + 1)) `shouldBe` False
      is_representable_number (IntVal $ -(2^31)) `shouldBe` True
    it "handles high integers" $ do
      is_representable_number (IntVal $ 2^31 - 1) `shouldBe` True
      is_representable_number (IntVal $ 2^31) `shouldBe` False
    it "handles small reals" $ do
      is_representable_number (RealVal 0.0) `shouldBe` True
      let eps = 2 ^^ (-1022)
          eps' = eps - 2 ^^ (-1074)
      is_representable_number (RealVal eps) `shouldBe` True
      is_representable_number (RealVal eps') `shouldBe` False
      is_representable_number (RealVal (-eps)) `shouldBe` True
      is_representable_number (RealVal (-eps')) `shouldBe` False
    it "handles large reals" $ do
      let big = (2.0 - 2 ^^ (-52)) * 2 ^^ 1023 :: Scientific
          big' = 2.0 * 2 ^^ 1023 :: Scientific
      is_representable_number (RealVal big) `shouldBe` True
      is_representable_number (RealVal big') `shouldBe` False
      is_representable_number (RealVal (-big)) `shouldBe` True
      is_representable_number (RealVal (-big')) `shouldBe` False
      -- There is no Infinity for Scientific values
