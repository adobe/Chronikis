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
module RNamesSpec (main, spec) where

import Test.Hspec

import RNames

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "translates invalid chars to '.'" $ do
    makeRName "a`~!@#$%^&*()-+={[}]|\\:;\"'<,>?/b" `shouldBe`
              "a..............................b"
  it "prepends 'X' to invalid initial substring" $ do
    makeRName "" `shouldBe` "X"
    makeRName "_" `shouldBe` "X_"
    makeRName "_a" `shouldBe` "X_a"
    makeRName "5" `shouldBe` "X5"
    makeRName "5a" `shouldBe` "X5a"
    makeRName ".2" `shouldBe` "X.2"
    makeRName ".0a" `shouldBe` "X.0a"
    makeRName "!5" `shouldBe` "X.5"
  it "appends '.' to reserved words" $ do
    makeRName "if" `shouldBe` "if."
    makeRName "else" `shouldBe` "else."
    makeRName "repeat" `shouldBe` "repeat."
    makeRName "while" `shouldBe` "while."
    makeRName "function" `shouldBe` "function."
    makeRName "for" `shouldBe` "for."
    makeRName "in" `shouldBe` "in."
    makeRName "next" `shouldBe` "next."
    makeRName "break" `shouldBe` "break."
    makeRName "TRUE" `shouldBe` "TRUE."
    makeRName "FALSE" `shouldBe` "FALSE."
    makeRName "NULL" `shouldBe` "NULL."
    makeRName "Inf" `shouldBe` "Inf."
    makeRName "NaN" `shouldBe` "NaN."
    makeRName "NA" `shouldBe` "NA."
    makeRName "NA_integer_" `shouldBe` "NA_integer_."
    makeRName "NA_real_" `shouldBe` "NA_real_."
    makeRName "NA_complex_" `shouldBe` "NA_complex_."
    makeRName "NA_character_" `shouldBe` "NA_character_."
    makeRName "..." `shouldBe` "...." --
    makeRName "..0" `shouldBe` "..0." --
    makeRName "..13" `shouldBe` "..13." --
  it "leaves everything else unchanged" $ do
    makeRName "..13f" `shouldBe` "..13f"
    makeRName "...." `shouldBe` "...."
    makeRName ".foo_bar27." `shouldBe` ".foo_bar27."
