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
module EnvSpec (main, spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Test.Hspec

import AST
import Env
import Misc

-- Still figuring out what to do here.

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  test_baseFctEnv
  test_baseFctShEnv
  test_baseDistrShEnv
  test_baseVarEnv

test_baseFctEnv :: Spec
test_baseFctEnv =
  describe "baseFctEnv" $
    it "has expected signatures" $
      forM_ baseFctEnv_cases $ \(fname, argTypes, retType) -> do
        case Map.lookup fname baseFctEnv of
          Nothing ->
            expectationFailure ("Could not find " ++ fname)
          Just (Sigs sigfct) ->
            sigfct argTypes `shouldBe` Just retType

test_baseFctShEnv :: Spec
test_baseFctShEnv =
  describe "baseFctShEnv" $ return ()

test_baseDistrShEnv :: Spec
test_baseDistrShEnv =
  describe "baseDistrShEnv" $ return ()

test_baseVarEnv :: Spec
test_baseVarEnv =
  describe "baseVarEnv" $ do
    it "is empty" $
      baseVarEnv `shouldSatisfy` Map.null

baseFctEnv_cases:: [(FctName, [Type], Type)]
baseFctEnv_cases = [ ]
