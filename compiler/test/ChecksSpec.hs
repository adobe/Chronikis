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
module ChecksSpec (main, spec) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Hspec

import AST
import Checks
import Env (baseFctEnv)
import Number
import ParseFctDef (parsefdt)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  test_checkProgram

test_checkProgram :: Spec
test_checkProgram =
  describe "checkProgram" $ do
    let dmain = FctDef { defName = "main"
                       , defParms = []
                       , defRetType = TSDType
                       , defBody = Var TSDType "x"
                       }
        d1    = FctDef { defName = ""
                       , defParms = []
                       , defRetType = RType
                       , defBody = Lit RType (RealVal 3.14)
                       }
        pitype = TypeSB{ tsbBase=IType, tsbShape=[], tsbBounds=nobounds }
        prtype = TypeSB{ tsbBase=RType, tsbShape=[], tsbBounds=nobounds }
        
    it "reports empty program" $
      checkProgram Set.empty [] `shouldBe` [ "No function 'main' defined" ]
      
    it "reports missing main function" $ do
      let d = dmain { defName = "foo" }
      checkProgram Set.empty [ d ] `shouldBe` [ "No function 'main' defined" ]
      
    it "reports wrong return type for main function" $ do
      let d = dmain { defRetType = RType, defBody = Lit RType (RealVal 3.14) }
      checkProgram Set.empty [ d ] `shouldBe`
        [ "Function 'main' return type must be real$~" ]
        
    it "reports illegal parameter type for main function" $ do
      let vptyp = TypeSB{ tsbBase=RDistrType, tsbShape=[], tsbBounds=nobounds }
          db = dmain { defParms = [ ("v", vptyp) ] }
      checkProgram Set.empty [ db ] `shouldBe`
        [ "Function 'main' cannot have a distribution parameter" ]
        
    it "reports multiple defs of same function name" $ do
      let da1 = d1 { defName = "a" }
          da2 = d1 { defName = "a", defBody = Lit RType (RealVal (-2.7)) }
          db  = d1 { defName = "b" }
      checkProgram Set.empty [ da1, db, da2, dmain ] `shouldBe`
        [ "Multiple definitions for function: a" ]
        
    it "reports multiple parameters with same name" $ do
      let da = d1 { defName = "foo"
                  , defParms = [ ("a", pitype), ("b", pitype), ("a", prtype) ] }
          db = d1 { defName = "bar"
                  , defParms = [ ("b", pitype), ("a", prtype), ("b", pitype)
                               , ("a", prtype) ]
                  }
          dc = d1 { defName = "baz" }
      checkProgram Set.empty [ da, db, dc, dmain ] `shouldBe`
        [ "Function 'foo' has multiple parameters with the same name: a"
        , "Function 'bar' has multiple parameters with the same name: a"
        , "Function 'bar' has multiple parameters with the same name: b"
        ]
        
    it "reports redefined functions" $ do
      let fnames = Set.singleton "foo"
          dfoo = d1 { defName = "foo" }
      checkProgram fnames [ dfoo, dmain ] `shouldBe`
        [ "Redefined function: foo" ]

    it "reports type mismatch between function body and declaration" $ do
      let d = FctDef { defName = "foo"
                     , defParms = []
                     , defRetType = IType
                     , defBody = Lit RType $ RealVal 0.9
                     }
      checkProgram Set.empty [ d, d{defName="bar"}, dmain] `shouldBe`
        [ "Function foo: body must have type int, has type real instead"
        , "Function bar: body must have type int, has type real instead"
        ]

    it "reports multiple defs of same variable in a function" $ do
      let fd = parsefdt (Map.empty, baseFctEnv)
                 "fip() : real =            \
                 \  x = (z = 1.0; 2.0 + z); \
                 \  y = (z = 5.0; x + 3.0); \
                 \  y + x"
      checkProgram Set.empty [ fd, dmain ] `shouldBe`
        [ "Function fip: Variable z multiply defined"
        ]
