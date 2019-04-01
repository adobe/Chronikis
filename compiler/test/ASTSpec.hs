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
module ASTSpec (main, spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Printf (printf)
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (prop)

--import TestUtils

import AST
import Env (baseFctEnv)
import Misc
import NameGen (ContainsVars(..))
import Number
import ParseFctDef (parsefd0)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  test_Bounds
  test_type_patterns
  test_show_Type
  test_fromET
  test_vars_in
  test_unrollVarDefs
  test_inferredVars
  test_Signatures
  test_typedExpr
  test_typedFctDef

test_Bounds :: Spec
test_Bounds =
  describe "Bounds" $ do
    it "has nobounds ctor" $
      nobounds `shouldBe` (Bounds Nothing Nothing :: Bounds ExprT)
    it "has lobound ctor" $
      lobound (Var () "x") `shouldBe` (Bounds (Just $ Var () "x") Nothing)
    it "has hibound ctor" $
      hibound (Var () "x") `shouldBe` (Bounds Nothing (Just $ Var () "x"))
    it "has bounds ctor" $
      bounds (Var () "x") (Var () "y") `shouldBe`
        (Bounds (Just $ Var () "x") (Just $ Var () "y"))

test_show_Type :: Spec
test_show_Type =
  describe "show :: Type -> String" $ do
    let cases =
          [ ( Type{elemType=IntType, numDim=0, isTS=False, isDistr=False}
            , "int")
          , (Type{elemType=RealType, numDim=0, isTS=False, isDistr=False}
            , "real")
          , ( Type{elemType=IntType, numDim=0, isTS=True, isDistr=False}
            , "int$")
          , ( Type{elemType=RealType, numDim=0, isTS=False, isDistr=True}
            , "real~")
          , (Type{elemType=IntType, numDim=1, isTS=False, isDistr=False}
            , "int@")
          , (Type{elemType=RealType, numDim=2, isTS=False, isDistr=False}
            , "real@@")
          , (Type{elemType=RealType, numDim=0, isTS=True, isDistr=True}
            , "real$~")
          , (Type{elemType=IntType, numDim=3, isTS=True, isDistr=False}
            , "int@@@$")
          , (Type{elemType=RealType, numDim=2, isTS=False, isDistr=True}
            , "real@@~")
          , (Type{elemType=RealType, numDim=1, isTS=True, isDistr=True}
            , "real@$~")
          ]
    it "works" $
      forM_ cases $ \(typ, str) -> show typ `shouldBe` str

test_type_patterns :: Spec
test_type_patterns =
  describe "Type patterns" $ do
    let f IType = "IType"
        f RType = "RType"
        f VecType = "VecType"
        f MatType = "MatType"
        f (RArrType n) = printf "RArrType %d" n
        f RDistrType = "RDistrType"
        f TSDType = "TSDType"
        f _ = "Other"
    it "matches IType" $ do
      let itype = Type{elemType=IntType, numDim=0, isTS=False, isDistr=False}
      f itype `shouldBe` "IType"
      f itype{numDim=1} `shouldBe` "Other"
      f itype{isTS=True} `shouldBe` "Other"
      f itype{isDistr=True} `shouldBe` "Other"
    it "matches RType" $
      f Type{elemType=RealType, numDim=0, isTS=False, isDistr=False}
        `shouldBe` "RType"
    it "matches array types" $ do
      f Type{elemType=RealType, numDim=1, isTS=False, isDistr=False}
        `shouldBe` "VecType"
      f Type{elemType=RealType, numDim=2, isTS=False, isDistr=False}
        `shouldBe` "MatType"
      f Type{elemType=RealType, numDim=4, isTS=False, isDistr=False}
        `shouldBe` "RArrType 4"
    it "matches RDistrType" $ do
      let rdistr = Type{elemType=RealType, numDim=0, isTS=False, isDistr=True}
      f rdistr `shouldBe` "RDistrType"
      f rdistr{elemType=IntType} `shouldBe` "Other"
      f rdistr{numDim=1} `shouldBe` "Other"
    it "matches TSDType" $ do
      let tsd = Type{elemType=RealType, numDim=0, isTS=True, isDistr=True}
      f tsd `shouldBe` "TSDType"
      f tsd{elemType=IntType} `shouldBe` "Other"
      f tsd{numDim=1} `shouldBe` "Other"
      f tsd{isDistr=False} `shouldBe` "Other"

forAllET :: Testable prop => (ElemType -> prop) -> Property
forAllET = forAll (elements [IntType, RealType])

test_fromET :: Spec
test_fromET =
  describe "fromET" $ do
    prop "sets elemType" $ forAllET $ \x -> elemType (fromET x) == x
    prop "sets numDim 0" $ forAllET $ \x -> numDim (fromET x) == 0
    prop "sets isTS false" $ forAllET $ \x -> isTS (fromET x) == False
    prop "sets isDistr false" $ forAllET $ \x -> isDistr (fromET x) == False

test_vars_in :: Spec
test_vars_in =
  describe "vars_in" $ do
    it "returns singleton for Var _ v" $
      vars_in (Var () "foo") `shouldBe` Set.fromList ["foo"]
    it "returns empty for literals" $
      vars_in (Lit () $ IntVal 37) `shouldBe` Set.fromList []
    it "handles apply" $
      vars_in (Apply () "+"
               [Var () "a", Lit () (IntVal 0), Var () "b"])
        `shouldBe`
        Set.fromList ["a", "b"]
    it "handles let" $
      vars_in (Let () "a" (Var () "b") (Var () "c"))
        `shouldBe` Set.fromList ["a", "b", "c"]
    it "handles draw" $
      vars_in (Draw () "a"
                (Apply () "foo" [Var () "x"])
                (Apply () "bar" [Var () "y", Var () "z"]))
        `shouldBe` Set.fromList ["a", "x", "y", "z"]
    
    it "is defined for Bounds a" $ do
      vars_in (Bounds Nothing Nothing) `shouldBe` Set.fromList []
      vars_in (Bounds Nothing (Just $ Var () "b"))
        `shouldBe` Set.fromList ["b"]
      vars_in (Bounds (Just $ Var () "a") Nothing)
        `shouldBe` Set.fromList ["a"]
      vars_in (Bounds (Just $ Var () "a") (Just $ Var () "b"))
        `shouldBe` Set.fromList ["a", "b"]
    
    it "is defined for TypeSB a" $ do
      let sh = [Var () "m", Apply () "+" [Var () "n", Lit () (IntVal 1)]]
          bnds = Bounds (Just $ Var () "lo") (Just $ Var () "hi")
          typ = TypeSB{ tsbBase = MatType, tsbShape = sh, tsbBounds = bnds }
      vars_in typ `shouldBe` Set.fromList ["hi", "lo", "m", "n"]
    
    it "is defined for FctDef a" $ do
      let fd = FctDef { defName = "something"
                      , defParms = [("x", xtyp), ("y", ytyp)]
                      , defRetType = VecType
                      , defBody = Let () "a" (Lit () (RealVal 2.5))
                                    (Apply () "+"
                                      [Var () "x", Var () "a"])
                      }
          xtyp = TypeSB{ tsbBase = RType, tsbShape = []
                       , tsbBounds = lobound (Var () "lo")
                       }
          ytyp = TypeSB{ tsbBase = VecType, tsbBounds = nobounds
                       , tsbShape = [ Var () "N" ]
                       }
      vars_in fd `shouldBe` Set.fromList ["x", "lo", "y", "N", "a"]

test_unrollVarDefs :: Spec
test_unrollVarDefs =
  describe "unrollVarDefs" $ do
    it "Leaves Var, Lit, and Apply alone" $ do
      unrollVarDefs (Var () "a") `shouldBe` ([], Var () "a")
      unrollVarDefs (litR 3.5) `shouldBe` ([], litR 3.5)
      unrollVarDefs (Apply () "square" [Var () "x"])
        `shouldBe` ([], Apply () "square" [Var () "x"])
    it "Unrolls Let and Draw" $ do
      let tsdType = TSDType
      let tsType = tsdType{isDistr = False}
      unrollVarDefs
        (Let RDistrType "x" (Apply IType "+" [litI 1, litI 2]) $
         Draw RDistrType "y" (Apply RDistrType "dfoo" []) $
         Draw RDistrType "z" (Apply TSDType "dbar" [Var RType "a"]) $
         Let RDistrType "b" (Lit RType $ RealVal 3.5) $
         Apply RDistrType "dbaz" [Var RType "y", Var tsType "z"])
        `shouldBe`
        ([LetDef IType "x" (Apply IType "+" [litI 1, litI 2]),
          DrawDef RType "y" (Apply RDistrType "dfoo" []),
          DrawDef tsType "z" (Apply TSDType "dbar" [Var RType "a"]),
          LetDef RType "b" (Lit RType $ RealVal 3.5)],
         Apply RDistrType "dbaz" [Var RType "y", Var tsType "z"])

test_inferredVars :: Spec
test_inferredVars =
  describe "inferredVars" $ do
    it "returns empty when no draws" $ do
      let e = Let () "x" edef ebody
          edef = Apply () "foo" [e1, e2]
          e1 = Apply () "+" [Var () "y", litR0 2.7]
          e2 = Var () "z"
          ebody = Apply () "wn" [Var () "sigma"]
        in inferredVars e `shouldBe` Set.empty
    it "handles single inferred var" $
      let e = Draw () "foo" (Apply () "normal" [Var () "mu", Var () "sigma"]) $
              Apply () "ar1" [Var () "phi", Var () "sigmaq", Var () "sigma"]
      in inferredVars e `shouldBe` Set.singleton "foo"
    it "handles multiple inferred vars" $ do
      let e = Draw () "a"
                (Draw () "b" (Apply () "exponential" [Var () "mua"])
                             (Apply () "half_normal" [Var () "b"])) $
              Draw () "c"
                (Apply () "uniform" [litR0 0.0, litR0 1.0]) $
              Apply () "certainly" [Apply () "+" [Var () "a", Var () "c"]]
        in inferredVars e `shouldBe` Set.fromList ["a", "b", "c"]
      let e = Let () "x"
                (Draw () "sigma" (Apply () "half_normal" [Var () "s"])
                                 (Apply () "wn" [Var () "sigma"])) $
              Draw () "a" (Apply () "exponential" [litR0 2.0]) $
              Apply () "+" [Var () "x",
                            Apply () "ar1" [Var () "p", Var () "t", Var () "a"]]
        in inferredVars e `shouldBe` Set.fromList ["sigma", "a"]
      let e = Apply () "+"
              [ Draw () "x" (Apply () "exponential" [Var () "mux"])
                            (Apply () "wn" [Var () "x"])
              , Draw () "y" (Apply () "half_cauchy" [Var () "yscale"])
                            (Apply () "const" [Var () "y"])
              ]
        in inferredVars e `shouldBe` Set.fromList ["x", "y"]

test_Signatures :: Spec
test_Signatures =
  describe "Signatures" $ do
    it "provides mempty" $
      let Sigs f = mempty in f [] `shouldBe` Nothing
    it "provides <>" $ do
      let ivec = (IType){numDim=1}
      let s1 = Sigs $ \typs->
                case typs of
                  [RType] -> Just VecType
                  _       -> Nothing
          s2 = Sigs $ \typs->
                 case typs of
                   [IType] -> Just ivec
                   _       -> Nothing
      let Sigs f = s1 <> s2
      f [] `shouldBe` Nothing
      f [RType] `shouldBe` Just VecType
      f [IType] `shouldBe` Just ivec

test_typedExpr :: Spec
test_typedExpr =
  describe "typedExpr" $ do
    let venv = Map.fromList [ ("rx", RType), ("ry", RType), ("ix", IType) ]
        matDistrType = (MatType){isDistr=True}
        fenv = Map.fromList
               [ ("+",   Sigs $ \typs ->
                         case typs of
                           [IType, IType] -> Just IType
                           [RType, RType] -> Just RType
                           _              -> Nothing
                 )
               , ("vec", Sigs $ \typs ->
                         case typs of
                           [RType] -> Just VecType
                           _       -> Nothing
                 )
               , ("i2r", Sigs $ \typs ->
                         case typs of
                           [IType] -> Just RType
                           _       -> Nothing
                 )
               , ("normal", Sigs $ \typs ->
                            case typs of
                              [RType, RType] -> Just RDistrType
                              _              -> Nothing
                 )
               , ("dfoo", Sigs $ \typs ->
                          case typs of
                            [RType] -> Just matDistrType
                            _       -> Nothing
                 )
               ]
    let cases =
          [ ( Var () "rx"
            , Right $ Var RType "rx"
            )
          , ( Lit () (IntVal 12)
            , Right $ Lit IType (IntVal 12)
            )
          , ( Lit () (RealVal 3.5)
            , Right $ Lit RType (RealVal 3.5)
            )
          , ( Let () "a"
                (Apply () "+" [Var () "ix", Lit () (IntVal 0)])
                (Apply () "vec" [Apply () "i2r" [Var () "a"]])
            , Right $ Let VecType "a"
                      (Apply IType "+" [Var IType "ix", Lit IType (IntVal 0)])
                      (Apply VecType "vec" [Apply RType "i2r" [Var IType "a"]])
            )
          , ( Draw () "b"
                (Apply () "normal" [Var () "rx", Var () "ry"])
                (Apply () "dfoo" [Var () "b"]) 
            , Right $ Draw matDistrType "b"
                      (Apply RDistrType "normal"
                         [Var RType "rx", Var RType "ry"])
                      (Apply matDistrType "dfoo" [Var RType "b"]) 
            )
          ]
    it "adds types to expressions" $
      forM_ cases $ \(input, expected) ->
        typedExpr (venv, fenv) input `shouldBe` expected
    let errcases =
          [ ( Var () "xyz"
            , Left "Unknown variable: xyz"
            )
          , ( Let () "rx"
                (Lit () (RealVal 1.7))
                (Apply () "+" [Var () "rx", Var () "rx"])
            , Left "Redefined variable: rx"
            )
          , ( Draw () "a" (Lit () (IntVal 23)) (Var () "a")
            , Left "In a ~ _e1; _e2: _e1 must be a distribution"
            )
          , ( Draw () "a"
                (Apply () "normal" [Var () "rx", Var () "ry"])
                (Apply () "+" [Var () "a", Lit () (RealVal 27.3)])
            , Left "In a ~ _e1; _e2: _e2 must be a distribution"
            )
          ]
    it "gives errors when there are typing errors" $
        forM_ errcases $ \(input, expected) ->
          typedExpr (venv, fenv) input `shouldBe` expected

test_typedFctDef :: Spec
test_typedFctDef =
  describe "typedFctDef" $ do
    test_typedFctDef_OK
    test_typedFctDef_Err

test_typedFctDef_OK :: Spec
test_typedFctDef_OK = do
  it "assigns types properly" $ do
    -- def fctname(N: int{1,}, x:real{lo,hi}[N]) : real =
    --   sum(x) + i2r(N)
    let bndsN = Bounds {lobnd = Just (Lit () (IntVal 1)), hibnd = Nothing}
        bndsx = Bounds {lobnd = Just (Var () "lo"), hibnd = Just (Var () "hi")}
        inp = FctDef
              { defName = "fctname"
              , defParms =
                [ ("N", TypeSB { tsbBase = IType, tsbShape = []
                               , tsbBounds = bndsN})
                , ("x", TypeSB { tsbBase = VecType, tsbShape = [Var () "N"]
                               , tsbBounds = bndsx})
                ]
              , defRetType = RType
              , defBody =
                  Apply () "+"
                  [ Apply () "sum" [Var () "x"]
                  , Apply () "i2r" [Var () "N"]
                  ]
              }
    let tbndsN = Bounds { lobnd = Just (Lit IType (IntVal 1))
                        , hibnd = Nothing }
        tbndsx = Bounds { lobnd = Just (Var RType "lo")
                        , hibnd = Just (Var RType "hi") }
        out = FctDef
              { defName = "fctname"
              , defParms =
                [ ("N", TypeSB { tsbBase = IType, tsbShape = []
                               , tsbBounds = tbndsN})
                , ("x", TypeSB { tsbBase = VecType, tsbShape = [Var IType "N"]
                               , tsbBounds = tbndsx})
                ]
              , defRetType = RType
              , defBody =
                  Apply RType "+"
                  [ Apply RType "sum" [Var VecType "x"]
                  , Apply RType "i2r" [Var IType "N"]
                  ]
              }
    let venv = Map.fromList [("lo", RType), ("hi", RType)]
        fenv = Map.fromList
               [ ("+", Sigs $ \typs ->
                       case typs of
                         [RType, RType] -> Just RType
                         _              -> Nothing)
               , ("sum", Sigs $ \typs ->
                         case typs of
                           [VecType] -> Just RType
                           _         -> Nothing)
               , ("i2r", Sigs $ \typs ->
                         case typs of
                           [IType] -> Just RType
                           _       -> Nothing)
               ]
    typedFctDef (venv, fenv) inp `shouldBe` Right out

test_typedFctDef_Err :: Spec
test_typedFctDef_Err = do
  it "reports type errors" $ do
    let envs = (Map.empty, baseFctEnv)
        cases =
          [ "a(v: real[n]): real = 3.0" .->
            "In function a: In type of parameter v: In shape: \
             \Unknown variable: n"
            
          , "b(n: real, v: real[2, n]): real = 2.3" .->
            "In function b: In type of parameter v: In shape: \
            \Sizes must have type int"

          , "c(n: int) : real = \
            \  x = 1.0; y = (x = 5.0; x + 3.0); y * x"
            .->
            "In function c: Redefined variable: x"

          , "c1(y: real) : real = \
            \ x = 1.0; y = 0.3; x * y"
            .->
            "In function c1: Redefined variable: y"
            
          , "d(n: int, v: real{lo, }) : real = v" .->
            "In function d: In type of parameter v: In bounds: Unknown variable: lo"

          , "e(n: int, v: real{,hi}) : real = v" .->
            "In function e: In type of parameter v: In bounds: Unknown variable: hi"

          , "f(n: int, v: real{,1}[3,2]) : real = v" .->
            "In function f: In type of parameter v: In bounds: Must have type real"
          , "g(n: int, lo: real, v: int{lo,}) : int = v" .->
            "In function g: In type of parameter v: In bounds: Must have type int"
          ]
    forM_ cases $ \(defstr, errmsg) ->
      typedFctDef envs (parsefd0 defstr) `shouldBe` (Left errmsg)
