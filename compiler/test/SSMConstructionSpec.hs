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
module SSMConstructionSpec (main, spec) where

import Control.Monad (forM_)
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as Map
import Data.String.QQ
import Data.Tuple.Extra (first)
import Test.Hspec

import Misc
import AST hiding (inferredVars)
import Env (baseFctEnv)
import Parser (parseOnly, fctDef)
import Number
import SSMConstruction

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  test_toSSMCtor

test_toSSMCtor :: Spec
test_toSSMCtor =
  describe "toSSMCtor" $ do
    it ("runs " ++ show n ++ " examples from acceptance tests") $
      forM_ toSSMCtorCases $ \(input, expected) ->
        toSSMCtor input `shouldBe` expected
  where
    n = length toSSMCtorCases

letv :: String -> ExprT -> ExprT -> ExprT
letv = Let TSDType

rvar, vvar, mvar :: String -> ExprT
rvar = Var RType
vvar = Var VecType
mvar = Var MatType

rapp, vapp, mapp :: String -> [ExprT] -> ExprT
rapp = Apply RType
vapp = Apply VecType
mapp = Apply MatType

negLitR :: Scientific -> ExprT
negLitR x = rapp "negate" [litR x]

toSSMCtorCases :: [(FctDefT, SSMConstruction)]
toSSMCtorCases =
  map (first parseit) $
  [ [s| def main(sigma_scale: real{0.0,}) =
          raw0 ~ half_normal(1.0);
          sigma = sigma_scale * raw0;
          Z = vec();
          H = square(sigma);
          T = diag();
          Q = diag();
          a0 = vec();
          P0 = diag();
          ssm(Z, H, T, Q, a0, P0)
    |]
    .->
    SSMConstruction
    { knownVars = ["sigma_scale"]
    , inferredVars = [("raw0", 0)]
    , ssmExpr =
      letv "sigma" (rapp "*" [rvar "sigma_scale", rvar "raw0"]) $
      letv "Z" (vapp "vec" []) $
      letv "H" (rapp "square" [rvar "sigma"]) $
      letv "T" (mapp "diag" []) $
      letv "Q" (mapp "diag" []) $
      letv "a0" (vapp "vec" []) $
      letv "P0" (mapp "diag" []) $
      Apply TSDType "ssm"
      [ vvar "Z", rvar "H", mvar "T", mvar "Q", vvar "a0", mvar "P0" ]
    }

  , [s| def main(sigma_qa_scale, sigma_qd_scale, sigma_h_scale: real{0.0,},
                 mu_a, mu_d: real, sigma_a, sigma_d: real{0.0,})
        =
        raw0 ~ half_normal(1.0);
        sigma_qa = raw0 * sigma_qa_scale;
        raw1 ~ half_normal(1.0);
        sigma_qd = raw1 * sigma_qd_scale;
        raw2 ~ half_normal(1.0);
        sigma_h = raw2 * sigma_h_scale;
        Z = vec(1.0, 0.0);
        H = square(sigma_h);
        T = mat22(1.0, 1.0, 0.0, 1.0);
        Q = diag(vec(square(sigma_qa), square(sigma_qd)));
        a0 = vec(mu_a, mu_d);
        P0 = diag(vec(square(sigma_a), square(sigma_d)));
        ssm(Z, H, T, Q, a0, P0)
    |]
    .->
    SSMConstruction
    { knownVars = ["mu_a", "mu_d", "sigma_a", "sigma_d",
                   "sigma_h_scale", "sigma_qa_scale", "sigma_qd_scale"]
    , inferredVars = [("raw0", 0), ("raw1", 0), ("raw2", 0)]
    , ssmExpr =
      letv "sigma_qa" (rapp "*" [rvar "raw0", rvar "sigma_qa_scale"]) $
      letv "sigma_qd" (rapp "*" [rvar "raw1", rvar "sigma_qd_scale"]) $
      letv "sigma_h" (rapp "*" [rvar "raw2", rvar "sigma_h_scale"]) $
      letv "Z" (vapp "vec" [litR 1.0, litR 0.0]) $
      letv "H" (rapp "square" [rvar "sigma_h"]) $
      letv "T" (mapp "mat22" [litR 1.0, litR 1.0, litR 0.0, litR 1.0]) $
      letv "Q" (mapp "diag"
                [vapp "vec"
                 [ rapp "square" [rvar "sigma_qa"]
                 , rapp "square" [rvar "sigma_qd"]
                 ]]) $
      letv "a0" (vapp "vec" [rvar "mu_a", rvar "mu_d"]) $
      letv "P0" (mapp "diag"
                 [vapp "vec"
                  [ rapp "square" [rvar "sigma_a"]
                  , rapp "square" [rvar "sigma_d"]
                  ]]) $
      Apply TSDType "ssm"
      [ vvar "Z", rvar "H", mvar "T", mvar "Q", vvar "a0", mvar "P0" ]
    }

  , [s| def main(sigma_q_scale, sigma_h_scale, sigma_p_scale: real{0.0,},
                 mu_a: real, sigma_a: real{0.0,}, rho_mean: real{0.0,1.0})
        =
        raw0 ~ half_cauchy(1.0);
        sigma_q = raw0 * sigma_q_scale;
        raw1 ~ half_normal(1.0);
        sigma_h = raw1 * sigma_h_scale;
        rho ~ exponential_rt(rho_mean, 1.0);
        raw2 ~ half_normal(1.0);
        sigma_p = raw2 * sigma_p_scale;
        csigma2s = square(sigma_p) *
                   vec(0.618, 0.618, 0.276, 0.276, 0.106, 0.106);
        rhoSqr = square(rho);
        Z = vec(1.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0);
        H = square(sigma_h);
        T = diag(mat11(1.0),
                 diag(sqrt(1.0 - rhoSqr) *
                      { mat22(0.623,-0.782,0.782,0.623)
                      , mat22(-0.223,-0.975,0.975,-0.223)
                      , mat22(-0.901,-0.434,0.434,-0.901)
                      }));
        Q = diag(vec(vec(square(sigma_q)), rhoSqr * csigma2s));
        a0 = vec(mu_a, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
        P0 = diag(vec(vec(square(sigma_a)), csigma2s));
        ssm(Z, H, T, Q, a0, P0)
    |]
    .->
    SSMConstruction
    { knownVars = ["mu_a", "sigma_a", "sigma_h_scale",
                   "sigma_p_scale", "sigma_q_scale"]
    , inferredVars = [("raw0", 0), ("raw1", 0), ("raw2", 0), ("rho", 0)]
    , ssmExpr =
      letv "sigma_q" (rapp "*" [rvar "raw0", rvar "sigma_q_scale"]) $
      letv "sigma_h" (rapp "*" [rvar "raw1", rvar "sigma_h_scale"]) $
      letv "sigma_p" (rapp "*" [rvar "raw2", rvar "sigma_p_scale"]) $
      letv "csigma2s" (vapp "*" [ rapp "square" [rvar "sigma_p"]
                                , vapp "vec" [litR 0.618, litR 0.618,
                                              litR 0.276, litR 0.276,
                                              litR 0.106, litR 0.106]
                                ]) $
      letv "rhoSqr" (rapp "square" [rvar "rho"]) $
      letv "Z" (vapp "vec" [litR 1.0, litR 1.0, litR 0.0, litR 1.0, litR 0.0,
                            litR 1.0, litR 0.0]) $
      letv "H" (rapp "square" [rvar "sigma_h"]) $
      letv "T" (mapp "diag"
                [ mapp "mat11" [litR 1.0]
                , mapp "diag"
                  [ Apply (RArrType 3) "*"
                    [ rapp "sqrt" [rapp "-" [litR 1.0, rvar "rhoSqr"]]
                    , Apply (RArrType 3) "{}"
                      [ mapp "mat22" [litR 0.623, negLitR 0.782,
                                      litR 0.782, litR 0.623]
                      , mapp "mat22" [negLitR 0.223, negLitR 0.975,
                                      litR 0.975, negLitR 0.223]
                      , mapp "mat22" [negLitR 0.901, negLitR 0.434,
                                      litR 0.434, negLitR 0.901]
                      ]
                    ]]
                ]) $
      letv "Q" (mapp "diag"
                [vapp "vec"
                 [ vapp "vec" [rapp "square" [rvar "sigma_q"]]
                 , vapp "*" [rvar "rhoSqr", vvar "csigma2s"]
                 ]]) $
      letv "a0" (vapp "vec" [rvar "mu_a", litR 0.0, litR 0.0, litR 0.0,
                           litR 0.0, litR 0.0, litR 0.0]) $
      letv "P0" (mapp "diag"
                 [vapp "vec"
                  [ vapp "vec" [rapp "square" [rvar "sigma_a"]]
                  , vvar "csigma2s"
                  ]]) $
      Apply TSDType "ssm"
      [ vvar "Z", rvar "H", mvar "T", mvar "Q", vvar "a0", mvar "P0" ]
    }
  ]

parseit :: String -> FctDefT
parseit input =
  case result of
    Left errmsg -> error errmsg
    Right fd    -> fd
  where
    result = do
      fd <- Bifunctor.first show $ parseOnly fctDef input
      typedFctDef (Map.empty, baseFctEnv) fd

