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
module PeriodicSpec (main, spec) where

import Test.Hspec

import Periodic

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  test_optimized_pcf_coeffs

test_optimized_pcf_coeffs :: Spec
test_optimized_pcf_coeffs =
  describe "optimized_pcf_coeffs" $ do
    it "returns result with at least desired DOF" $ do
      let inp = Periodic{ p_period=10.0, p_length_scale=1.0, p_min_dof=9 }
          n   = length $ optimized_pcf_coeffs inp
        in n `shouldBe` 5
      let inp = Periodic{ p_period=10.0, p_length_scale=1.0, p_min_dof=8 }
          n   = length $ optimized_pcf_coeffs inp 
        in n `shouldBe` 4
      let inp = Periodic{ p_period=7.0, p_length_scale=2.0, p_min_dof=5 }
          n   = length $ optimized_pcf_coeffs inp
        in n `shouldBe` 3
        
    let actual_corrfct :: Periodic -> Int -> Double
        actual_corrfct p n =
          sum [ c_k * cos (2 * pi * k * x) | (c_k, k) <- zip c [1..]]
          where x = fromIntegral (n::Int) / p_period p
                c = optimized_pcf_coeffs p
                
    it "produces marginal variance of 1" $ do
      let p = Periodic{ p_period=7.0, p_length_scale=0.7, p_min_dof=0 }
        in abs (actual_corrfct p 0 - 1.0) `shouldSatisfy` (<= 1e-13)
      let p = Periodic{ p_period=12.387, p_length_scale=0.5, p_min_dof=0 }
        in abs (actual_corrfct p 0 - 1.0) `shouldSatisfy` (<= 1e-13)

    it "produces zero-centered correlations" $ do
      let p     = Periodic{ p_period=7.0, p_length_scale=0.8, p_min_dof=0 }
          corrs = [ actual_corrfct p n | n <- [0..6] ]
        in abs (sum corrs) `shouldSatisfy` (<= 1e-13)
      let p     = Periodic{ p_period=9.3, p_length_scale=0.5, p_min_dof=10 }
          corrs = [ actual_corrfct p n | n <- [0..92] ]
        in abs (sum corrs) `shouldSatisfy` (<= 1e-13)
      let p     = Periodic{ p_period=5.5, p_length_scale=0.7, p_min_dof=0 }
          corrs = [ actual_corrfct p n | n <- [0..10] ]
        in abs (sum corrs) `shouldSatisfy` (<= 1e-13)

    it "achieves desired correlation fct to within error 0.01" $ do
    {- Each target vector of correlations was computed by the R expression

         print(pcovf_std(length_scale, period, nmax), digits=20)

       where nmax is period-1 if period is an integer, or some suitably
       large number otherwise, and R function pcovf_std is defined as follows:

         periodic_covf <- function(ell)function(x){
           exp(-2 * sin(pi * x)^2 / ell^2)
         }

         pcovf_std <- function(ell, iperiod, nmax){
           rhos <- periodic_covf(ell)((0:nmax) / iperiod)
           rhos <- rhos - mean(rhos)
           rhos <- rhos / rhos[1]
           stopifnot(rhos[1] == 1)
           stopifnot(abs(sum(rhos)) <= 3 * sqrt(nmax+1) * 1e-15)
           rhos
         }
    -}
      let maxdiff p target =
            maximum [ abs (a - b) | (a, b) <- zip target actual ]
            where
              actual = [ actual_corrfct p n | n <- [0..] ]
      let p = Periodic{ p_period = 7.0, p_length_scale=0.5, p_min_dof=0 }
          target =
            [ 1.000000000000000000000,  0.016766528244867691388
            ,-0.253947118795246007394, -0.262819409449621510522
            ,-0.262819409449621510522, -0.253947118795246007394
            , 0.016766528244867445058
            ]
        in maxdiff p target `shouldSatisfy` (<= 0.01)
      let p = Periodic{ p_period = 24.0, p_length_scale=1.0, p_min_dof=20 }
          target =
            [ 1.000000000000000000000,  0.937293796247541277644
            , 0.765297197667511586161,  0.524749162495045662347
            , 0.263497582960590392975,  0.020199128780199356731
            ,-0.183213713926271976051, -0.340240841366895752085
            ,-0.454157811528146659885, -0.532288042959741081184
            ,-0.582176398273461681576, -0.609713203200442865892
            ,-0.618493713791855914685, -0.609713203200443087937
            ,-0.582176398273461681576, -0.532288042959741081184
            ,-0.454157811528147048463, -0.340240841366895752085
            ,-0.183213713926272281363,  0.020199128780198940397
            , 0.263497582960590392975,  0.524749162495045440302
            , 0.765297197667511253094,  0.937293796247541499689
            ]
        in maxdiff p target `shouldSatisfy` (<= 0.01)
      let p = Periodic{ p_period = 5.5, p_length_scale=0.6, p_min_dof=0 }
          target =
            [ 1.000000000000000000000, -0.076179325353533916543
            ,-0.326913632320053271219, -0.334630981864835419515
            ,-0.284301322465758565805,  0.522025262004180645725
            , 0.522025262004182311060, -0.284301322465758454783
            ,-0.334630981864835419515, -0.326913632320053215707
            ,-0.076179325353534999010
            ]
        in maxdiff p target `shouldSatisfy` (<= 0.01)
      let p = Periodic{ p_period=50, p_length_scale = 0.2, p_min_dof=0 }
          target =
            [ 1.000000000000000000000,  0.805482156280585281927
            , 0.408490581809638486632,  0.100683333071156844163
            ,-0.037835104745258980874, -0.078011363339022893326
            ,-0.085948489945198311424, -0.087063252829699858126
            ,-0.087179132186201588328, -0.087188420845888944810
            ,-0.087189020196807132113, -0.087189052764285027153
            ,-0.087189054326321110966, -0.087189054395736653724
            ,-0.087189054398739168628, -0.087189054398871965179
            ,-0.087189054398878279573, -0.087189054398878612639
            ,-0.087189054398878640395, -0.087189054398878640395
            ,-0.087189054398878640395, -0.087189054398878640395
            ,-0.087189054398878640395, -0.087189054398878640395
            ,-0.087189054398878640395, -0.087189054398878640395
            ,-0.087189054398878640395, -0.087189054398878640395
            ,-0.087189054398878640395, -0.087189054398878640395
            ,-0.087189054398878640395, -0.087189054398878640395
            ,-0.087189054398878640395, -0.087189054398878612639
            ,-0.087189054398878279573, -0.087189054398871965179
            ,-0.087189054398739168628, -0.087189054395736653724
            ,-0.087189054326321110966, -0.087189052764285027153
            ,-0.087189020196807132113, -0.087188420845888944810
            ,-0.087179132186201588328, -0.087063252829699858126
            ,-0.085948489945198311424, -0.078011363339022934960
            ,-0.037835104745258980874,  0.100683333071155456384
            , 0.408490581809636821298,  0.805482156280584060681 
            ]
          in maxdiff p target `shouldSatisfy` (<= 0.01)
