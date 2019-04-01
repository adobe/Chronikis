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
module Periodic(Periodic(..), optimized_pcf_coeffs) where

import Data.List (find)
import Data.Matrix (nrows)
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Numeric.GSL.Special.Bessel (bessel_In)

data Periodic = Periodic {
  p_period :: Double,
  p_length_scale :: Double,
  p_min_dof :: Int
}

optimized_pcf_coeffs :: Periodic -> [Double]
optimized_pcf_coeffs Periodic{ p_period = period,
                               p_length_scale = length_scale,
                               p_min_dof = min_dof } =
  center_and_norm . take (n + 1) . unalias $ coeffs0
  where
    coeffs0 = take (max 100 $ n + 1) $ pcf_coeffs length_scale
    unalias = if periodIsInt
              then unalias_pcf_coeffs iperiod
              else id
    periodIsInt = (period == fromIntegral iperiod)
    iperiod = round period :: Int
    n = max ((mindof + 1) `div` 2) (min_order_required length_scale)
    mindof = if periodIsInt
             then min min_dof (iperiod - 1)
             else min_dof
    center_and_norm cs = map (/ (sum cs1)) cs1 where cs1 = tail cs

pcf_coeffs :: Double -> [Double]
pcf_coeffs length_scale =
  (0.5 * head coeffs0) : tail coeffs0
  where
    coeffs0 = map cfct [0 ..]
    cfct k = a * besselI k invls
    a = 2 / exp invls
    invls = 1.0 / sqr length_scale

unalias_pcf_coeffs :: Int -> [Double] -> [Double]
unalias_pcf_coeffs period coeffs =
  if n <= kmax + 1 then coeffs else uacoeffs
  where
    n = length coeffs
    kmax = period `div` 2
    coeffsmat = M.fromList reps period $ coeffs ++ take (n1 - n) (repeat 0)
      where reps = n `div` period + fromEnum (n `mod` period /= 0)
            n1 = reps * period
    coeffsv = M.getMatrixAsVector $ ones * coeffsmat
      where ones = M.matrix 1 (nrows coeffsmat) $ const 1.0
    uacoeffs = map f [0..kmax]
      where f k = if 0 < k && k < period - k then ck + cn else ck
                  where ck = coeffsv V.! k
                        cn = coeffsv V.! (period - k)

min_order_required :: Double -> Int
min_order_required length_scale =
  case find ((length_scale >=) . snd) mls_table of
    Nothing -> error "Length scale is too small"
    Just (k, _) -> k

sqr :: Double -> Double
sqr = (^^(2::Int))

besselI :: Int -> Double -> Double
besselI n = bessel_In (fromIntegral n)

-- Minimum length scale for which the max error is <= 0.01 when truncating
-- cosine series for the covariance function γ to a given order. This list
-- was created by running
--   d <- min_length_scale(0.01, 100); cat(write_mls(d))
-- in R after sourcing covfct.R, and copying the output into this file.
--
mls_table :: [(Int, Double)]
mls_table =
  [
  (2, 1.403170586500004e+00),
  (3, 8.503610623254895e-01),
  (4, 6.258554177957133e-01),
  (5, 4.978468563784536e-01),
  (6, 4.152820847655110e-01),
  (7, 3.570393242509731e-01),
  (8, 3.123123460464706e-01),
  (9, 2.791499535072714e-01),
  (10, 2.516726890967672e-01),
  (11, 2.288678246495944e-01),
  (12, 2.099343556509220e-01),
  (13, 1.942372021477807e-01),
  (14, 1.804913412554068e-01),
  (15, 1.684439403003159e-01),
  (16, 1.585639785978510e-01),
  (17, 1.492635191503687e-01),
  (18, 1.411165272217898e-01),
  (19, 1.334142084314441e-01),
  (20, 1.272261572523596e-01),
  (21, 1.213251218105430e-01),
  (22, 1.156977896702928e-01),
  (23, 1.108088504586215e-01),
  (24, 1.061264988289907e-01),
  (25, 1.020817920459364e-01),
  (26, 9.819123764839745e-02),
  (27, 9.444896056081600e-02),
  (28, 9.124239840112498e-02),
  (29, 8.814469970401639e-02),
  (30, 8.515216853194243e-02),
  (31, 8.226123442521560e-02),
  (32, 7.981229396325920e-02),
  (33, 7.743625915884747e-02),
  (34, 7.513095958971636e-02),
  (35, 7.289428944769295e-02),
  (36, 7.103021665635979e-02),
  (37, 6.921381244644387e-02),
  (38, 6.744385782388823e-02),
  (39, 6.571916496708681e-02),
  (40, 6.403857642973385e-02),
  (41, 6.240096436405846e-02),
  (42, 6.106832316329456e-02),
  (43, 5.950666722997589e-02),
  (44, 5.823583692664665e-02),
  (45, 5.699214660165999e-02),
  (46, 5.577501665093932e-02),
  (47, 5.458387984849037e-02),
  (48, 5.341818108205365e-02),
  (49, 5.227737709440224e-02),
  (50, 5.138230048912878e-02),
  (51, 5.028497496988915e-02),
  (52, 4.921108404356793e-02),
  (53, 4.836850753159024e-02),
  (54, 4.754035734637074e-02),
  (55, 4.672638648493606e-02),
  (56, 4.572849312820279e-02),
  (57, 4.494554442900638e-02),
  (58, 4.417600112814345e-02),
  (59, 4.341963370265209e-02),
  (60, 4.286086894752326e-02),
  (61, 4.212701879648485e-02),
  (62, 4.140573339407154e-02),
  (63, 4.069679761065803e-02),
  (64, 4.017307287573340e-02),
  (65, 4.000000000000000e-02)
  ]
