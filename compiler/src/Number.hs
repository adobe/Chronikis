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
module Number
  (Number(..), is_representable_number, Scientific, scientific)
where

import Data.Scientific (Scientific, scientific)

data Number = IntVal Integer | RealVal Scientific
  deriving (Eq, Show)

is_representable_number :: Number -> Bool
is_representable_number (IntVal n) =
-- Representable as 32-bit signed int
  (lo <= n && n <= hi)
  where lo = -(2^e) :: Integer
        hi = (2^e - 1) :: Integer
        e = 31 :: Integer
is_representable_number (RealVal x) =
-- Representable (with rounding) as 64-bit IEEE float
  (absx == 0 || lo <= absx && absx <= hi)
  where absx = abs x
        lo = 2.0^^(1 - emax) :: Scientific
        hi = fromInteger (2^mantbits1 - 1) *
             fromInteger (2^(emax - mantbits)) :: Scientific
        emax = 1023 :: Integer
        mantbits = 52 :: Integer
        mantbits1 = mantbits + 1
