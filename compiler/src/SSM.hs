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
module SSM where

import BasicExpr
import NameGen (ContainsVars(..))

data SSM = SSM{ vecZ, scalH, matT, matQ, veca0, matP0 :: Expr }
  deriving (Eq, Show)

ssmMap :: (Expr -> Expr) -> SSM -> SSM
ssmMap f (SSM a1 a2 a3 a4 a5 a6) =
  SSM (f a1) (f a2) (f a3) (f a4) (f a5) (f a6)

instance ContainsVars SSM where
  vars_in (SSM mZ mH mT mQ va0 mP0) = vars_in [mZ, mH, mT, mQ, va0, mP0]
