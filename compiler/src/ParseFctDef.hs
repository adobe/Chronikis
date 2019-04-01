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
module ParseFctDef (parsefdt, parsefd0, parseOnlyFctDefT, parseOnlyFctDef0)
where

import Control.Monad ((>=>))
import qualified Data.Bifunctor as Bifunctor
import Data.Either (either)

import AST (typedFctDef, FctDef0, FctDefT, FctTypeEnv, VarTypeEnv)
import Misc
import Parser (parseOnly, fctDef)

parseOnlyFctDefT :: (VarTypeEnv, FctTypeEnv) -> String ->
                    Either ErrorMsg FctDefT
parseOnlyFctDefT envs = parseOnlyFctDef0 >=> typedFctDef envs

parseOnlyFctDef0 :: String -> Either ErrorMsg FctDef0
parseOnlyFctDef0 =
  Bifunctor.first show . parseOnly fctDef

parsefdt :: (VarTypeEnv, FctTypeEnv) -> String -> FctDefT
parsefdt envs = either error id . (parseOnlyFctDefT envs . ("def " ++))

parsefd0 :: String -> FctDef0
parsefd0 = either error id . (parseOnlyFctDef0 . ("def " ++))
