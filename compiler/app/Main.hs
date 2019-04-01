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
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Monad (forM_, when)
import qualified Data.Map.Strict as Map
import System.Console.GetOpt
import System.Exit (ExitCode(..), exitWith)
import System.Environment
import System.FilePath (takeBaseName)
import System.IO
import Text.Pretty.Simple ({-pPrint, pPrintLightBg,-} pPrintNoColor)

import InferShapesBounds (toTranslateModel)
import Misc
import AST hiding (inferredVars)
import qualified AST
import Checks
import Env (baseFctEnv, baseVarEnv)
import ParseFctDef (parseOnlyFctDefT)
import Transforms
import RAST (printRFctDef)
import RNames (makeRName)
import SSMConstruction (toSSMCtor, SSMConstruction(..))
import StanAST (compileStanAST)
import Translate (unrollModel, translate)
import TranslToR (ssmCreationFct)

data UnrolledFctDef a =
  UnrolledFctDef
  { udefName :: FctName
  , udefParms :: [(VarName, TypeSB a)]
  , udefRetType :: Type
  , udefDefs :: [VarDef a]
  , udefBody :: Expr a
  }
  deriving (Show)

unrolledFctDef :: AST.MaybeType a => FctDef a -> UnrolledFctDef a
unrolledFctDef fd =
  UnrolledFctDef
  { udefName = defName fd
  , udefParms = defParms fd
  , udefRetType = defRetType fd
  , udefDefs = defs
  , udefBody = body
  }
  where
    (defs, body) = unrollVarDefs (defBody fd)

data Options = Options
  { showParse :: Bool
  , showXformed :: Bool
  , showTModel :: Bool
  , showSModel :: Bool
  , outFileStan :: Maybe String
  , outFileR :: Maybe String
  , fctNameR :: Maybe String
  }

defaultOptions :: Options
defaultOptions = Options
  { showParse = False
  , showXformed = False
  , showTModel = False
  , showSModel = False
  , outFileStan = Nothing
  , outFileR = Nothing
  , fctNameR = Nothing
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['p'] [] (NoArg $ \x-> x{showParse=True}) "show AST from parse"
  , Option ['x'] [] (NoArg $ \x-> x{showXformed=True}) "show transformed AST"
  , Option ['t'] [] (NoArg $ \x-> x{showTModel=True}) "show translated model"
  , Option ['s'] [] (NoArg $ \x-> x{showSModel=True}) "show Stan AST"
  , Option [] ["stan"] (ReqArg (\f x-> x{outFileStan=Just f}) "STAN_FILE")
    "path of Stan file to create"
  , Option [] ["R"] (ReqArg (\f x-> x{outFileR=Just f}) "R_FILE")
    "path of R file to create"
  , Option [] ["rfct"] (ReqArg (\nam x-> x{fctNameR = Just nam}) "R_FCT_NAME")
    "name of R function to create SSMs"
  ]

progOptions :: [String] -> Either [String] Options
progOptions argStrs =
  case getOpt RequireOrder options argStrs of
    (opts, [], []) ->
      Right $ foldl (flip id) defaultOptions opts
    (_, _, []) ->
      Left $ ["Non-option arguments"]
    (_, _, errs) ->
      Left $ errs

main :: IO ()
main =
  do
    args <- getArgs
    either exitWithErrors mainBody (progOptions args)

mainBody :: Options -> IO()
mainBody (Options{showParse, showXformed, showTModel, showSModel,
                  outFileStan, outFileR, fctNameR}) =
  do
    fd' <- parseOnlyFctDefT (baseVarEnv, baseFctEnv)  <$> getContents
    case fd' of
      Left errmsg -> exitWithErrors [errmsg]
      Right fd ->
        let failedChecks = checkProgram (Map.keysSet baseFctEnv) [fd]
        in
          if not (null failedChecks)
          then exitWithErrors failedChecks
          else do
            when showParse $
              output "parse" (unrolledFctDef fd)
            let xfd = xformFctDef expansionXform fd
            when showXformed $
              output "xformed" (unrolledFctDef xfd)
            let tmdl = toTranslateModel xfd
            when showTModel $
              output "tmodel" (unrollModel tmdl)
            let to_monitor = AST.inferredVars $ defBody fd
                smdl = translate to_monitor tmdl
            when showSModel $
              output "smodel" smdl
            let stansrc = compileStanAST smdl
            case outFileStan of
              Nothing -> return ()
              Just fpath -> writeFile fpath stansrc
            case outFileR of
              Nothing -> return ()
              Just fpath ->
                let fctName = ssmCreationFctName fctNameR fpath
                in
                  writeFile fpath $ ssmCreationCode fctName xfd

ssmCreationFctName :: Maybe String -> FilePath -> String
ssmCreationFctName maybeFctName fpath =
  makeRName $
  case maybeFctName of
    Nothing -> "createSSMs_" ++ takeBaseName fpath
    Just fctName -> fctName

ssmCreationCode :: String -> FctDefT -> String
ssmCreationCode fctName xfd =
  fctName ++ " <- " ++ fct ++ "\n" ++
  fctName1 ++ " <- " ++
  printRFctDef (ssmCreationFct ssmExpr knownVars inferredVars)
  where
    SSMConstruction{ ssmExpr, knownVars, inferredVars } = toSSMCtor xfd
    fctName1 = fctName ++ "_1"
    fct = concat [header, body, footer]
    header = "function(data_, posterior_draws_, ndraws_) {\n"
    footer = "}\n"
    body = concatMap (("  " ++) . (++ "\n")) bodyLines
    bodyLines =
      [ "if (missing(ndraws_)) ndraws_ <- .Machine$integer.max"
      , "ndraws_ <- min(chronikis::num_draws(posterior_draws_), ndraws_)"
      , "lapply(1:ndraws_, function(i_){" ++ call1 ++ "})"
      ]
    call1 = fctName1 ++ "(data_, posterior_draws_, i_)"
    
output :: Show a => String -> a -> IO ()
output label x = do
  putStrLn $ "--- " ++ label ++ " ---"
  pPrintNoColor x
  putStrLn "------"

exitWithErrors :: [ ErrorMsg ] -> IO ()
exitWithErrors messages = do
  hPutStrLn stderr "*** ERROR ***"
  forM_ messages (hPutStrLn stderr)
  exitWith $ ExitFailure (-1)
