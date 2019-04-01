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
module RAST (
  module Number, RExpr(..), RFctDef(..), RStmt(..), printRExpr, printRFctDef,
  rlitI, rlitR
) where

import Control.Arrow((>>>))
import Control.Exception (assert)
import Data.List (intercalate)
import Data.List.Utils (startswith)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)

import Misc
import Number

data RFctDef = RFctDef
               { rfctParms :: [VarName]
               , rfctStmts :: [RStmt]
               , rfctReturn :: RExpr
               }
  deriving (Eq, Show)

data RStmt = RLet VarName RExpr | RCheck RExpr
  deriving (Eq, Show)

data RExpr = RVar VarName
           | RLit Number
           | RApply FctName [RExpr]
           | RApply' FctName [RExpr] [(String, RExpr)]
           | RIndex Bool RExpr [Maybe RExpr]
           | RField RExpr VarName
           | RLambda [VarName] [RStmt] RExpr
  deriving (Eq, Show)

rlitI :: Integer -> RExpr
rlitI n = RLit (IntVal n)

rlitR :: Scientific -> RExpr
rlitR x = RLit (RealVal x)

printRFctDef :: RFctDef -> String
printRFctDef RFctDef{ rfctParms = parms, rfctStmts = stmts, rfctReturn = ret }
  = concatMap (++ "\n") $
    wrap (printFctHeader parms) $ map printStmt stmts ++ [rcode ret]
  where
    printFctHeader vars =
      "function(" ++ commasep (map rVarCode vars) ++ ")"
    wrap header src_lines =
      [ header ++ " {" ] ++ map ("  " ++) src_lines ++ [ "}" ]

printStmt :: RStmt -> String
printStmt (RCheck e) =
  rcode $ RApply "stopifnot" [e]
printStmt (RLet v e) =
  printf "%s <- %s" (rVarCode v) (rcode e)

printRExpr :: RExpr -> String
printRExpr = rcode

rcode :: RExpr -> String
rcode = rcodep maxBound

rcodep :: Int -> RExpr -> String
rcodep _ (RVar v) = rVarCode v
rcodep _ (RLit (IntVal n)) = paren (n < 0) $ rIntegerCode n
rcodep _ (RLit (RealVal x)) = paren (x < 0) $ rRealCode x
rcodep n (RApply fctName args) = rOpCode fctName n args
rcodep _ (RApply' fctName unnamed_args named_args) =
  rFctCode' fctName unnamed_args named_args
rcodep _ (RIndex drop_ arr maybeArgs) = bracketFct drop_ arr maybeArgs
rcodep _ (RField e field) = printf fmt (rcode e) field
  where
    fmt = case e of
            RVar _     -> "%s$%s"
            RField _ _ -> "%s$%s"
            _          -> "(%s)$%s"
rcodep n (RLambda vars stmts retExpr) =
  paren (n < maxBound) $
  "function" ++ paren True (commasep $ map rVarCode vars) ++
  braces (concatMap (printStmt >>> (++ "; ")) stmts ++ rcode retExpr)
  where
    braces s = "{ " ++ s ++ " }"

paren :: Bool -> String -> String
paren False str = str
paren True str = "(" ++ str ++ ")"

rVarCode :: VarName -> String
rVarCode v = v ++ "_"  -- Avoid name clash with any R functions we use

rIntegerCode :: Integer -> String
rIntegerCode n = show n ++ "L"

rRealCode :: Scientific -> String
rRealCode = show

rOpCode :: FctName -> Int -> [RExpr] -> String
rOpCode opName n args =
  case Map.lookup opName rEnv of
    Just f  -> f n args
    Nothing -> if startswith "%" opName
                 then logicError $ "Invalid R operator: " ++ opName
                 else rFctCode opName args

bracketFct :: Bool -> RExpr -> [Maybe RExpr] -> String
bracketFct drop_ arr args =
  case drop_ of
    False -> printf "%s[%s, drop=FALSE]" arrStr argsStr
    True  -> printf "%s[%s]" arrStr argsStr
  where
    arrStr = rcodep 0 arr
    argsStr = commasep $ map maybeRcode args
    maybeRcode (Just e) = rcode e
    maybeRcode Nothing  = ""

rEnv :: Map FctName (Int -> [RExpr] -> String)
rEnv = Map.fromList $
  [ ("::", const2 $ logicError "R operator `::` not allowed")   -- prec -2
  , (":::", const2 $ logicError "R operator `:::` not allowed") -- prec -2
  , ("$", const2 $ logicError "R operator `$` not allowed")     -- prec -1
  , ("@", const2 $ logicError "R operator `@` not allowed")     -- prec -1
  , ("->", const2 $ logicError "R operator `->` not allowed")   -- prec 12
  , ("->>", const2 $ logicError "R operator `->>` not allowed") -- prec 12
  , ("<-", const2 $ logicError "R operator `<-` not allowed")   -- prec 13
  , ("<<-", const2 $ logicError "R operator `<<-` not allowed") -- prec 13
  , ("=", const2 $ logicError "R operator `=` not allowed")     -- prec 14
  , ("?", const2 $ logicError "R operator `?` not allowed")     -- prec 15
  
  , ("[[", const bbrackFct)     -- prec 0
  -- "[" handled by bracketFct  -- prec 0

  , ("^", rbinopcode ("^", 1))

  , ("unary+", unopcode 2 "+")
  , ("unary-", unopcode 2 "-")
  
  , (":", lbinopcode (":", 3))

  , ("%/%", lbinopcode ("%/%", 4))
  -- any other %op% operators go here with same precedence
  
  , ("*", lbinopcode ("*", 5))
  , ("/", lbinopcode ("/", 5))
  
  , ("+", lbinopcode ("+", 6))
  , ("-", lbinopcode ("-", 6))
  
  , ("<", lbinopcode ("<", 7))
  , (">", lbinopcode (">", 7))
  , ("<=", lbinopcode ("<=", 7))
  , (">=", lbinopcode (">=", 7))
  , ("==", lbinopcode ("==", 7))
  , ("!=", lbinopcode ("!=", 7))

  , ("!", unopcode 8 "!")

  , ("&", lbinopcode ("&", 9))
  , ("&&", lbinopcode ("&&", 9))

  , ("|", lbinopcode ("|", 10))
  , ("||", lbinopcode ("||", 10))

  , ("~", lbinopcode ("~", 11))
  ]
  where
    bbrackFct (hd:tl@(_:_)) =
      printf "%s[[%s]]" (rcodep 0 hd) (rArgsCode tl)
    bbrackFct _ =
      logicError "`[[` must have at least two arguments"
    const2 x _ _ = x

rFctCode :: FctName -> [RExpr] -> String
rFctCode fctName args =
  printf "%s(%s)" fctName (rArgsCode args)

rFctCode' :: FctName -> [RExpr] -> [(String, RExpr)] -> String
rFctCode' fctName unnamed_args named_args =
  printf "%s(%s)" fctName (rArgsCode' unnamed_args named_args)

rArgsCode :: [RExpr] -> String
rArgsCode args = commasep $ map rcode args

rArgsCode' :: [RExpr] -> [(String, RExpr)] -> String
rArgsCode' unnamed_args named_args =
  commasep $ map rcode unnamed_args ++ map namedArgCode named_args
  where
    namedArgCode (name, e) = printf "%s = %s" name (rcode e)

unopcode :: Int -> FctName -> Int -> [RExpr] -> String
unopcode prec op = \n [a] ->
  paren (n < prec) $ op ++ " " ++ rcodep prec a

-- left-associative binary operator
lbinopcode :: (FctName, Int) -> Int -> [RExpr] -> String
lbinopcode (name, p) =
  let sep = printf " %s " name
      pm1 = p - 1
  in \n (hd:tl) ->
       assert (not $ null tl) $
       paren (n < p) $ intercalate sep $ (rcodep p hd) : map (rcodep pm1) tl

-- right-associative binary operator
rbinopcode :: (FctName, Int) -> Int -> [RExpr] -> String
rbinopcode (name, p) =
  let sep = printf " %s " name
      pm1 = p - 1
  in \n-> (\(hd:tl)->
             assert (not $ null tl) $
             paren (n < p) $ intercalate sep $ reverse $
            (rcodep p hd) : map (rcodep pm1) tl
          ) . reverse
