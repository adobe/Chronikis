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
{-# LANGUAGE TupleSections #-}
module Parser (
  expr, type_spec, parm_type_spec, fctDef, program
  , whiteSpace, Parsec.eof
  , parse, parseOnly, Parser
  -- the rest are exported only for testing
  , opExpr, term, number
  )
where

import Control.Applicative ((<|>))
--import Data.Functor.Identity (Identity)
import Data.List (genericLength)
import Data.Maybe (isNothing, fromMaybe)
import qualified Data.Scientific as Scientific
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

import Misc
import AST
import Number

--- Utilities ---

type Parser a = Parsec.Parsec String () a
--type Oper a = Operator String () Identity a

parse :: Parser a -> String -> Either Parsec.ParseError a
parse rule text = Parsec.parse rule "(source)" text

parseOnly :: Parser a -> String -> Either Parsec.ParseError a
parseOnly rule text = parse (whiteSpace *> rule <* Parsec.eof) text

ptry :: Parser a -> Parser a
ptry = Parsec.try

--- Lexer ---

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser 
        emptyDef
        { Token.commentStart = "/*"
        , Token.commentEnd   = "*/"
        , Token.commentLine  = "//"
        , Token.nestedComments = True
        , Token.identStart = Parsec.letter
        , Token.identLetter = Parsec.alphaNum <|> Parsec.char '_'
        , Token.reservedNames = ["int", "real"]
        , Token.reservedOpNames =
          ["^", "+", "-", "*", "/", "->", "=", "~", "div"]
        , Token.caseSensitive = True
        }

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

comma :: Parser String
comma = Token.comma lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Token.commaSep1 lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

semi :: Parser String
semi = Token.semi lexer

number :: Parser Number
number =
  do
    -- n.f * 10^e
    n <- integerp
    f <- fractionp
    e <- exponentp
    whiteSpace
    x <- if isNothing f && isNothing e
         then return $ IntVal n
         else case convertReal n (fromMaybe 0 f) (fromMaybe 0 e) of
                Left msg -> Parsec.parserFail msg
                Right x  -> return $ RealVal x
    if is_representable_number x
      then return x
      else Parsec.parserFail $
           case x of
             IntVal _  -> "Integer literal exceeds range of 32-bit signed ints"
             RealVal _ -> "Numeric literal exceeds range of 64-bit IEEE floats"
  where
    integerp :: Parser Integer
    integerp = read <$> digitsp

    digitsp :: Parser String
    digitsp = Parsec.many1 Parsec.digit
    
    fractionp :: Parser (Maybe Scientific)
    fractionp = Parsec.option Nothing $ Just <$> do
      _ <- Parsec.char '.'
      str <- digitsp
      return $ scientific (read str) (negate $ length str)

    exponentp :: Parser (Maybe Integer)
    exponentp = Parsec.option Nothing $ Just <$> do
      _ <- Parsec.oneOf "eE"
      sign <- Parsec.option '+' $ Parsec.oneOf "+-"
      e <- integerp
      case sign of
        '+' -> return e
        '-' -> return (-e)
        _   -> fail "Logical error in program"

    convertReal :: Integer-> Scientific-> Integer-> Either ErrorMsg Scientific
    convertReal n f e =
      let x = Scientific.normalize $ scientific n 0 + f
          e1 = e + (toInteger $ Scientific.base10Exponent x)
      in
        if e1 > toInteger (maxBound::Int) || e1 < toInteger (minBound::Int) then
          Left "Exponent of floating-point number is too large"
        else
          Right $ Scientific.normalize $ x * scientific 1 (fromInteger e)

--- Expr parsing ---
var0 :: VarName -> Expr0
lit0 :: Number -> Expr0
let0 :: VarName -> Expr0 -> Expr0 -> Expr0
draw0 :: VarName -> Expr0 -> Expr0 -> Expr0
apply0 :: FctName -> [Expr0] -> Expr0
var0 v = Var () v
lit0 x = Lit () x
let0 v edef ebody = Let () v edef ebody
draw0 v edistr ebody = Draw () v edistr ebody
apply0 fctName args = Apply () fctName args

expr :: Parser Expr0
expr = (identAndOp "=" >>= letExpr)
       <|> (identAndOp "~" >>= drawExpr)
       <|> opExpr
  where
    identAndOp op = ptry (identifier <* reservedOp op)
    letExpr name = let0 name <$> (opExpr <* semi) <*> expr
    drawExpr name = draw0 name <$> (opExpr <* semi) <*> expr

opExpr :: Parser Expr0
opExpr = buildExpressionParser opTable term where
  opTable =
    [ [Postfix indexAppExpr]
    , [ Prefix (reservedOp "-" >> return (\e -> apply0 "negate" [e]))
      , Prefix (reservedOp "+" >> return id)
      ]
    , [binary "^" AssocRight]
    , [binary "*" AssocLeft, binary "/" AssocLeft, binary "div" AssocLeft]
    , [binary "+" AssocLeft, binary "-" AssocLeft]
    ]
  binary name assoc =
    flip Infix assoc $ reservedOp name >> return (\e1 e2-> apply0 name [e1, e2])
  indexAppExpr = do
    es <- brackets (commaSep1 expr)
    return $ \e -> apply0 "[]" (e : es)

term :: Parser Expr0
term = (ptry (identifier <* lookAheadParen) >>= fctapp)
       <|> fmap var0 identifier
       <|> fmap lit0 number
       <|> fmap (apply0 "{}") (braces $ commaSep1 expr)
       <|> parens expr
  where
    lookAheadParen = Parsec.lookAhead $ Parsec.char '('
    fctapp name = apply0 name <$> parens (commaSep expr)

{-
basicExpr :: Parser Expr0
basicExpr = buildExpressionParser opTable basicTerm where
  opTable = mkOpTable indexAppExpr binary
  binary name assoc =
    flip Infix assoc $
    reservedOp name >> return (\e1 e2-> apply0 name [e1, e2])
  indexAppExpr = do
    es <- brackets (commaSep1 basicExpr)
    return $ \e -> apply0 "[]" (e : es)

basicTerm :: Parser Expr0
basicTerm =
  (ptry (identifier <* lookAheadParen) >>= fctapp)
  <|> fmap var0 identifier
  <|> fmap lit0 number
  <|> parens basicExpr
  where
    lookAheadParen = Parsec.lookAhead $ reservedOp "("
    fctapp name = apply0 name <$> parens (commaSep basicExpr)
-}

--- Type parsing ---

elem_type :: Parser ElemType
elem_type = (IntType <$ reserved "int")
            <|> (RealType <$ reserved "real")
            <?> "element type"

type_spec :: Parser Type
type_spec =
  do
    (typ, _, _) <- gentype_spec (return ()) $ ((), ) <$> dims
    return typ
  where
    dims = Parsec.option 0 $
            ((+ 1) . genericLength) <$> (brackets $ Parsec.many comma)

parm_type_spec :: Parser TypeSB0
parm_type_spec =
  do
    (typ, bnds, shape) <- gentype_spec bndspec shspec
    return $ typeSB typ shape bnds
  where
    shspec = do
      sh <- Parsec.option [] $ brackets $ commaSep1 opExpr
      return (sh, genericLength sh)
    bndspec = Parsec.option nobounds $ braces $ do
      lo <- Parsec.option Nothing $ Just <$> opExpr
      _ <- comma
      hi <- Parsec.option Nothing $ Just <$> opExpr
      return $ Bounds lo hi
    
gentype_spec :: Parser b -> Parser (sh, Natural) -> Parser (Type, b, sh)
gentype_spec bounds_parser shape_parser = do
  et <- elem_type
  bnds <- bounds_parser
  (shape, nd) <- shape_parser
  ts <- Parsec.option False (True <$ Parsec.char '$')
  d  <- Parsec.option False (True <$ Parsec.char '~')
  whiteSpace
  let typ =
        Type{ elemType = et, numDim = nd, isTS = ts, isDistr = d }
  return (typ, bnds, shape)

--- FctDef parsing ---

fctDef :: Parser FctDef0
fctDef =
  FctDef
  <$> (reserved "def" *> identifier)
  <*> (extendTypes <$> parens (commaSep identsType))
  <*> (Parsec.option TSDType $ reservedOp ":" *> type_spec)
  <*> (reservedOp "=" *> expr)
  where
    identsType =
      (,) <$> (commaSep1 identifier) <*> (reservedOp ":" *> parm_type_spec)
    extendTypes xss = concat $ do
      (vars, typ) <- xss
      return [(v, typ) | v <- vars]

program :: Parser [ FctDef0 ]
program = whiteSpace *> Parsec.many1 fctDef <* Parsec.eof
