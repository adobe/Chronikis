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
module RNames (makeRName)
where

import Data.Char
import Data.Set (Set, member)
import qualified Data.Set as Set

-- Valid R variable names:
-- a. Only ASCII letters, numbers, '.', '_'
-- b. Starts with letter or '.'
-- c. Does not start with '.' followed by digit
-- d. Not a reserved identifier:
--    - Not a member of reservedR.
--    - Not of the form ".." ++ show n for a positive integer n

-- R's algorithm:
-- 1. Translate invalid characters to '.'
-- 2. If restrictions on initial substring violated, prepend 'X'
-- 3. If name matches R keyword, append '.'
--
makeRName :: String -> String
makeRName = renameReserved . makeValidInitial . map translateInvalidChar

translateInvalidChar :: Char -> Char
translateInvalidChar c =
  if isAscii c && (isAlphaNum c || c == '_' || c == '.')
    then c
    else '.'

makeValidInitial :: String -> String
makeValidInitial s =
  if isValidInitial s then s else 'X' : s
  where
    isValidInitial "" =
      False
    isValidInitial (c : rest) =
      (isAlpha c || c == '.') && not (startsWithDigit rest)
    startsWithDigit "" =
      False
    startsWithDigit (c : _) =
      isDigit c

renameReserved :: String -> String
renameReserved s =
  if isReserved s
    then s ++ "."
    else s

isReserved :: String -> Bool
isReserved s =
  s `member` reservedR || isDotDotInt s
  where
    isDotDotInt ('.' : '.' : rest@(_:_)) =
      all isDigit rest
    isDotDotInt _  =
      False

reservedR :: Set String
reservedR = Set.fromList
  [ "if", "else", "repeat", "while", "function", "for", "in", "next", "break"
  , "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_"
  , "NA_real_", "NA_complex_", "NA_character_", "..."
  ]

