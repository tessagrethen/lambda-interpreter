{- Author: Tessa Pham
   File: Lexer.hs
   Description: Lexes the syntax for the Pre-λ interpreter.
-}

module Lexer where

import Data.Char
import Text.Read

import Token
import Syntax

-- Lex a Preλ expression into a list of tokens.
-- Call `error` if there is a lexical error (something that doesn't lex).

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

listOp = ["+", "-", "*", "/", "<", ">", "=", "<=", ">=", "/="]

listKeyword = ["if", "then", "else", "not", "\\", ".", "@"]

listBoolean = ["true", "false"]

listToken :: [(String, Token)]
listToken = [("if", IfT), ("then", ThenT), ("else", ElseT), ("not", NotT), ("\\", LambdaT), (".", DotT), ("@", AppT), ("true", LiteralT (BoolV True)), ("false", LiteralT (BoolV False)), ("+", OpT Plus), ("-", OpT Minus), ("*", OpT Times), ("/", OpT Divides), ("<", OpT LessThan), (">", OpT GreaterThan), ("=", OpT Equals), ("<=", OpT LessThanEquals), (">=", OpT GreaterThanEquals), ("/=", OpT NotEquals)]

isStartCmt '{' = True
isStartCmt _   = False

isEndCmt '}' = True
isEndCmt _   = False

isParenthesis '(' = True
isParenthesis ')' = True
isParenthesis _   = False

isOp :: String -> Bool
isOp [] = False
isOp [c] = [c] `elem` listOp
isOp (c:str) = take 2 (c:str) `elem` listOp || isOp [c]

-- Input into giveOp should already pass isOp.
giveOp :: String -> String
giveOp [c] = [c]
giveOp (c:str)
  | take 2 (c:str) `elem` listOp = take 2 (c:str)
  | otherwise = [c]

isKeyword str = str `elem` listKeyword

isBoolean str = str `elem` listBoolean

isVar str = not (isKeyword str || isBoolean str)

isComment :: String -> Bool
isComment [] = False
isComment [c]
  | isEndCmt c = True
  | otherwise = False
isComment (c:str)
  | isEndCmt c = True
  | otherwise = isComment str
  
-- Input passed into takeComment should already pass isComment.
takeComment :: [Char] -> [Char]
takeComment [c] = []
takeComment (c:str)
  | isEndCmt c = str
  | otherwise = takeComment str

giveToken :: String -> Token
giveToken str = find str listToken

findToken :: String -> String
findToken [] = []
findToken [c]
  | isSpace c || isParenthesis c = []
  | otherwise = [c]
findToken (c:str)
  | isSpace c || isParenthesis c = findToken str
  | isStartCmt c && isComment str = findToken (takeComment str)
  | isStartCmt c && not (isComment str) = error "unterminated comment"
  | otherwise = (c:str)

cutString :: String -> String -> String
cutString [_] [] = []
cutString substr str = drop (length substr) str

lex1 :: Char -> String -> (Token, String)
lex1 c str
  | isDigit c
  , (more_digits, rest) <- span isDigit str
  , Just n <- readMaybe (c:more_digits)
  = (LiteralT (IntegerV n), rest)
  
  | isAlpha c
  , (more_letters, rest) <- span isAlpha str
  , s <- (c:more_letters)
  , isVar s
  = (VarT s, rest)
  
  | isAlpha c
  , (more_letters, rest) <- span isAlpha str
  , s <- (c:more_letters)
  , isKeyword s || isBoolean s
  = (giveToken s, rest)
  
  | isKeyword [c]
  = (giveToken [c], str)

  | isOp [c]
  , (o, rest) <- (giveOp (c:str), drop (length (giveOp (c:str))) (c:str))
  , isOp o
  = (giveToken o, rest)

lexNoPrefix :: String -> [Token]
lexNoPrefix [] = []
lexNoPrefix (c:str) = token : lexPreL rest
  where
    (token, rest) = lex1 c str

lexPreL :: String -> [Token]
lexPreL [] = []
lexPreL str = lexNoPrefix (findToken str)
