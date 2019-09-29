{- Author: Tessa Pham
   File: PreL.hs
   Description: Defines a main action for the Pre-λ interpreter, as well as functions
   that combine multiple interpretation phases.
-}

module Main where

import Control.Exception
import System.Exit
import Control.Monad

import Syntax
import Token
import Parser
import Lexer
import Eval

main :: IO ()
main = do

  -- primary user interaction commands
  putStrLn ""
  putStrLn "Enter an expression:"
  expr_string <- getLine

  -- allow users to quit
  when (expr_string == "quit")
    exitSuccess

  catch (do value <- evaluate (evalString expr_string)
            print value)
        (\ (SomeException e) -> print e)
    
  main

-- Lex and parse an expression string.
-- Call `error` if the input is somehow malformed.
lexParse :: String -> Expr
lexParse str
  | length leftover == 0 = ex
  | otherwise = error "malformed input"
  where ex = fst (parse (lexPreL str))
        leftover = snd (parse (lexPreL str))

-- Lex, parse, and evaluate an expression string.
-- Call `error` if the input is somehow malformed or cannot be evaluated.
evalString :: String -> Value
evalString str
  = eval (lexParse str)
