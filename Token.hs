{- File: Token.hs

   Defines lexical tokens.
-}

module Token where

import Syntax

data Token
  = LiteralT Value     -- numbers, booleans
  | IfT                -- "if"
  | ThenT              -- "then"
  | ElseT              -- "else"
  | OpT Op             -- binary operators
  | NotT               -- "not"
  | LambdaT            -- "\"
  | VarT String        -- e.g. "x" "y"
  | DotT               -- "."
  | AppT               -- "@"
  deriving Show
