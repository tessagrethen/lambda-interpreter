{- Author: Tessa Pham
   File: Parser.hs
   Description: Parses the Pre-λ syntax.
-}

module Parser where

import Token
import Syntax

-- Parse an expression, returning the parsed expression and
-- a list of unconsumed tokens.
-- Call `error` if the list of tokens has no valid parse.

getExpr :: [Token] -> Expr
getExpr ts = fst (parse ts)

getRest :: [Token] -> [Token]
getRest ts = snd (parse ts)

parse :: [Token] -> (Expr, [Token])
parse [] = error "no valid parse"
parse ((VarT n) : ts) = (VarE n, ts)
parse ((LiteralT n) : ts) = (ValueE n, ts)
parse (NotT : ts) = (NotE ex1, re1)
  where ex1 = getExpr ts
        re1 = getRest ts
parse ((OpT op) : ts) = (OpE op ex1 ex2, re2)
  where ex1 = getExpr ts
        re1 = getRest ts
        ex2 = getExpr re1
        re2 = getRest re1
parse (AppT : ts) = (AppE ex1 ex2, re2)
  where ex1 = getExpr ts
        re1 = getRest ts
        ex2 = getExpr re1
        re2 = getRest re1
parse (IfT : ts) = (IfE ex1 ex2 ex3, re3)
  where ex1 = getExpr ts
        re1 = getRest ts
        ex2 = getExpr re1
        re2 = getRest re1
        ex3 = getExpr re2
        re3 = getRest re2
parse (ThenT : ts) = parse ts
parse (ElseT : ts) = parse ts
parse (LambdaT : (VarT n) : ts) = (lambdaEx, re)
  where lambdaEx = ValueE (LambdaV n (getExpr ts))
        re = getRest ts
parse (DotT : ts) = parse ts
