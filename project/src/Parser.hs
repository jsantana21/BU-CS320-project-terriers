module Parser where

import Ast
import ParserMonad


-- | parser for the language
parser :: Parser Ast
parser = apps


keywords = ["if","then","else", "let", "in", "true","false"]
boolean = ["true", "false"]

vars :: Parser Ast
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s

ints :: Parser Ast
ints = do s <- intParser
          return $ ValInt s

floats :: Parser Ast
floats = do s <- token $ floatParser
            return $ValFloat s

chars :: Parser Ast
chars = do s <- token $ charParser
           return $ValChar s

bools :: Parser Ast
bools = do x <- token $ varParser
           if (x `elem` boolean)
            then (if x == "true" then return (ValBool True) else return (ValBool False))
            else failParse

nil :: Parser Ast
nil = do token $ literal "[]"
         return Nil

separatorExpr :: Parser Ast
separatorExpr = withInfix apps [(";", Sep)]

apps :: Parser Ast
apps = withInfix orExpr [("",App)]

orExpr :: Parser Ast
orExpr = withInfix andExpr [("||", Or)]

andExpr :: Parser Ast
andExpr = withInfix relationalExpr [("&&", And)]

relationalExpr :: Parser Ast
relationalExpr = withInfix cons [("==", Eq), ("/=", Neq),
                                 ("<", Lt), ("<=", Lte), (">", Gt), (">=", Gte)
                                ]

cons :: Parser Ast
cons = (do x <- orExpr
           token $ literal ":"
           x' <- token cons
           return (Cons x x')) <|> concatExpr

concatExpr :: Parser Ast
concatExpr = (do x <- orExpr
                 token $ literal "++"
                 x' <- token concatExpr
                 return (Concat x x')) <|> addSubExpr


addSubExpr :: Parser Ast
addSubExpr = withInfix multDivExpr [("+", Plus), ("-", Minus)]

multDivExpr :: Parser Ast
multDivExpr = withInfix exponentExpr [("*", Mult), ("/", FloatingPointDiv), ("//", IntegerDiv), ("%", Mod)]

exponentExpr :: Parser Ast
exponentExpr = withInfix  indexingExpr [("^", Fpe), ("**", Exp)]

indexingExpr :: Parser Ast
indexingExpr = withInfix notExp [("!!", Index)]

notExp :: Parser Ast
notExp = (do token $ literal "!"
             a <- notExp
             return $ Not a) <|> uminusExpr

uminusExpr :: Parser Ast
uminusExpr = (do token $ literal "-"
                 a <- uminusExpr
                 return $ Uminus a) <|> printExpr

printExpr :: Parser Ast
printExpr = (do token $ literal "print"
                x <- parser
                return $ Print x) <|> atoms

atoms:: Parser Ast
atoms = ints <|> bools <|> chars <|> floats <|> nil <|>  vars <|> lambdaParser <|> infixLambdaParser <|> letParser <|> ifParser <|> parens

parens :: Parser Ast
parens = do token $ literal "("
            x <- parser
            token $ literal ")"
            return x

lambdaParser :: Parser Ast
lambdaParser = do token $ literal "\\"
                  x <- varParser
                  token $ literal "->"
                  y <- parser
                  return (Lam x y)

infixLambdaParser :: Parser Ast
infixLambdaParser = do 
    x <- varParser
    token $ literal " . "
    y <- parser
    return (Lam x y)

letParser :: Parser Ast
letParser = do token $ literal "let"
               x <- varParser
               token $ literal "="
               y <- parser
               token $ literal "in"
               z <- parser 
               return (Let x y z)

ifParser :: Parser Ast
ifParser = do token $ literal "if"
              x <- parser
              token $ literal "then"
              y <- parser
              token $ literal "else"
              z <- parser
              return (If x y z)
