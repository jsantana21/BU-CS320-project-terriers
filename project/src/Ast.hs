module Ast where


data Ast = ValBool Bool
         | And Ast Ast | Or Ast Ast | Not Ast

         | ValInt Int
         | Plus Ast Ast 
         | Minus Ast Ast 
         | Mult Ast Ast 
         | IntegerDiv Ast Ast 
         | Mod Ast Ast  
         | Uminus Ast
         | Exp Ast Ast                  -- exponent

         | ValFloat Float
         | FloatingPointDiv Ast Ast     
         | Fpe Ast Ast              -- floating point exponentiation

         | ValChar Char
         
         | Nil
         | Cons Ast Ast

         | Index Ast Ast 
         | Concat Ast Ast

         | If Ast Ast Ast
         | Let String Ast Ast

         | Var String
         | Lam String Ast
         | InfixLam String Ast
         | App Ast Ast

         | Sep Ast Ast 

         | Eq Ast Ast
         | Neq Ast Ast

         | Lt Ast Ast
         | Lte Ast Ast
         | Gt Ast Ast 
         | Gte Ast Ast 

         | Print Ast


         deriving Eq

instance Show Ast where
  -- display the ast in a readable way
  show ast = showPretty ast 0

  
showFullyParen :: Ast -> String

showFullyParen (ValInt i)       = "(" ++ show i ++ ")"
showFullyParen (ValFloat i)     = "(" ++ show i ++ ")"
showFullyParen (ValChar i)      = "(" ++ show i ++ ")"
showFullyParen (ValBool True)   = "(" ++ "true" ++ ")"
showFullyParen (ValBool False)  = "(" ++ "false" ++ ")"

showFullyParen (And l r)        = "(" ++ (showFullyParen l) ++ " && " ++ (showFullyParen r) ++ ")"
showFullyParen (Or l r)         = "(" ++ (showFullyParen l) ++ " || " ++ (showFullyParen r) ++ ")"
showFullyParen (Not a)          = "(" ++ " ! " ++ (showFullyParen a) ++ ")"

showFullyParen (Plus l r)    = "(" ++ (showFullyParen l) ++ " + " ++ (showFullyParen r) ++ ")"
showFullyParen (Minus l r)   = "(" ++ (showFullyParen l) ++ " - " ++ (showFullyParen r) ++ ")"
showFullyParen (Mult l r)    = "(" ++ (showFullyParen l) ++ " * " ++ (showFullyParen r) ++ ")"
showFullyParen (FloatingPointDiv l r)      = "(" ++ (showFullyParen l) ++ " / " ++ (showFullyParen r) ++ ")"
showFullyParen (IntegerDiv l r)     = "(" ++ (showFullyParen l) ++ " // " ++ (showFullyParen r) ++ ")"
showFullyParen (Mod l r)        = "(" ++ (showFullyParen l) ++ " % " ++ (showFullyParen r) ++ ")"
showFullyParen (Exp l r)        = "(" ++ (showFullyParen l) ++ " ** " ++ (showFullyParen r) ++ ")"
showFullyParen (Index xs x)     = "(" ++ (showFullyParen xs) ++ " !! " ++ (showFullyParen x) ++ ")"
showFullyParen (Uminus x)    = "(" ++ "-" ++ show x ++ ")"
showFullyParen (Print b)        = "print(" ++ showFullyParen b ++ ")"
showFullyParen (Fpe l r)        = "(" ++ (showFullyParen l) ++ " ^ " ++ (showFullyParen r) ++ ")"

showFullyParen (If b t e)       = "(if " ++ (showFullyParen b) ++ " then " ++ (showFullyParen t) ++ " else " ++ (showFullyParen e) ++ ")"
showFullyParen (Let v a bod)    = "(let " ++ v ++ " = " ++ (showFullyParen a) ++ " in " ++ (showFullyParen bod) ++ ")"
showFullyParen (InfixLam v bod) = "(" ++ v ++ " . " ++ (showFullyParen bod) ++ ")"
showFullyParen (Lam v bod)      = "(\\ " ++ v ++ " -> " ++ (showFullyParen bod) ++ ")"
showFullyParen (App f a)        = "( " ++ (showFullyParen f)  ++ " " ++ (showFullyParen a) ++ ")"
showFullyParen (Var s)          = "( " ++ s ++ ")"
showFullyParen (Cons h t)       = "(" ++ (showFullyParen h)  ++ " : " ++ (showFullyParen t) ++ ")"
showFullyParen Nil              = "( [] )"

showFullyParen (Concat xs ys)   = "(" ++ (showFullyParen xs) ++ " ++ " ++ (showFullyParen ys) ++ ")"
showFullyParen (Sep l r)        = "(" ++ showFullyParen l ++ " ; " ++  showFullyParen r ++ ")"


showFullyParen (Eq l r)      = "(" ++ (showFullyParen l) ++ " == " ++ (showFullyParen r) ++ ")"
showFullyParen (Neq l r)     = "(" ++ (showFullyParen l) ++ " /= " ++ (showFullyParen r) ++ ")"

showFullyParen (Lt l r)      = "(" ++ (showFullyParen l) ++ " < " ++ (showFullyParen r) ++ ")"
showFullyParen (Lte l r)     = "(" ++ (showFullyParen l) ++ " <= " ++ (showFullyParen r) ++ ")"
showFullyParen (Gt l r)      = "(" ++ (showFullyParen l) ++ " > " ++ (showFullyParen r) ++ ")"
showFullyParen (Gte l r)     = "(" ++ (showFullyParen l) ++ " >= " ++ (showFullyParen r) ++ ")"

-- | provide a nice show with minimal parentheses

showPretty :: Ast -> Integer -> String
showPretty (ValInt i) _        =  if i < 0
                                  then  "(" ++ show i ++ ")"
                                  else show i
showPretty (ValFloat i) _      =  if i < 0
                                  then  "(" ++ show i ++ ")"
                                  else show i
showPretty (ValChar i) _          =  "(" ++ show i ++ ")"
                           
showPretty (ValBool True) _       =  "true"
showPretty (ValBool False) _      = "false"
showPretty Nil _                  = "[]"
showPretty (Var s) _              = s

showPretty (Sep l r)  i           = parenthesize 1 i $ ((showPretty l 1) ++ " ; " ++  (showPretty r 0)) 

showPretty (Lam v bod) i          = parenthesize 1 i $ "\\ " ++ v ++ " -> " ++ (showPretty bod 100)
showPretty (InfixLam v bod) i     = parenthesize 1 i $ v ++ " . " ++ (showPretty bod 100)
showPretty (Let v a bod) i        = parenthesize 1 i $  "let " ++ v ++ " = " ++ (showPretty a 1) ++ " in " ++ (showPretty bod 1)
showPretty (If b t e) i           = parenthesize 1 i $  "if " ++ (showPretty b 1) ++ " then " ++ (showPretty t 1) ++ " else " ++ (showPretty e 1)

showPretty (App l r) i            = parenthesize 2 i $ (showPretty l 2) ++ " " ++ (showPretty r 3)

showPretty (Or l r) i             = parenthesize 4 i $ (showPretty l 4) ++ " || " ++ (showPretty r 5)

showPretty (And l r) i            = parenthesize 6 i $ (showPretty l 6) ++ " && " ++ (showPretty r 7)

showPretty (Eq l r)  i         = parenthesize 8 i $ (showPretty l 9) ++ " == " ++ (showPretty r 9)
showPretty (Neq l r)  i        = parenthesize 8 i $ (showPretty l 9) ++ " /= " ++ (showPretty r 9)
showPretty (Lt l r) i          = parenthesize 8 i $ (showPretty l 9) ++ " < " ++ (showPretty r 9) 
showPretty (Lte l r) i         = parenthesize 8 i $ (showPretty l 9) ++ " <= " ++ (showPretty r 9) 
showPretty (Gt l r) i          = parenthesize 8 i $ (showPretty l 9) ++ " > " ++ (showPretty r 9) 
showPretty (Gte l r) i         = parenthesize 8 i $ (showPretty l 9) ++ " >= " ++ (showPretty r 9) 

showPretty (Cons l r) i           = parenthesize 10 i $ (showPretty l 11) ++ " : " ++ (showPretty r 10)
showPretty (Concat xs ys) i       = parenthesize 10 i $ (showPretty xs 11) ++ " ++ " ++ (showPretty ys 10)

showPretty (Minus l r) i       = parenthesize 12 i $ (showPretty l 12) ++ " - " ++ (showPretty r 13)
showPretty (Plus l r) i        = parenthesize 12 i $ (showPretty l 12) ++ " + " ++ (showPretty r 13)

showPretty (Mult l r) i        = parenthesize 14 i $ (showPretty l 14) ++ " * " ++ (showPretty r 15)
showPretty (FloatingPointDiv l r) i          = parenthesize 14 i $ (showPretty l 14) ++ " / " ++ (showPretty r 15)
showPretty (IntegerDiv l r) i         = parenthesize 14 i $ (showPretty l 14) ++ " // " ++ (showPretty r 15)
showPretty (Mod l r) i            = parenthesize 14 i $ (showPretty l 14) ++ " % " ++ (showPretty r 15)

showPretty (Fpe l r) i            = parenthesize 16 i $ (showPretty l 17) ++ " ^ " ++ (showPretty r 16)
showPretty (Exp l r) i            = parenthesize 16 i $ (showPretty l 17) ++ " ** " ++ (showPretty r 16)

showPretty (Index l r) i          = parenthesize 18 i $ (showPretty l 18) ++ " !! " ++ (showPretty r 19)

showPretty (Not l) i              = parenthesize 20 i $  " ! " ++ (showPretty l 20)
showPretty (Uminus l) i        = parenthesize 20 i $  " - " ++ (showPretty l 20)
showPretty (Print b) i            = "print(" ++ showPretty b 20 ++ ")" 





parenthesize :: Integer -- ^ the precedence level of outer expression
              -> Integer -- ^ the precedence level of the current expression
              -> String -- ^ string representation current expression
              -> String -- ^ the properly (not necessarily fully) parenthesized current expression

parenthesize outerLevel curLevel showExp 
  | outerLevel < curLevel = "(" ++ showExp ++ ")" 
  | otherwise             =        showExp
