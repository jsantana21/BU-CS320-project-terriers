module ParserTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Test.Tasty.QuickCheck -- Mix-In: Writing a quickcheck generator and shrinker for your Ast and using it to test your parser

import ParserMonad
import Ast
import Parser

-- This will generate random instances of types
instance Arbitrary Ast where
    arbitrary = sized arbitrarySizedAst

-- recursively and randomly generate instances up to a given size limit

arbitrarySizedAst ::  Int -> Gen Ast
arbitrarySizedAst m | m < 1 = do i    <- arbitrary -- chooses a random Integer
                                 b    <- arbitrary -- chooses a random Boolean
                                 f    <- arbitary  -- chooses a random Float
                                 c    <- arbitrary -- chooses a random Char
                                 node <- elements [ValInt i, ValFloat f, ValBool b, ValChar c,
                                                   Nil]  -- so put all the non-recursive Ast expressions here
                                 return $ node
arbitrarySizedAst m | otherwise = do l <- arbitrarySizedAst (m `div` 2)  -- get ast half as big
                                     r <- arbitrarySizedAst (m `div` 2)  -- ditto
                                     x <- elements ["x", "y", "z"]   -- will choose random element from the list
                                     ifAst <- arbitrarySizedIf m
                                     node <- elements [Plus l r,     -- list here all your binary Ast constructors
                                                       Minus l r,
                                                       Mult l r,
                                                       ifAst,        -- chooses from if expressions
                                                       Let x l r,    -- takes a string and two asts
                                                       Lam x l,      -- takes a string and one ast
                                                       InfixLam x l, -- takes a string and one ast
                                                       And l r,
                                                       Or l r,
                                                       Not l,
                                                       UMinus l,
                                                       FloatingPointDiv l r,
                                                       IntegerDiv l r,
                                                       Fpe l r,
                                                       Exp l r,
                                                       Index l r,
                                                       Var x
                                                       Cons l r,
                                                       Concat l r,
                                                       App l r,
                                                       Sep l r,
                                                       Eq l r,
                                                       Neq l r,
                                                       Lt l r,
                                                       Lte l r,
                                                       Gt l r,
                                                       Gte l r,                                                       
                                                       Print l
                                                     ]
                                     return node

-- break in thirds for mix-fix operators which have three separate sub-asts

arbitrarySizedIf ::  Int -> Gen Ast
arbitrarySizedIf m = do x <- arbitrarySizedAst (m `div` 3)
                        y <- arbitrarySizedAst (m `div` 3)
                        z <- arbitrarySizedAst (m `div` 3)
                        return $ If x y z

parserTest = testGroup
      "parser Test"
      [
      testProperty "parse should return the same AST when fully parenthesized" $
                  ((\ x -> Just (x , "") == (parse parser $ showFullyParen x)) :: Ast -> Bool),

      testProperty "parse should return the same AST when pretty printed" $
                  ((\ x -> Just (x , "") == (parse parser $ showPretty x 0)) :: Ast -> Bool)
      ]
