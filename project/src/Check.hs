module Check where

import Data.Set (Set)
import qualified Data.Set as Set

import Ast

-- here you can preform static checks


-- | The data type for all the static check warning
-- Some example include:
--   * use of undefined variable
--   * defined but unused variable
--   * type errors

data WarningMsg = 
    UndefinedVarUse String  -- Mix-In Feature: Warn when a variable is introduced but never used
    | DefinedButUnused String
    | TypeError String
    | Ok String
  
  deriving (Show,Eq, Ord)

-- | perform static checking on the Ast
-- the output a set of warning on that input Ast
check :: Ast -> Set WarningMsg
check t = case (helperUndefined t Set.empty) of
	set -> warningList (Set.toList set)
	_   -> undefined

warningList :: [String] -> Set WarningMsg
warningList [] = Set.empty
warningList (x:xs) = Set.insert (UndefinedVarUse x) (warningList xs)


helperUndefined :: Ast -> Set String-> Set String
helperUndefined (ValInt _) _ 		                	= Set.empty
helperUndefined (ValFloat _) _		                = Set.empty
helperUndefined (ValBool _) _    	                = Set.empty
helperUndefined (ValChar _) _			                = Set.empty

helperUndefined (And l r ) goodVars		            = (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)
helperUndefined (Or l r) goodVars	  	            = (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)
helperUndefined (Not _) _			                   	= Set.empty

helperUndefined (Uminus _) _			                = Set.empty

helperUndefined (Plus l r) goodVars 	            = (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)
helperUndefined (Minus l r) goodVars              = (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)
helperUndefined (Mult l r) goodVars             	= (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)
helperUndefined (FloatingPointDiv l r) goodVars 	= (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)
helperUndefined (IntegerDiv l r) goodVars         = (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)
helperUndefined (Exp l r) goodVars 		            = (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)
helperUndefined (Fpe l r) goodVars 	            	= (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)

helperUndefined (Mod l r) goodVars 	            	= (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)

helperUndefined (Var s) goodVars 	              	= if Set.member s goodVars then Set.empty else Set.singleton "Undefined Variable!" 
helperUndefined (Lam s a) goodVars              	= (helperUndefined a (Set.insert s goodVars)) 
helperUndefined (App l r) goodVars                = (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)
helperUndefined (Sep l r) goodVars                = (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)

helperUndefined (Eq l r) goodVars 	              = (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)
helperUndefined (Neq l r) goodVars              	= (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)
helperUndefined (Lt l r) goodVars                 = (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)
helperUndefined (Lte l r) goodVars                = (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)
helperUndefined (Gt l r) goodVars                 = (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)
helperUndefined (Gte l r) goodVars                = (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)

helperUndefined (Nil) _					                  = Set.empty
helperUndefined (Cons l r) goodVars	             	= (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)

helperUndefined (Concat l r) goodVars             = (helperUndefined (l) goodVars) `Set.union` (helperUndefined (r) goodVars)
helperUndefined (If i t e) goodVars	            	= Set.union (Set.union (helperUndefined (i) goodVars) (helperUndefined (t) goodVars)) (helperUndefined (e) goodVars)
helperUndefined (Let s x y) goodVars              = undefined

helperUndefined (Print a) goodVars	            	= Set.empty







