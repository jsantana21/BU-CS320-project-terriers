module Eval where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import EnvUnsafeLog

import Ast

run :: Ast -> (Unsafe Val, Env)
run ast = runEnvUnsafeLog (eval ast) stdLib

stdLib = Map.fromList


  [("head", Fun $ \v ->
   	case v of
   		Ls (x:_) -> (Ok $ x)
   		_ -> Error "you can't call a head on an empty list"),

  ("tail", Fun $ \ v ->
	case v of
		Ls (_:ls) -> Ok $ Ls ls
		_         -> Error "you can't call a head on an empty list"),
{-}
   ("elem", Fun $ \v ->
	case v of
		Fun _ -> (Error "you need a function here")
		_ -> (Ok (Fun $ \xs ->
			case xs of
				Ls ls -> (Ok (B (elem v ls)))
				_     -> Error "you can't call a head on an empty list"))),

   ("map", Fun $ \f ->
	case f of
		Fun f -> 
			(Ok ( Fun $ \xs ->
				case xs of
					Ls ls -> map f ls
					_     -> Error "you can't call a head on an empty list"))
		_    -> Error "you can't call a head on an empty list"),

		
   ("filter", Fun $ \f -> 
   	\ys ->
   	case f of
   		Fun (x) -> case ys of
   			(Ok (Ls ys)) -> map x ys
   			_     -> [Error "this is not a list of integers"]
   		_       -> [Error "this is not a function"]),
-}
   ("ord", Fun $ \v ->
   	case v of
   		C (x) -> Ok $ (I (ord x))
   		_     -> Error "this is not a ord"),

   ("chr", Fun $ \v ->
   	case v of
   		I (x) -> Ok $ (C (chr x))
   		_     -> Error "this is not a chr"),

   ("float", Fun $ \v ->
	case v of
		I (x) -> Ok $ F $ fromIntegral x
		_     -> Error "that's not an float"), -- fromIntegral, fromFloat?

   ("int", Fun $ \v ->
   	case v of
   		F (x) -> Ok $ I $ truncate x
		_  -> Error "that's not an int")]
		   


-- the goal of the program is to return a value, what values are possible?
data Val = I Int
		  | B Bool
		  | F Float
		  | S String
		  | C Char
		  | Ls [Val]
		  | Fun (Val -> (Unsafe Val))


instance Show Val where
  show (I i) = show i
  show (B b) = show b
  show (F f) = show f
  show (S s) = show s
  show (C c) = show c
  show (Ls ls) = show ls
  show (Fun _) = "\\x -> " -- no good way to show a function


type Env = Map String Val


evalInt :: Ast -> EnvUnsafeLog Env Int
evalInt x = do
	res <- eval x
	case res of
		I i -> return i
		_    -> err "value is not an integer"

evalBool :: Ast -> EnvUnsafeLog Env Bool
evalBool x = do
	res <- eval x
	case res of
		B b -> return b
		_    -> err "value is not a bool"

evalFloat :: Ast -> EnvUnsafeLog Env Float
evalFloat x = do
	res <- eval x
	case res of
		F f -> return f
		_   -> err "value is not a float"

evalString :: Ast -> EnvUnsafeLog Env String
evalString x = do
	res <- eval x
	case res of
		S s -> return s
		_   -> err "value is not a string"

evalChar :: Ast -> EnvUnsafeLog Env Char
evalChar x = do
	res <- eval x
	case res of
		C c -> return c
		_   -> err "value is not a char"

evalList :: Ast -> EnvUnsafeLog Env [Val]
evalList x = do
	res <- eval x
	case res of
		Ls xs -> return xs
		_     -> err "value is not a list"

evalFunc :: Ast -> EnvUnsafeLog Env (Val -> Unsafe Val)
evalFunc x = do
	res <- eval x
	case res of
		Fun f -> return f
		_     -> err "value is not a function"


eval :: Ast -> EnvUnsafeLog Env Val

-- bools

eval (ValBool x) = (return (B x))

eval (And x y) = do
	boolA <- evalBool x
	boolB <- evalBool y
	return (B (boolA && boolB))

eval (Or x y) = do
	boolA <- evalBool x
	boolB <- evalBool y
	return (B (boolA || boolB))

eval (Not x) = do
	boolA <- evalBool x
	return (B (not boolA))

-- ints and floats

eval (ValInt x)   = (return (I x))
eval (ValFloat x) = (return (F x))


eval (Plus x y) = do 
	a <- eval x
	b <- eval y
	case a of
		I a -> case b of
			I b -> return $ I (a + b)
			_   -> err "can't add a float and an int!"
		F a -> case b of
			F b -> return $ F (a + b)
			_ -> err "can't add a float and an int!"
		_ -> err "type error!"


eval (Minus x y) = do 
	a <- eval x
	b <- eval y
	case a of
		I a -> case b of
			I b -> return $ I (a - b)
			_   -> err "can't subtract a float and an int!"
		F a -> case b of
			F b -> return $ F (a - b)
			_ -> err "can't add a float and an int!"
		_ -> err "type error!"  

eval (Mult x y) = do 
	a <- eval x
	b <- eval y
	case a of
		I a -> case b of
			I b -> return $ I (a * b)
			_   -> err "can't multiply a float and an int!"
		F a -> case b of
			F b -> return $ F (a * b)
			_ -> err "can't multiply a float and an int!"
		_ -> err "type error!"

eval (FloatingPointDiv x y) = do 
	a <- eval x
	b <- eval y
	case a of
		I a -> err "only use FloatingPointDiv with floats!"
		F a -> case b of
			F 0 -> err "can't divide by 0!"
			F b -> return $ F (a / b)
			_ -> err "type error!"
		_ -> err "type error!"


eval (IntegerDiv x y) = do 
	a <- eval x
	b <- eval y
	case a of
		I a -> case b of
			I 0 -> err "can't divide by 0!"
			I b -> return $ I (a `div` b)
			_   -> err "type error!"
		F a -> err "only use IntegerDiv with integers!"
		_ -> err "type error!"

eval (Mod x y) = do 
	a <- eval x
	b <- eval y
	case a of
		I a -> case b of
			I 0 -> err "can't do mod 0!"
			I b -> return $ I (a `mod` b)
			_   -> err "can only use mod with Integers!"
		_  -> err "type error!"

eval (Exp x y) = do 
	a <- eval x
	b <- eval y
	case a of
		I a -> case b of
			I b -> return $ I (a ^ b)
			_   -> err "type error!"
		_   -> err "can only use exp with integers!"

eval (Fpe x y) = do 
	a <- eval x
	b <- eval y
	case a of
		F a -> case b of
			F b -> return $ F (a ** b)
			_   -> err "type error!"
		_  -> err "can only use fpe with floats!"



eval (Uminus x) = do
	a <- eval x
	case a of
		I 0 -> return (I 0)
		I a -> return (I (-a))
		F 0 -> return (F 0)
		F b -> return (F (-b))
		_   -> err "can only do uminus on ints and floats!"


eval (Nil) = (return (Ls ([])))

eval (Cons x y) = do
	head <- eval x
	tail <- evalList y
	return (Ls (head:tail))

eval (Index xs x) = do
	list  <- evalList xs
	index <- evalInt x 
	if (index > length list)
		then err "index is too big"
		else if (0 > index)
			then err "negative index"
			else return (list !! index)

eval (Concat xs ys) = do
	list1 <- evalList xs
	list2 <- evalList ys
	return (Ls (list1 ++ list2))

eval (If i t e) = do
	cond     <- evalBool i
	thenCase <- eval t
	elseCase <- eval e
	case cond of
		True -> return thenCase
		_    -> return elseCase

eval (Var name) = do
	a <- getEnv
	case (Map.lookup name a) of
		Just z  -> return z
		Nothing -> err "type error!"


{-
eval (InfixLam x bod) = do
	env <- getEnv
	return (Fun $ \v -> EnvUnsafeLog (eval bod) (Map.insert x v env))

eval (Lam x body) = do env <- getEnv
	return $ Fun $ \ v -> runEnvUnsafeLog (eval body) (Map.insert x v env)

eval (Let v x y) = do
	a <- (eval x)
	local (Map.insert v a)(eval y)

local :: (e -> e) -> EnvUnsafeLog e a -> EnvUnsafeLog e a
local changeEnv comp = EnvUnsafeLog (\env -> runEnvUnsafeLog comp (changeEnv env))

-}	

eval (Sep x y) = do
	a <- eval x
	b <- eval y
	return b


eval (App x y) = do
	f <- evalFunc x
	a <- eval y
	case (f a) of
		Error str -> err str
		Ok y      -> return y

eval (Eq x y) = do 
	a <- eval x
	b <- eval y
	case (a, b) of
		(I a, I b) -> return $ B (a == b)
		(F a, F b) -> return $ B (a == b)
		(I a, F b) -> err "you're comparing different types"
		(F a, I b) -> err "you're comparing different types"
		(C a, C b) -> return $ B (a == b)
		(B a, B b) -> return $ B (a == b)
		(_, _)     -> err "type error!"

eval (Neq x y) = do 
	a <- eval x
	b <- eval y
	case (a, b) of
		(I a, I b) -> return $ B (a /= b)
		(F a, F b) -> return $ B (a /= b)
		(I a, F b) -> err "you're comparing different types"
		(F a, I b) -> err "you're comparing different types"
		(C a, C b) -> return $ B (a /= b)
		(B a, B b) -> return $ B (a /= b)
		(_, _)     -> err "type error!"


eval (Lt x y) = do 
	a <- eval x
	b <- eval y
	case (a, b) of
		(I a, I b) -> return $ B (a < b)
		(F a, F b) -> return $ B (a < b)
		(I a, F b) -> err "you're comparing two different types"
		(F a, I b) -> err "you're comparing two different types"
		(_, _)     -> err "type error!"

eval (Lte x y) = do 
	a <- eval x
	b <- eval y
	case (a, b) of
		(I a, I b) -> return $ B (a <= b)
		(F a, F b) -> return $ B (a <= b)
		(_, _)     -> err "you're not comparing int/float"

eval (Gt x y) = do 
	a <- eval x
	b <- eval y
	case (a, b) of
		(I a, I b) -> return $ B (a > b)
		(F a, F b) -> return $ B (a > b)
		(I a, F b) -> err "you're comparing two different types"
		(F a, I b) -> err "you're comparing two different types"
		(_, _)     -> err "you're not comparing int/float"

eval (Gte x y) = do 
	a <- eval x
	b <- eval y
	case (a, b) of
		(I a, I b) -> return $ B (a >= b)
		(F a, F b) -> return $ B (a >= b)
		(I a, F b) -> err "you're comparing two different types"
		(F a, I b) -> err "you're comparing two different types"
		(_, _)     -> err "you're not comparing int/float"
{-
eval (Print ast) = do
    v <- eval ast
	EnvUnsafeLog $ \env -> (Ok v, show v)
	-}

-- Mix-In: Ordering a list using Quicksort

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater) where
	lesser  = filter (< p) xs
	greater = filter (>= p) xs
