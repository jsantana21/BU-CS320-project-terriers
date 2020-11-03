module EnvUnsafeLog where

import Control.Monad(ap)

--This monad will form the plumbing for the evaluation function

data Unsafe a = Error String | Ok a deriving (Show, Eq)
data EnvUnsafeLog env a = EnvUnsafeLog (env -> (Unsafe a,[String]))

instance Functor (EnvUnsafeLog e) where
  -- fmap :: (a -> b) -> EnvUnsafeLog env a -> EnvUnsafeLog env b
  fmap f (EnvUnsafeLog g) = EnvUnsafeLog $ \e -> case g e of
                                                    (Error str,["List of strings to print out before error was made"]) -> (Error str,["List of strings to print out before error was made"])
                                                    (Ok x,["List of strings logged"]) -> (Ok (f x),["List of strings logged"])

                                           
                             
  -- make sure your implementation follows the functor laws

--ignore this for now
instance Applicative (EnvUnsafeLog e) where
  pure = return
  (<*>) = ap

instance Monad (EnvUnsafeLog e) where
  --return :: a -> EnvUnsafeLog a
  return a = EnvUnsafeLog (\_ -> (Ok a,["List of strings logged"]))

  --(>>=) :: EnvUnsafeLog a -> (a -> EnvUnsafeLog b) -> EnvUnsafeLog b
  (EnvUnsafeLog g) >>= f = EnvUnsafeLog $ \e -> case g e of
                                                   (Error str,["List of strings to print out before error was made"]) -> (Error str,["List of strings to print out before error was made"])
                                                   (Ok x,["List of strings logged"]) -> let (EnvUnsafeLog h) = f x

  -- make sure your implementation follows the Monad laws

-- technical note: this could be done more succinctly with monad transformers, but it is also good practice to do it by hand