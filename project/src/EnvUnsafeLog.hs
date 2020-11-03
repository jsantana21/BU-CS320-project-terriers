module EnvUnsafeLog where

  import Control.Monad(ap)
  
  --This monad will form the plumbing for the evaluation function
  
  data Unsafe a = Error String | Ok a deriving (Show, Eq)
  data EnvUnsafeLog env a = EnvUnsafeLog (env -> (Unsafe a, env))
  
  runEnvUnsafeLog :: (EnvUnsafeLog e a) -> e -> (Unsafe a, e)
  runEnvUnsafeLog (EnvUnsafeLog eu) e = eu e
  
  app :: EnvUnsafeLog s a -> (s -> (Unsafe a,s))
  app (EnvUnsafeLog eu) = eu
  
  err :: String -> EnvUnsafeLog e a
  err s = EnvUnsafeLog $ \e -> (Error s, e)
  
  getEnv :: EnvUnsafeLog e e
  getEnv = EnvUnsafeLog $ \ e -> (Ok e, e)
  
  put :: s -> EnvUnsafeLog s ()
  put s = EnvUnsafeLog $ \ _ -> (Ok (), s)
  
  envunsafereturn :: Unsafe a -> EnvUnsafeLog s a
  envunsafereturn eu = EnvUnsafeLog $ \ s -> (eu,s)
  
  instance Functor (EnvUnsafeLog e) where
    fmap f (EnvUnsafeLog g) = EnvUnsafeLog $ \ s -> case g s of
                                                      ((Ok a), result) -> ((Ok $ f a), result)
                                                      ((Error e), result) -> ((Error e), result)

  
  -- make sure your implementation follows the functor laws
  
  --ignore this for now
  instance Applicative (EnvUnsafeLog e) where
    pure = return
    (<*>) = ap
  
  instance Monad (EnvUnsafeLog e) where
  
  
    -- return :: a -> EnvUnsafeLog a
    return a = EnvUnsafeLog $ \ s -> (Ok a,s)
  
    -- (>>=) :: EnvUnsafeLog a -> (a -> EnvUnsafeLog b) -> EnvUnsafeLog b
    (EnvUnsafeLog g) >>= f = EnvUnsafeLog $ \e -> case g e of
                                                   (Ok a, result) -> app (f a) result
                                                   (Error e, result) -> (Error e, result)
  
    -- make sure your implementation follows the Monad laws 
  
  -- technical note: this could be done more succinctly with monad transformers, but it is also good practice to do it by hand
  
