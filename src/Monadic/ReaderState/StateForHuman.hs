module Monadic.ReaderState.StateForHuman where


import Control.Monad.Trans.State


-- functions 'get', 'put', 'return'
-- each produce a State monad in different purposes
-- being composed with '>>=' operator.
-- Usually, they collectively work as a single State Monad for certain function.

-- get
-- | Fetch the current value of the state within the monad.
-- get :: (Monad m) => StateT s m s
-- get = state $ \ s -> (s, s)

-- put
-- | @'put' s@ sets the state within the monad to @s@.
-- put :: (Monad m) => s -> StateT s m ()
-- put s = state $ \ _ -> ((), s)
--

-- return
-- instance (Monad m) => Monad (StateT s m) where
--     return a = StateT $ \ s -> return (a, s)
--     {-# INLINE return #-}

-- state
-- state :: (Monad m)
--       => (s -> (a, s))  -- ^pure state transformer
--       -> StateT s m a   -- ^equivalent state-passing computation
-- state f = StateT (return . f) -- this return is pair with m above.

-- >>=
--     m >>= k  = StateT $ \ s -> do
--         ~(a, s') <- runStateT m s
--         runStateT (k a) s'
--     {-# INLINE (>>=) #-}


-- | Common usage
-- 1. use 'get' to introduce state. (compulsory)
-- 3. some pure function :: s -> a will work on s . (optional)
-- 4. 'return' use to wrap the result of 3 into State Monad. (compulsory)
-- 5. 'put' will put update the state. (optional)
-- 6. 'evalState' / 'evalStateT'
--              or
--    'runState' / 'runStateT'
--  get the function wrapped inside slight differently. (compulsory)

-- runStateT
-- newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
--
-- evalStateT
-- evalStateT :: (Monad m) => StateT s m a -> s -> m a
-- evalStateT m s = do
--     ~(a, _) <- runStateT m s
--     return a

-- runState
-- | Unwrap a state monad computation as a function.
-- (The inverse of 'state'.)
-- runState :: State s a   -- ^state-passing computation to execute
--          -> s           -- ^initial state
--          -> (a, s)      -- ^return value and final state
-- runState m = runIdentity . runStateT m
--
-- evalState
-- evalState :: State s a  -- ^state-passing computation to execute
--           -> s          -- ^initial value
--           -> a          -- ^return value of the state computation
-- evalState m s = fst (runState m s)
-- {-# INLINE evalState #-}

-- |Example one

-- In general . State Monad is a monad represent a function relation .
-- just the output include an extra part of information that is of the same type of the input.
-- The input of >=> operation will always be the first one
-- the Initial state will also be the first one as well .

s1 :: Int -> State String Float
s1 i = do
  s <- get
  let
    is = show i ++ "_"++ s
    r = fromIntegral i / (fromIntegral . length) is
  put is
  return r

s2 :: Float -> State String String
s2 f = do
  s <- get
  let
    sf = show f ++ "_"++ s
  put sf
  return sf

s3 :: String -> State String Int
s3 str = do
  s <- get
  let
    ns = str ++ "_" ++ s
  put ns
  return $ length ns

