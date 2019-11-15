module Monadic.ReaderState.ForHuman where


-- Example One imports
import           Control.Monad.Trans.Reader
import           GHC.Float

-- Example Two imports
import           Control.Monad.IO.Class


-- | Example one ------------->>----------------->>------------->>
-- Core operations chain  
-- Intuitive Explaination:
--          CoreInput -> Reader Env CoreOutput
--          Pass Env down through the core operation chain.

-- core operation 1 :: Int -> Int  
a1 :: Int -> Reader Int Int
a1 n = do
    env <- ask           -- get Enviroment information
    return $ n + env

-- core operation 2 :: Into -> Float
a2 :: Int -> Reader Int Float
a2 n = do
    env <- ask          -- get Enviroment information
    let fn = fromIntegral n
    return $ fromIntegral env / fn


-- core operation 3 :: Float -> Double
a3 :: Float -> Reader Int Double
a3 f = do
    env <- ask          -- get Enviroment information
    let fn = fromIntegral env
    return $ float2Double $ fn * f

-- | A Chain of core operations. 
-- Return a function from `enviroment information` to `result`
-- only when enviroment information being provided, the result 
-- can be produced.
chainA :: Int -> Reader Int Double
chainA n = do
    t1 <- a1 n          -- core operaiton 1
    t2 <- a2 t1         -- core operaiton 2
    a3 t2               -- core operation 3

-- 
-- ask :: (Monad m) => ReaderT r m r
-- ask = ReaderT return 
-- 
-- return :: r -> m r

-- | Example Two ------------->>----------------->>------------->>
-- 
-- Operations depend only on Env information.
-- Intuitive Explaination:
--     Sequence opeartion depends only on Enviroment. 
--      
b1 :: (MonadIO m) => ReaderT String m Int
b1 = do
    env <- ask           -- get Enviroment information
    n   <- liftIO getLine
    let n1 = read env
        n2 = read n
    return $ n1 + n2

-- core operation 2 :: Into -> Float
b2 :: (MonadIO m) => ReaderT String m Float
b2 = do
    env <- ask          -- get Enviroment information
    n   <- liftIO getLine
    let n1 = read env
        n2 = read n
    return $ n1 / n2


-- core operation 3 :: Float -> Double
b3 :: (MonadIO m) => ReaderT String m Double
b3 = do
    env <- ask          -- get Enviroment information
    n   <- liftIO getLine
    let n1 = read env
        n2 = read n
    return $ n1 * n2

-- | b1 b2 b3, use same Env of type String
--   Three paraller computations.
-- runReaderT tryB ==> function that take a Env and produce a result 
-- of chain
tryB :: ReaderT String IO Float
tryB = do
    t1 <- b1
    t2 <- b2
    t3 <- b3
    return $ fromIntegral t1 + t2 + double2Float t3

 -- runReaderT :: ReaderT  --> function :: Env -> Result
-- let p = runReaderT tryB
--  r <- p "10"
-- 1
-- 2
-- 3
--  r
-- 46.0
-- 1+10  + 10/2 + 3*10 = 46


-- | Example Three ------------->>----------------->>------------->>
-- 
-- local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
-- withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
-- Intuitive Explaination:
--      change the Env information  
-- 
changeEnv :: String -> Float
changeEnv s = read s + 100

c1 :: (Monad m) => ReaderT Float m Float 
c1 = do 
    env <- ask 
    return $ env * 5

tryC :: ReaderT String IO Float
tryC = do
    t1 <- b1
    t2 <- b2
    tc1 <- withReaderT changeEnv c1
    t3 <- b3
    return $ fromIntegral t1 + t2 + double2Float t3 + tc1

--  let p = runReaderT tryC
--  r <- p "10"   -- env is "10"
-- 1        b1 readin 1  
-- 2        b2 readin 2  ; computation c1 
-- 3        b3 readin 3 
--  r
-- 346.0
-- 1+10 + 10/2 + 10*3 + 110*5 = 596

-- |
-- local modify the value of Env 
-- withReaderT is more general, it could modify the type of Env.
-- The type of withReaderT guarantee the computation stays in 
-- ReaderT r' m a
-- local
--     :: (r -> r)         -- ^ The function to modify the environment.
--     -> ReaderT r m a    -- ^ Computation to run in the modified environment.
--     -> ReaderT r m a
-- local = withReaderT
-- 
-- withReaderT
--     :: (r' -> r)        -- ^ The function to modify the environment.
--     -> ReaderT r m a    -- ^ Computation to run in the modified environment.
--     -> ReaderT r' m a
-- withReaderT f m = ReaderT $ runReaderT m . f
-- 

-- | Example Four ------------->>----------------->>------------->>
-- asks :: (Monad m) => (r - a) -> ReaderT r m a 
-- Intuitive Explaination:
--         convert an simple function into ReaderT. 
--          
simpleFunc :: String -> Float 
simpleFunc s = 1000 + read s


tryD :: ReaderT String IO Float
tryD = do
    t1 <- b1
    t2 <- b2
    tc1 <- withReaderT changeEnv c1
    t3 <- b3
    td1 <- asks simpleFunc 
    return $ fromIntegral t1 + t2 + double2Float t3 + tc1 + td1

--  let p = runReaderT tryD 
--  r <- p "10"
-- 1
-- 2
-- 3
--  r
-- 1606.0
--1+10 + 10/2 +  10*3 + 110 * 5 + 1010 = 1606