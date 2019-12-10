{-# LANGUAGE RankNTypes #-}

module Monadic.ReaderState.ReaderForHuman where

-- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
-- Summary
-- Function 'ask' introduce the 'env' informatoin into the Reader Monad
-- 'local' (withReaderT) alter the 'env' information temporarily.
-- 'asks' convert a function from 'env' to other type to a Reader Monad.
-- Usually, one 'ask' will related to one Reader Monad that represent a function depends on 'env' information.
-- Because there could be several function depends on same 'env'. They can be composed by >>= or >=>.
-- ask
-- ask :: (Monad m) => ReaderT r m r
-- ask = ReaderT return
-- return :: r -> m r
-- local
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
-- asks
-- asks :: (Monad m)
--     => (r -> a)         -- ^ The selector function to apply to the environment.
--     -> ReaderT r m a
-- asks f = ReaderT (return . f)
-- {-# INLINE asks #-}
-- | Common usage
-- 1. use 'ask' to introduce the 'env' into computation. (almost compulsory, asks is rarely being used)
-- 2. so we could construct functions of type `a -> Reader r b` or `Reader r b`. (compulsory)
-- 3. 'local' or 'withReaderT' alter enviroment (optional)runreaderT ==> bring out , follow by an Env. (optional)
-- 4. 'runReader' or 'runreaderT' to unwrapp functions. (compulsory)
-- 5. Feed the 'env' information (compulsory)
-- | Intuition:
-- Pass Env/Context/Configuration information through a chain of operations that share the same information.
-- Imports before Example One
import Control.Monad.Trans.Reader
import GHC.Float

-- Imports before Example Two
import Control.Monad.IO.Class
import Control.Monad -- kleisli arrow ( >=> )

-- Import before Staging Example 
import GHC.Float
import Data.Store

-- The context informaiton is Env
-- syntax: give me a Env gave you an output
-- | Example one ------------->>----------------->>------------->>
-- Core operations chain
-- Intuitive Explaination:
--          CoreInput -> Reader Env CoreOutput
--          Pass Env down through the core operation chain.
-- core operation 1 :: Int -> Int
a1 :: Int -> Reader Int Int
a1 n = do
  env <- ask -- get Enviroment information
  return $ n + env

-- core operation 2 :: Into -> Float
a2 :: Int -> Reader Int Float
a2 n = do
  env <- ask -- get Enviroment information
  let fn = fromIntegral n
  return $ fromIntegral env / fn

-- core operation 3 :: Float -> Double
a3 :: Float -> Reader Int Double
a3 f = do
  env <- ask -- get Enviroment information
  let fn = fromIntegral env
  return $ float2Double $ fn * f

-- | A Chain of core operations.
-- Return a function from `enviroment information` to `result`
-- only when enviroment information being provided, the result
-- can be produced.
chainA :: Int -> Reader Int Double
chainA n = do
  t1 <- a1 n -- core operaiton 1
  t2 <- a2 t1 -- core operaiton 2
  a3 t2 -- core operation 3

chainA' :: Int -> Reader Int Double
chainA' = a1 >=> a2 >=> a3

--
-- ask :: (Monad m) => ReaderT r m r
-- ask = ReaderT return
--
-- return :: r -> m r
--  let p1 = chainA 10
--  runReader p1 $ 2
--  0.3333333432674408
--  env is 2 and n is 10 so
--  t1 = 10 + 2 , t2 = 2 / 12,  t3 = 2*2 / 12  =  0.33333333
-- | Example Two: ------------->>----------------->>------------->>
-- Monad transformer and envrioment information dependence only.
-- Operations depend only on Env information.
-- Intuitive Explaination:
--     Sequence opeartion depends only on Enviroment.
--
b1
  :: (MonadIO m)
  => ReaderT String m Int
b1 = do
  env <- ask -- get Enviroment information
  n <- liftIO getLine
  let n1 = read env
      n2 = read n
  return $ n1 + n2

-- core operation 2 :: Into -> Float
b2
  :: (MonadIO m)
  => ReaderT String m Float
b2 = do
  env <- ask -- get Enviroment information
  n <- liftIO getLine
  let n1 = read env
      n2 = read n
  return $ n1 / n2

-- core operation 3 :: Float -> Double
b3
  :: (MonadIO m)
  => ReaderT String m Double
b3 = do
  env <- ask -- get Enviroment information
  n <- liftIO getLine
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
-- env information is 10 but of the input type String
-- | Example Three ------------->>----------------->>------------->>
-- local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
---
-- withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
--
-- Intuitive Explaination:
--      local change env within same type
---     withReaderT change env to another type
-- *** The modification only effect temporarily. That is the reason why
-- the name is `local`
changeEnv :: String -> Float
changeEnv s = read s + 100

c1
  :: (Monad m)
  => ReaderT Float m Float
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
--
-- | Start the staging example 
data Env = Env
  { sf1Env :: String
  , sf2Env :: Int
  , sf3Env :: Float
  , sf4Env :: Double
  , sf5Env :: Int
  , serializer :: Serializer
  , deserializer :: Deserializer
  }

type StageCore a = ReaderT Env IO a

-- sf1
--   :: (Monad m)
--   => ReaderT Env m Int
sf1 :: StageCore Int
sf1 = do
  env <- ask
  return $ length . sf1Env $ env

sf2 :: Int -> StageCore Float
sf2 arg2 = do
  env <- ask
  return $ fromIntegral $ sf2Env env + arg2

sf3 :: Float -> StageCore Double
sf3 arg3 = do
  env <- ask
  return $ float2Double $ sf3Env env + arg3

sf4 :: Double -> StageCore String
sf4 arg4 = do
  env <- ask
  return $ show $ sf4Env env + arg4

sf5 :: StageCore Int
sf5 = do
  env <- ask
  return $ 100 + sf5Env env

coreChain :: () -> ReaderT Env IO Int
coreChain = (\_ -> sf1) >=> sf2 >=> sf3 >=> sf4 >=> (\_ -> sf5)

-- | Now we would like to:
--  1. Serialize the core output of each core functions above on the disk.
--  2. If relative env information changes, we performe the calculation.
--  3. If previous calculation is new, we performe this calculation.
--  4. otherwise, we deserialize what we saved on the disk.
-- | we need to name each stage 
type StageName = String

type Core a = (a, Bool)

-- | We use Data.Store to serialize output of each core function 
-- StageName and Env defines this boolen value 
checkSerialization :: StageName -> StageCore (Core FilePath)
checkSerialization = undefined

type Serializer = forall a. (Store a) =>
                            a -> FilePath -> IO ()

type Deserializer = forall a. (Store a) =>
                              FilePath -> IO a

-- | This would be make more sense, 
--  we take a corefunction ( a -> StageCore b) 
--  and a StageName 
--  embellish original corefunction output with a Bool information ( a-> StageCore (b,Bool) == (a -> StageCore (Core b))
--  this bool information is being used for helping following operation to decide whether to do serialization.
--
staging
  :: (Store b)
  => StageName -> (a -> StageCore b) -> Core a -> StageCore (Core b)
staging stageName coreFunc (coreInput, preStatus) = do
  env <- ask
  let savetoDisk = serializer env
      readDisk = deserializer env
  (serializationPath, done) <- checkSerialization stageName
  if done && preStatus
    then do
      ds <- liftIO $ readDisk serializationPath
      return (ds, True)
    else do
      coreOutput <- coreFunc coreInput
      liftIO $ savetoDisk coreOutput serializationPath
      return (coreOutput, False)

stageChain =
  (staging "sf1" (\_ -> sf1)) >=>
  (staging "sf2" sf2) >=>
  (staging "sf3" sf3) >=> (staging "sf4" sf4) >=> (staging "sf5" (\_ -> sf5))