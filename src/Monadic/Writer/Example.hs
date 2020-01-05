module Monadic.Writer.Example where

import Control.Monad.Trans.Writer
import GHC.Float
import Data.Traversable

f1 :: Int -> Writer String Float
f1 i = do
  tell $ show i
  return $ fromIntegral i + 0.2

f2 :: Float -> Writer String Double
f2 f = do
  tell $ show f
  return $ float2Double f * 2


data LoggingType = LoggingType
  {
    partOne :: Int
  , partTwo :: Int
  }deriving (Show,Eq)

instance Semigroup LoggingType where
  (LoggingType o1 t1) <> (LoggingType o2 t2) = LoggingType (o1 + o2) (t1 + t2)

instance Monoid LoggingType where
  mempty = LoggingType 0 0

p1list = [1..10]
p2list = [2,4..40]

logList = uncurry LoggingType <$> zip p1list p2list

createLog :: (Int,Int) -> Writer LoggingType Int
createLog (e1,e2) = do
  tell $ LoggingType e1 e2
  pure $ e1 + e2
-- createLog (e1, e2) =
--   let w = LoggingType e1 e2
--       s = e1 + e2
--   in writer (s,w)

totalLog :: Writer LoggingType [Int]
totalLog = traverse createLog $ zip p1list p2list
