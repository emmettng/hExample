{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Packages.Configurator where

import Data.Configurator
import Data.Configurator.Types (Value(..), Configured(..))
import Data.Maybe (catMaybes)

import Data.Text as T

tryConfig :: IO ()
tryConfig = do
  c <- load [Required "data/configDemoOne.cfg"]
  lst <- require c "DemoOneInfo.floatField" :: IO Float
  print lst

lst :: String -> IO Value
lst filePath = do
  cfg <- load [Required filePath]
  require cfg "DemoOneInfo.numberField" :: IO Value

stringField :: IO String
stringField = do
  cfg <- load [Required "data/configDemoOne.cfg"]
  require cfg "DemoOneInfo.stringField" :: IO String

intField :: IO Int
intField = do
  cfg <- load [Required "data/configDemoOne.cfg"]
  require cfg "DemoOneInfo.intField" :: IO Int

floatField :: IO Float
floatField = do
  cfg <- load [Required "data/configDemoOne.cfg"]
  require cfg "DemoOneInfo.floatField" :: IO Float

homoIntListField :: IO [Int]
homoIntListField = do
  cfg <- load [Required "data/configDemoOne.cfg"]
  require cfg "DemoOneInfo.homoListNum" :: IO [Int]

homoStringListField :: IO [String]
homoStringListField = do
  cfg <- load [Required "data/configDemoOne.cfg"]
  require cfg "DemoOneInfo.homoListString" :: IO [String]

homoBoolListField :: IO [Bool]
homoBoolListField = do
  cfg <- load [Required "data/configDemoOne.cfg"]
  require cfg "DemoOneInfo.homoListBool" :: IO [Bool]

heteValueList :: IO [Value]
heteValueList = do
  cfg <- load [Required "data/configDemoOne.cfg"]
  require cfg "DemoOneInfo.heteList" :: IO [Value]

heteListField :: IO Value
heteListField = do
  cfg <- load [Required "data/configDemoOne.cfg"]
  require cfg "DemoOneInfo.heteList" :: IO Value

-- | Import other config files
--
nestDemo :: IO String
nestDemo = do
  cfg <- load [Required "data/configDemo2.cfg"]
  require cfg "Demo2Info.demo2_nest.nest-name" :: IO String

importDemo :: IO String
importDemo = do
  cfg <- load [Required "data/configDemoThree.cfg"]
  require cfg "DemoThreeInfo.Demo2Info.demo2_nest.nest-name" :: IO String

-- | customize data type
--
--
data DemoFour = DemoFour
  { sName :: String
  , iAge :: Int
  , bAdult :: Bool
  , fWeight :: Float
  } deriving (Show)

instance Configured DemoFour where
  convert :: Value -> Maybe DemoFour
  convert (List [sV, nV, bV, fV]) = do
    s <- convert sV :: Maybe String
    n <- convert nV :: Maybe Int
    b <- convert bV :: Maybe Bool
    f <- convert fV :: Maybe Float
    return $ DemoFour s n b f
  convert _ = Nothing

customizeDemo :: IO DemoFour
customizeDemo = do
  cfg <- load [Required "data/configDemoCustomize.cfg"]
  require cfg "DemoCustomizeInfo.custDemo" :: IO DemoFour
