{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module DomainModeling.Project where

import           Data.Text                      ( Text )
import           Data.Decimal                   ( Decimal )

newtype Money = Money
    {
        unMoney:: Decimal 
    } deriving(Show, Eq, Num, Fractional)

newtype ProjectId = ProjectId
    {
        unProjectId :: Int
    } deriving (Show, Eq, Num)

data Project g a =
            Project Text a
          | ProjectGroup Text g [Project g a]
            deriving (Show, Eq,Functor, Foldable, Traversable)


data Budget = Budget
    {
        budgetIncome        :: Money
      , budgetExpenditure   :: Money
    }deriving (Show, Eq)


data Transaction =
            Sale ! Money
          | Purchase Money
          deriving( Show, Eq)


