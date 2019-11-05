module DomainModeling.Database where 

import System.Random (getStdRandom, randomR)

import DomainModeling.Project 
import Data.Decimal (realFracToDecimal)

randomMoney :: (Double, Double) -> IO Money 
randomMoney range = Money . realFracToDecimal 2 <$> getStdRandom (randomR range)

getBudget :: ProjectId -> IO Budget
getBudget _ = do 
    income <- randomMoney (0,10000)
    expenditure <- randomMoney (0,10000)
    return Budget
        {
            budgetIncome = income,
            budgetExpenditure = expenditure
        }

getTransactionId :: ProjectId -> IO [Transaction]
getTransactionId _ = do 
    sale     <- Sale <$> randomMoney (0,4000)
    purchase <- Purchase <$> randomMoney (0,4000)
    return [sale, purchase]