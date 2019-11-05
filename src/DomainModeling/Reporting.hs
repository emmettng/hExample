{-#LANGUAGE FlexibleContexts#-}
module DomainModeling.Reporting where

-- fold is necessary before the WriterT version 
-- import           Data.Foldable                  ( fold )
import           Data.Monoid                    ( getSum
                                                , getProduct
                                                )
import Control.Monad.IO.Class (liftIO)                                            
import Control.Monad.Writer (listen,runWriterT,tell)

import qualified DomainModeling.Database       as DB
import           DomainModeling.Project

data Report = Report
    {
        budgetProfit :: Money ,
        netProfit :: Money,
        difference :: Money
    } deriving (Show, Eq)

-- | The defnintio here is really .... 
-- | Dig more deep about Monoid .. Semigrop and fold ....
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html
instance Semigroup Report where
    (Report b1 n1 d1) <> (Report b2 n2 d2) =
        Report (b1 + b2) (n1 + n2) (d1 + d2)
instance Monoid Report where
    mempty = Report 0 0 0


calculateReport :: Budget -> [Transaction] -> Report
calculateReport budget ts = Report { budgetProfit = budgetProfit'
                                   , netProfit    = netProfit'
                                   , difference   = netProfit' - budgetProfit'
                                   }
  where
    budgetProfit' = budgetIncome budget - budgetExpenditure budget
    netProfit'    = getSum $ foldMap asProfit ts
    asProfit :: Applicative m1 => Transaction -> m1 Money
    asProfit (Sale     m) = pure m
    asProfit (Purchase m) = pure $ negate m

-- | Project a derive Functor, Foldable and Traversable
--   thus:
--   class (Functor t, Foldable t) => Traversable (t :: * -> *) where
--     traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   the following lambda function take type ProjectId as input type is necessary. 
-- calculateProjectReport :: Project g ProjectId -> IO (Project Report Report)
-- calculateProjectReport = traverse
    -- (\p -> calculateReport <$> DB.getBudget p <*> DB.getTransactionId p)\

-- | Traversable version
-- accumulateProjectReport :: Project Report -> Report
-- accumulateProjectReport = fold

--  Sweden
--  |
--  +- Stocklm: Budget: -3360.56, Net 5428.53, Difference +8789.09 
--  |
--  +- Gothenburg: Budget: 2392.68, Net 3275.87, Difference +883.19 
--  |
--  `- Malo
--     |
--     +- malo city: Budget: -1805.79, Net 3112.11, Difference +4917.90 
--     |
--     `- Limhamn: Budget: 185.25, Net 4683.35, Difference +4498.10 

-- need to accumulate individul project into overall project
-- 
-- putStrLn $ prettyReport $ accumulateProjectReport pr
-- Budget: -2588.41, Net 16499.87, Difference +19088.29


-- | WriterT version 
-- mapM = traverse 
-- traversa f = sequence . fmap f
-- mapM ::(Monad m, Traversable t) => ( a - m b) -> t a -> m t a
--      when t is a of type []
calculateProjectReport :: Project g ProjectId -> IO (Project Report Report)
calculateProjectReport project = fst <$> runWriterT (cal project)
    where 
        cal (Project name p) = do 
            report <- liftIO (calculateReport <$> DB.getBudget p <*> DB.getTransactionId p)
            tell report 
            pure $ Project name report 
        cal (ProjectGroup name _ projects)  = do 
            (projects', reports) <- listen(mapM cal projects)
            pure $ ProjectGroup name reports projects'

-- |donot need to a accumulate function any more
-- 
-- Sweden: Budget: -8155.96, Net -755.99, Difference +7399.97
-- |
-- +- Stocklm: Budget: -41.06, Net -2104.47, Difference -2063.41 
-- |
-- +- Gothenburg: Budget: -2927.44, Net +1619.98, Difference +4547.42 
-- |
-- `- Malo: Budget: -5187.46, Net -271.50, Difference +4915.96
--    |
--    +- malo city: Budget: -7557.90, Net -1290.71, Difference +6267.19 
--    |
--    `- Limhamn: Budget: +2370.44, Net +1019.21, Difference -1351.23 





-- | Previous version of calculateProjectReport
--   instance Monoid a => Monoid (IO a) -- Defined in ‘GHC.Base’
--   instance Semigroup a => Semigroup (IO a) -- Defined in ‘GHC.Base’
--   http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#line-440
--   so that foldMap can be used here
--   
-- calculateProjectReport :: Project -> IO Report
-- calculateProjectReport = calc
--   where
--     calc (Project p _) =
--       calculateReport <$> DB.getBudget p <*> DB.getTransactions p
--     calc (ProjectGroup _ projects) = foldMap calc projects

