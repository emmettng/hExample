module DomainModeling.PrettyPrint where

-- import qualified Data.Text                     as Text
import           Data.Tree
import           Text.Printf         
import           Data.Decimal                   ( roundTo )

import           DomainModeling.Project
import           DomainModeling.Reporting



asTree :: (g -> String) -> (a -> String) -> Project g a -> Tree String
asTree prettyGroup prettyValue project = case project of
    Project name x -> Node (printf "%s: %s " name (prettyValue x)) []
    ProjectGroup name x ps ->
        Node (printf "%s: %s" name (prettyGroup x)) (map (asTree prettyGroup prettyValue) ps)

prettyProject :: (g -> String) -> (a -> String) -> Project g a -> String
prettyProject prettyGroup prettyValue = drawTree . asTree prettyGroup prettyValue


prettyMoney :: Money -> String
prettyMoney (Money m) = sing ++ show (roundTo 2 m)
    where sing = if m > 0 then "+" else ""

prettyReport :: Report -> String
prettyReport r = printf "Budget: %s, Net %s, Difference %s"
                        (prettyMoney (budgetProfit r))
                        (prettyMoney (netProfit r))
                        (prettyMoney (difference r))
