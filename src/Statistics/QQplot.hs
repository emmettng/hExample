module Statistics.QQplot where 

import qualified Data.List as DL
import qualified Statistics.Distribution.Normal as SDnorm
import qualified Statistics.Distribution as SD 
{-
Python scipy code:

import numpy as np
from scipy.stats import beta
from scipy import stats

a = range(1,5)
b = 4-a+1
>>> pmean = beta.mean(a,b)
array([0.2, 0.4, 0.6, 0.8])
>>> pmedia = beta.median(a,b)
array([0.15910358, 0.38572757, 0.61427243, 0.84089642])
>>> dist = stats.norm

>>> pmeanppf = dist.ppf(pmean)
>>> pmeanppf
array([-0.84162123, -0.2533471 ,  0.2533471 ,  0.84162123])

>>> pmedia = beta.median(a,b)
>>> pmediappf = dist.ppf(pmedia)
>>> pmediappf
array([-0.99814888, -0.29047204,  0.29047204,  0.99814888])
-}

{-
Haskell code:
import Playground.Stats

*Main Playground.Stats> let hmean = betaMean 4
*Main Playground.Stats> hmean
[0.2,0.4,0.6,0.8]

*Main Playground.Stats> let hmedian = betaMedian 4
*Main Playground.Stats> hmedian
[0.1591035847462855,0.3854524627720504,0.6145475372279496,0.8408964152537145]
Main Playground.Stats> zipWith (-) hmedian pmedian 
[4.74628550350964e-9,-2.751072279496003e-4,2.751072279496558e-4,-4.746285475754064e-9]

*Main Playground.Stats> hmeanppf = normPPF hmean
*Main Playground.Stats> hmeanppf
[-0.8416212335729141,-0.2533471031357998,0.2533471031357998,0.8416212335729144]

*Main Playground.Stats> hmedianppf = normPPF hmedian 
*Main Playground.Stats> hmedianppf 
[-0.9981488825015569,-0.2911914182975527,0.29119141829755285,0.9981488825015569]

*Main Playground.Stats> zipWith (-) hmedianppf pmedianppf 
[-2.5015568505537544e-9,-7.193782975526974e-4,7.19378297552864e-4,2.5015568505537544e-9]

*Main Playground.Stats> zipWith (-) hmeanppf pmeanppf 
[-3.572914075711253e-9,-3.135799786502247e-9,3.135799786502247e-9,3.5729144087781606e-9]
-}
-- | About beta mean & beta median
-- | scipy.stats.morestats
-- | https://github.com/scipy/scipy/blob/abdab61d65dda1591f9d742230f0d1459fd7c0fa/scipy/stats/morestats.py#L523
-- | line: 393
-- | betaMean definition is the same as 
-- | https://github.com/statsmodels/statsmodels/blob/66fc298c51dc323ce8ab8564b07b1b3797108dad/statsmodels/graphics/gofplots.py#L58
-- | line: 639, function: plotting_pos
-- | betaMedain approximation definition is 
-- | https://github.com/scipy/scipy/blob/abdab61d65dda1591f9d742230f0d1459fd7c0fa/scipy/stats/morestats.py#L523
-- | line 361, function: _calc_uniform_order_statistic_medians
-- | This approximation also being introduced in doc scipy.stats.probplot:
-- | https://docs.scipy.org/doc/scipy-0.14.0/reference/generated/scipy.stats.probplot.html

-- | statistic has function to compute mean for beta distribution but no median !!!
betaMean :: (Enum a, Fractional a) => a -> [a]
betaMean nobs = (\x-> x / (nobs+1)) <$> [1.0..nobs]

-- | Filliben’s estimate
betaMedian :: (Enum a, Floating a) => a -> [a]
betaMedian nobs = headElem : body ++ [lastElem]
                    where 
                        headElem = 1 - 0.5**(1/nobs)
                        lastElem = 0.5**(1/nobs) 
                        body = (\x-> (x-0.3175)/(nobs+0.365)) <$> [2.0..nobs-1] 

-- | http://hackage.haskell.org/package/statistics-0.15.1.1/docs/src/Statistics.Distribution.Normal.html
-- | inverse CDF function of normal distribution
normPPF quantile = 
    let norm = SDnorm.normalDistr 0 1
    in realToFrac . SD.quantile norm <$> quantile 


sort :: Ord a => [a] -> [a]
sort = DL.sort  