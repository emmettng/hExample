module Wikibook.Monoid where 

import qualified Data.Vector as DV
    
-- | Example One
-- link : https://github.com/aymannadeem/foldilocks
-- Property test: TODO

{- 
- max :: (Ord a) => [a] -> a
- reverse' :: [a] -> [a]
- elem' :: (Eq a) => a -> [a] -> Bool
- product' :: (Num a) => [a] -> a
- filter' :: (a -> Bool) -> [a] -> [a]
- head' :: [a] -> a
- last' :: [a] -> a
- map :: (a -> b) -> [a] -> [b] using foldr
- map :: (a -> b) -> [a] -> [b] using foldl
- Foldable instance for a binary tree using foldr
- Foldable instance for a binary tree using foldMap
-}

testList1 = [1..10]
testList2 = [11..20]
testListZ = do 
    t1 <- testList1
    t2 <- testList2
    return (t1+t2)



max :: (Ord a) => [a] -> a 
max = foldr1 compare  
    where 
        compare :: (Ord a) => a -> a -> a 
        compare l r 
            | l > r = l 
            | l <= r = r

product' :: (Num a) => [a] -> a 
product' = foldr1 pro 
    where 
        pro :: Num a1 => a1 -> a1 -> a1 
        pro x y = x * y 

-- | foldl reconL ==> reverse [10..10000]      
-- (0.14 secs, 39,892,864 bytes)
-- however  
-- foldr reconR ==> reverse [10..10000]
-- (1.09 secs, 4,323,602,304 bytes)
-- TODO: Explaination
reverseL :: [a] -> [a]
reverseL = foldl reconL [] 
    where  
        reconL :: [a] -> a -> [a] 
        reconL [] x = x : []
        reconL ys x = x : ys

reverseR :: [a] -> [a]
reverseR = foldr reconR [] 
    where  
        reconR :: a -> [a] -> [a]
        reconR x [] = [] ++ [x] 
        reconR x ys = ys ++ [x]

