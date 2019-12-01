{-# LANGUAGE RankNTypes #-}
module Topics.RankNTypes where

-- Examples below from :
-- https://ocharles.org.uk/guest-posts/2014-12-18-rank-n-types.html
--
-- | Seeing 'forall' keyword, GHC will suggest to use 'RankNTypes'
-- > let try1 = Topics.RankNTypes.id 10
-- > :info try1
--    try1 :: Num a => a 	-- Defined at <interactive>:1:5
id :: forall a . a -> a
id x = x

type IdFunc = forall a . a -> a

-- | someInt is of Rank2Type, because
-- 1. it takes an rank1type polymorphism function as input
someInt :: IdFunc -> Integer
someInt fc = fc undefined

