module Topics.RankNTypes where

-- | GHC will suggest to use 'RankNTypes'
id :: forall a . a -> a
id x = x
