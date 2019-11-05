{-# LANGUAGE TupleSections #-}

module List.Yahtzee where

-- We don’t need to know what the program does to apply this refactoring.
--
--
-- | 
-- This is the example from https://blog.plover.com/prog/haskell/type-markers.html
-- Refactor from : http://h2.jaguarpaw.co.uk/posts/good-design-and-type-safety-in-yahtzee/
-- 		and http://h2.jaguarpaw.co.uk/posts/using-brain-less-refactoring-yahtzee/
--
--
-- |
-- |
-- 1. Original implementation 
-- type DiceChoice = [Bool]
-- 
-- type DiceVals = [Integer]
-- 
-- type DiceState = (DiceVals, Integer)
-- 
-- allRolls :: DiceChoice -> DiceState -> [DiceState]
-- allRolls [] ([], n) = [([], n - 1)]
-- allRolls [] _ = undefined
-- allRolls (chosen:choices) (v:vs, n) =
--   allRolls choices (vs, n - 1) >>=
--   \(roll, _) ->
--      [ (d : roll, n - 1)
--      | d <- rollList ]
--   where
--     rollList =
--       if chosen
--         then [v]
--         else [1 .. 6]
-- 
-- exampleOrigin =
--   let diceChoice = [False, True, True, False, False]
--       dicevals = [5, 4, 3, 2, 1]
--   in mapM_ print $ allRolls diceChoice (dicevals, 2)
-- |
-- |
-- 2. avoid catch all pattern 
-- 'undefined' is not a good implementation. And not all pattern was exhausted in previous example.
-- type DiceChoice = [Bool]
-- 
-- type DiceVals = [Integer]
-- 
-- type DiceState = (DiceVals, Integer)
-- 
-- allRolls :: DiceChoice -> DiceState -> [DiceState]
-- allRolls [] ([], n) = [([], n - 1)]
-- allRolls [] (_:_, _) =
--   error " Invariant violated: choices must be same length as vals"
-- allRolls (_:_) ([], _) =
--   error " Invariant violated: choices must be same length as vals"
-- allRolls (chosen:choices) (v:vs, n) =
--   allRolls choices (vs, n - 1) >>=
--   \(roll, _) ->
--      [ (d : roll, n - 1)
--      | d <- rollList ]
--   where
--     rollList =
--       if chosen
--         then [v]
--         else [1 .. 6]
-- 
-- exampleOrigin =
--   let diceChoice = [False, True, True, False, False]
--       dicevals = [5, 4, 3, 2, 1]
--   in mapM_ print $ allRolls diceChoice (dicevals, 2)
-- |
-- | 
-- 3. pop function 
-- The invariant constriant here is that the length  of DiceChoice must be the same as the length of DiceVals. 
-- Therefore, the function that check invariant should be sperated with the function do the pure computation.
--
-- type DiceChoice = [Bool]
-- 
-- type DiceVals = [Integer]
-- 
-- type DiceState = (DiceVals, Integer)
-- 
-- pop :: DiceChoice -> DiceVals -> Maybe ((Bool, Integer), (DiceChoice, DiceVals))
-- pop [] [] = Nothing
-- pop (chosen:choices) (v:vs) = Just ((chosen, v), (choices, vs))
-- pop (_:_) [] = error " Invariant violated: choices must be same length as vals"
-- pop [] (_:_) = error " Invariant violated: choices must be same length as vals"
-- 
-- allRolls :: DiceChoice -> DiceState -> [DiceState]
-- allRolls choices (vs, n) =
--   case pop choices vs of
--     Nothing -> [([], n - 1)]
--     Just ((chosen, v), (choices, vs)) ->
--       allRolls choices (vs, n - 1) >>=
--       \(roll, _) ->
--          [ (d : roll, n - 1)
--          | d <- rollList ]
--       where rollList =
--               if chosen
--                 then [v]
--                 else [1 .. 6]
-- 
-- exampleOrigin =
--   let diceChoice = [False, True, True, False, False]
--       dicevals = [5, 4, 3, 2, 1]
--   in mapM_ print $ allRolls diceChoice (dicevals, 2)
-- |
-- |
-- 4. introduce tuple
-- Business logic:  each Diceval much be coupled with a DiceChoice. So 
-- type DiceChoice = [Bool]
-- 
-- type DiceVals = [Integer]
-- 
-- -- type DiceState = (DiceVals, Integer)
-- pop :: (DiceChoice, DiceVals) -> Maybe ((Bool, Integer), (DiceChoice, DiceVals))
-- pop (chosen:choices, v:vs) = Just ((chosen, v), (choices, vs))
-- pop ([], []) = Nothing
-- pop ((_:_), []) =
--   error " Invariant violated: choices must be same length as vals"
-- pop ([], (_:_)) =
--   error " Invariant violated: choices must be same length as vals"
-- 
-- allRolls :: (DiceChoice, DiceVals) -> Int -> [(DiceVals, Int)]
-- allRolls (choices, vs) n =
--   case pop (choices, vs) of
--     Nothing -> [([], n - 1)]
--     Just ((chosen, v), t ->
--       allRolls t (n - 1) >>=
--       \(roll, _) ->
--          [ (d : roll, n - 1)
--          | d <- rollList ]
--       where rollList =
--               if chosen
--                 then [v]
--                 else [1 .. 6]
-- exampleOrigin =
--   let diceChoice = [False, True, True, False, False]
--       dicevals = [5, 4, 3, 2, 1]
--   in mapM_ print $ allRolls (diceChoice, dicevals) 2
-- |
-- |
-- |
-- 5. Up till now 
--    1. we seperate validity check from the pure logic
--    2. don't need to unpack tuple in 'Just' pattern match 
-- In this version
-- 	1. remove irrelevant Int from recursive list operation
-- 	2. introduce new data type to make illegal state unrepresentable
-- type DiceVals = [Integer]
-- 
-- type DiceTurn = [(Bool, Integer)]
-- 
-- -- type DiceState = (DiceVals, Integer)
-- pop :: DiceTurn -> Maybe ((Bool, Integer), DiceTurn)
-- pop (a:as) = Just (a, as)
-- pop [] = Nothing
-- 
-- allRollsNoN :: DiceTurn -> [DiceVals]
-- allRollsNoN t =
--   case pop t of
--     Nothing -> [[]]
--     Just ((chosen, v), t) ->
--       allRollsNoN t >>=
--       \roll ->
--          [ d : roll
--          | d <- rollList ]
--       where rollList =
--               if chosen
--                 then [v]
--                 else [1 .. 6]
-- 
-- allRolls :: DiceTurn -> Int -> [(DiceVals, Int)]
-- allRolls t n =
--   [ (d, n - 1)
--   | d <- allRollsNoN t ]
-- 
-- exampleOrigin =
--   let diceChoice = [False, True, True, False, False]
--       dicevals = [5, 4, 3, 2, 1]
--   in mapM_ print $ allRolls (zip diceChoice dicevals) 2
-- |
-- |
-- 6.  
-- 	1. pop can be replaced by pattern match 
-- 	2. put >>= in do notation for better understanding 
-- type DiceVals = [Integer]
-- 
-- type DiceTurn = [(Bool, Integer)]
-- type DiceState = (DiceVals, Integer)
-- pop :: DiceTurn -> Maybe ((Bool, Integer), DiceTurn)
-- pop (a:as) = Just (a, as)
-- pop [] = Nothing
-- allRollsNoN :: DiceTurn -> [DiceVals]
-- allRollsNoN t =
--   case t of
--     [] -> [[]]
--     ((chosen, v):t) -> do
--       dv <- allRollsNoN t
--       let rollList =
--             if chosen
--               then [v]
--               else [1 .. 6]
--       d <- rollList
--       [d : dv]
-- 
-- allRolls :: DiceTurn -> Int -> [(DiceVals, Int)]
-- allRolls t n =
--   [ (d, n - 1)
--   | d <- allRollsNoN t ]
-- 
-- exampleOrigin =
--   let diceChoice = [False, True, True, False, False]
--       dicevals = [5, 4, 3, 2, 1]
--   in mapM_ print $ allRolls (zip diceChoice dicevals) 2
-- |
-- |
-- |
-- 7.  Final part ! Most important part !!!
-- 	1. mapM :: Monad m => ( a -> m b) -> t a -> m (t b) 
-- 	2. Avoid boolean blindness : dice value and reroll choice acturally pair up. So we introduce the new DiceChoice type
--
-- 
-- type DiceTurn = [(Bool, Integer)]
--
type DiceVals = [Integer]

data DiceChoice
  = Keep Integer
  | Reroll

allRollsNoN :: [DiceChoice] -> [DiceVals]
allRollsNoN dc = mapM roll dc
  where
    roll :: DiceChoice -> [Integer]
    roll (Keep v) = [v]
    roll Reroll = [1 .. 6]

-- | relies on TupleSections
allRolls :: [DiceChoice] -> Int -> [(DiceVals, Int)]
allRolls t n = fmap (, n - 1) $ allRollsNoN t

--   [ (d, n - 1)
--   | d <- allRollsNoN t ]
exampleFinal =
  let diceChoice = [Reroll, Keep 4, Keep 3, Reroll, Reroll]
  in mapM_ print $ allRolls diceChoice 2
