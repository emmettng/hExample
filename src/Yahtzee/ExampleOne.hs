module Yahtzee.ExampleOne where

-- | 
-- This is the example from https://blog.plover.com/prog/haskell/type-markers.html
--
--
type DiceChoice = [Bool]

type DiceVals = [Integer]

type DiceState = (DiceVals, Integer)

allRolls :: DiceChoice -> DiceState -> [DiceState]
allRolls [] ([], n) = [([], n - 1)]
allRolls [] _ = undefined
allRolls (chosen:choices) (v:vs, n) =
  allRolls choices (vs, n - 1) >>=
  \(roll, _) ->
     [ (d : roll, n - 1)
     | d <- rollList ]
  where
    rollList =
      if chosen
        then [v]
        else [1 .. 6]
