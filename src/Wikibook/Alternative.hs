module Wikibook.Alternative where 

import Control.Applicative

digit :: Int -> String -> Maybe Int 
digit _ [] = Nothing 
digit i (c:_) 
    | i > 9  || i < 0 = Nothing 
    | otherwise = if [c] == show i then Just i else Nothing 

binChar :: String -> Maybe Int 
binChar s = digit 1 s <|> digit 0 s 