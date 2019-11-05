module Wikibook.Applicative where 

import Text.Read 

interactiveDoubling = do 
    putStrLn "Choose a Number"
    s <- getLine 
    let mx = readMaybe s :: Maybe Double
    case (*2) <$> mx of
        Just x -> putStrLn $ "The double of your number is: " ++ show x
        Nothing -> do 
                    putStrLn "This is not a valid number. Retrying .... "
                    interactiveDoubling

interactiveSumming = do 
    putStrLn "Choose two numbers: "
    mx <- readMaybe <$> getLine
    my <- readMaybe <$> getLine 
    case (+) <$> mx <*> my of 
        Just x -> putStrLn $ "The double of your number is: " ++ show x
        Nothing -> do 
                    putStrLn "This is not a valid number. Retrying .... "
                    interactiveSumming

interactiveConcatenating = do 
    putStrLn "Choose two strings!"
    sxx <- (++) <$> getLine <*> getLine
    putStrLn sxx                    