module Wikibook.Monadic where 

import Control.Monad
import Control.Monad.State 
import Data.Time.Clock.POSIX
import Data.Int 

-- | Example Operation
nameReturn :: IO String 
nameReturn = do 
    putStr "What is your first name?"
    first <- getLine
    putStr "And your last name: "
    last <- getLine
    let full = first ++ " " ++ last 
    putStrLn $ "Please to meet you " ++ full ++" !"
    return full 

greetAndSeeYou :: IO ()
greetAndSeeYou = do 
    fullName <- nameReturn
    putStrLn $ "See your "++fullName ++ " !"

-- | Example Writer and State 


-- | monad Transfor : StateT
type ClockTime = Int64

posixToClockTime :: POSIXTime -> ClockTime 
posixToClockTime x = floor $ (*1000) . read . init . show $ x

curTime :: IO ClockTime 
curTime = posixToClockTime `fmap` getPOSIXTime


tickTock :: MonadIO m => StateT ClockTime m Bool 
tickTock = do 
    newtime <- liftIO curTime
    oldtime <- get 
    let tick = oldtime < newtime 
    when tick $ put newtime 
    return tick 

runTimer :: MonadIO m => StateT ClockTime m () 
runTimer = forever $ do 
    tick <- tickTock
    when tick $ liftIO . print . show =<< get 

doIt :: IO ()
doIt  = do 
    ct <- curTime 
    evalStateT runTimer ct