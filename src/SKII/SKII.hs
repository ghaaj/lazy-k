{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SKII.SKII (main) where

import Control.Arrow (second)
import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.Text (pack, unpack)
import SKII.Parse (CST, RunParserError, parseInput)
import SKII.Reduce (reduce, rmExtraGroupings)
import SKII.Utils (SGRsPair, cstToText)
import System.Console.ANSI (
    Color (..),
    ColorIntensity (..),
    ConsoleLayer (..),
    SGR (Reset, SetColor),
 )
import System.Console.Haskeline (
    InputT,
    defaultSettings,
    getInputLine,
    handleInterrupt,
    outputStrLn,
    runInputT,
    withInterrupt,
 )
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings promptLoopWithInterrupt
promptLoopWithInterrupt :: InputT IO ()
promptLoopWithInterrupt = withInterrupt (handleInterrupt (outputStrLn "SIGINT" >> promptLoopWithInterrupt) promptLoop)
promptLoop :: InputT IO ()
promptLoop =
    getInputLine ">>> " >>= \case
        Nothing -> return ()
        Just "" -> promptLoop
        Just input -> handleInput input >> promptLoop

handleInput :: String -> InputT IO ()
handleInput = either processErr processCST . parseInput . pack

processErr :: RunParserError -> InputT IO ()
processErr = outputStrLn . errorBundlePretty

processCST :: CST -> InputT IO ()
processCST = unless . null <*> liftIO . reductionLoop 0 [] . rmExtraGroupings

syntaxHgls :: SGRsPair
syntaxHgls =
    ( [SetColor Foreground Dull Black]
    , [Reset]
    )

reductionLoop :: Int -> [CST] -> CST -> IO ()
reductionLoop steps redexStack input = do
    threadDelay 100000
    let (nextSteps, reducedInput) = second rmExtraGroupings $ reduce steps input
    let isReduced = input /= reducedInput
    when (isReduced || null redexStack) . putStrLn . unpack $ "  = " <> cstToText syntaxHgls reducedInput
    let nextRedexStack = input : redexStack
    let exitCondition = reducedInput `elem` nextRedexStack
    if exitCondition
        then
            if isReduced
                then putStrLn "Endless loop"
                else when exitCondition . putStrLn $ show nextSteps ++ bool " steps" " step" (nextSteps == 1)
        else reductionLoop nextSteps nextRedexStack reducedInput
