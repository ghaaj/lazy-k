{-# LANGUAGE OverloadedStrings #-}

module SKII.SKII (main) where

import Control.Arrow (second)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.Text (pack, unpack)
import SKII.Parse (RunParserError, parseInput)
import SKII.Reduce (reduce, rmExtraGroupings)
import SKII.Utils (CST, SGRsPair, cstToText, noop)
import System.Console.ANSI (
    Color (..),
    ColorIntensity (..),
    ConsoleIntensity (BoldIntensity),
    ConsoleLayer (..),
    SGR (Reset, SetColor, SetConsoleIntensity),
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
promptLoopWithInterrupt = withInterrupt $ handleInterrupt (outputStrLn "SIGINT" >> promptLoopWithInterrupt) promptLoop
promptLoop :: InputT IO ()
promptLoop = getInputLine ">>> " >>= maybe noop ((>> promptLoop) . handleInput)

handleInput :: String -> InputT IO ()
handleInput = either processErr processCST . parseInput . pack

processErr :: RunParserError -> InputT IO ()
processErr = outputStrLn . errorBundlePretty

processCST :: CST -> InputT IO ()
processCST = unless . null <*> liftIO . reductionLoop 0 [] . rmExtraGroupings

syntaxHgls :: SGRsPair
syntaxHgls =
    ( [Reset]
    , [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity]
    )

reductionLoop :: Int -> [CST] -> CST -> IO ()
reductionLoop steps redexStack input = do
    let nextRedexStack = input : redexStack
        (nextSteps, reducedInput) = second rmExtraGroupings $ reduce steps input
        isReduced = input /= reducedInput
    when (isReduced || null redexStack) . putStrLn . unpack $ "  = " <> cstToText syntaxHgls reducedInput
    if reducedInput `elem` nextRedexStack
        then putStrLn $ bool (show nextSteps ++ bool " steps" " step" (nextSteps == 1)) "Endless loop" isReduced
        else reductionLoop nextSteps nextRedexStack reducedInput
