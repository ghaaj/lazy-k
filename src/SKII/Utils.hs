{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SKII.Utils (
    CST,
    CSTNode (..),
    SGRsPair,
    applySGR,
    cstToText,
    highlightCombinator,
    isCombinator,
    noop,
) where

import Control.Arrow ((&&&), (***))
import Control.Monad (join, liftM2)
import Data.Bool (bool)
import Data.Function ((&))
import Data.Text (Text, pack)
import System.Console.ANSI (
    SGR (Reset),
    setSGRCode,
 )

data CSTNode = Combinator Char | Group CST
    deriving (Show, Eq, Ord)

type CST = [CSTNode]

isCombinator :: Char -> Bool
isCombinator c = any (elem c) [['a' .. 'z'], ['A' .. 'Z'], ['α' .. 'ω'], ['Α' .. 'Ω']]

isSupportedCombinator :: Char -> Bool
isSupportedCombinator = flip elem ("SKIBCWYι" :: [Char])

applySGR :: [SGR] -> String -> String
applySGR sgrs content = setSGRCode sgrs <> content <> setSGRCode [Reset]

type SGRsPair =
    ( [SGR] -- SGRs for free variables
    , [SGR] -- SGRs for supported combinators
    )

highlightCombinator :: SGRsPair -> Char -> String
highlightCombinator = (isSupportedCombinator &) . liftM2 (uncurry bool) . uncurry (&&&) . join (***) ((. return) . applySGR)

cstToText :: SGRsPair -> CST -> Text
cstToText hgls = flip foldr "" \case
    Combinator c -> (<>) . pack $ highlightCombinator hgls c
    Group groupCst -> (<>) $ "(" <> cstToText hgls groupCst <> ")"

noop :: (Monad m) => m ()
noop = pure ()
