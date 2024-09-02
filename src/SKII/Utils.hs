{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SKII.Utils (
    SGRsPair,
    applySGR,
    cstToText,
    highlightCombinator,
) where

import Control.Arrow ((***))
import Control.Monad (join, liftM2)
import Data.Bool (bool)
import Data.Text (Text, pack)
import SKII.Parse (CST, CSTNode (..), isSupportedCombinator)
import System.Console.ANSI (
    SGR (Reset),
    setSGRCode,
 )

applySGR :: [SGR] -> String -> String
applySGR sgrs content = setSGRCode sgrs <> content <> setSGRCode [Reset]

type SGRsPair =
    ( [SGR] -- SGRs for free variables
    , [SGR] -- SGRs for supported combinators
    )

highlightCombinator :: SGRsPair -> Char -> String
highlightCombinator = uncurry (((<*> isSupportedCombinator) .) . liftM2 bool) . join (***) ((. (: [])) . applySGR)

cstToText :: SGRsPair -> CST -> Text
cstToText hgls = flip foldr "" \case
    Combinator c -> (<>) . pack $ highlightCombinator hgls c
    Group groupCst -> (<>) $ "(" <> cstToText hgls groupCst <> ")"
