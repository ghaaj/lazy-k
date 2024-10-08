{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module SKII.Reduce (reduce, rmExtraGroupings) where

import Control.Arrow (second)
import Data.Function ((&))
import SKII.Utils (CST, CSTNode (..))

reduce :: Int -> CST -> (Int, CST)
reduce steps = \case
    (Combinator 'S' : x : y : z : rest) -> (steps + 1, x : z : Group [y, z] : rest)
    (Combinator 'K' : x : y : rest) -> (steps + 1, x : rest)
    (Combinator 'I' : x : rest) -> (steps + 1, x : rest)
    (Combinator 'B' : x : y : z : rest) -> (steps + 1, x : Group [y, z] : rest)
    (Combinator 'C' : x : y : z : rest) -> (steps + 1, x : z : y : rest)
    (Combinator 'W' : x : y : rest) -> (steps + 1, x : y : y : rest)
    (Combinator 'Y' : x : rest) -> (steps + 1, x : Group [Combinator 'Y', x] : rest)
    (Combinator 'ι' : x : rest) -> (steps + 1, x : Combinator 'S' : Combinator 'K' : rest)
    cst -> groupReduce steps cst

groupReduce :: Int -> CST -> (Int, CST)
groupReduce gsteps = \case
    [] -> (gsteps, [])
    (Combinator c : rest) -> second (Combinator c :) $ groupReduce gsteps rest
    (Group groupCst : rest) ->
        case reduce 0 groupCst of
            (0, groupCst') -> second (Group groupCst' :) $ groupReduce gsteps rest
            (1, groupCst') -> (gsteps + 1, Group groupCst' : rest)

rmExtraGroupings :: CST -> CST
rmExtraGroupings (Group groupCst : rest) = rmExtraGroupings $ groupCst ++ rest
rmExtraGroupings cst =
    cst & map \case
        Group [Combinator c] -> Combinator c
        Group groupCst -> Group $ rmExtraGroupings groupCst
        cst' -> cst'
