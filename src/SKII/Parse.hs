module SKII.Parse (
    CST,
    CSTNode (..),
    RunParserError,
    isCombinator,
    isSupportedCombinator,
    parseInput,
) where

import Control.Monad (when)
import Data.Char (isSpace)
import Data.Void (Void)

import Data.Text (Text)
import Text.Megaparsec (
    MonadParsec (eof, lookAhead, try),
    ParseErrorBundle,
    Parsec,
    many,
    optional,
    parse,
    satisfy,
    skipMany,
    (<|>),
 )
import Text.Megaparsec.Char (char)

type Parser = Parsec Void Text
type RunParserError = ParseErrorBundle Text Void

data CSTNode = Combinator Char | Group CST
    deriving (Show, Eq, Ord)

type CST = [CSTNode]

isCombinator :: Char -> Bool
isCombinator c = any (elem c) [['a' .. 'z'], ['A' .. 'Z'], ['α' .. 'ω'], ['Α' .. 'Ω']]

isSupportedCombinator :: Char -> Bool
isSupportedCombinator = flip elem ("SKIBCWYι" :: [Char])

spaceConsumer :: Parser ()
spaceConsumer = skipMany $ satisfy isSpace

combinator :: Parser CSTNode
combinator = Combinator <$> satisfy isCombinator

group :: Parser CSTNode
group = do
    _ <- char '('
    groupEx <- expr
    closingBracket <- optional . lookAhead $ char ')'
    case closingBracket of
        Nothing -> return ()
        Just _ -> when (null groupEx) $ fail "Empty bracket"
    _ <- char ')'
    return $ Group groupEx

expr :: Parser CST
expr = (*>) spaceConsumer . many $ (try combinator <|> group) <* spaceConsumer

parseInput :: Text -> Either RunParserError CST
parseInput = parse (expr <* eof) "SKII"
