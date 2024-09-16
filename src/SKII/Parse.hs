module SKII.Parse (
    RunParserError,
    parseInput,
) where

import Control.Monad (liftM2, when)
import Data.Char (isSpace)
import Data.Void (Void)

import Data.Text (Text)
import SKII.Utils (CST, CSTNode (..), isCombinator)
import Text.Megaparsec (
    MonadParsec (eof, try),
    ParseErrorBundle,
    Parsec,
    many,
    parse,
    satisfy,
    skipMany,
    (<|>),
 )
import Text.Megaparsec.Char (char)

type Parser = Parsec Void Text
type RunParserError = ParseErrorBundle Text Void

spaceConsumer :: Parser ()
spaceConsumer = skipMany $ satisfy isSpace

combinator :: Parser CSTNode
combinator = Combinator <$> satisfy isCombinator

group :: Parser CSTNode
group = (Group <$>) $ char '(' *> (expr >>= liftM2 (*>) requireNonEmpty pure) <* char ')'

requireNonEmpty :: CST -> Parser ()
requireNonEmpty = ($ fail "Empty bracket") . when . null

expr :: Parser CST
expr = spaceConsumer *> many ((try combinator <|> group) <* spaceConsumer)

parseInput :: Text -> Either RunParserError CST
parseInput = parse (expr <* eof) "SKII"
