{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Command where

import GHC.Generics (Generic)
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Text
import Text.Parsec (char, try, skipMany, many, oneOf, eof, between, string, noneOf, anyChar)
import Text.Parsec.Text (Parser)

data Command where
  Eval :: !Text -> Command
  Type :: !Text -> Command
    deriving (Eq, Generic, NFData, Ord, Read, Show)

parseCommand :: Parser Command
parseCommand = parseType
  where
    space = oneOf " \n"
    
    -- parseEval :: Paraser Command
    -- parseEval = skipMany space
    --   *> (Eval . pack <$> parseCode)
    --  <* skipMany space
    --  <* eof

    parseType :: Parser Command
    parseType = skipMany space
      *> (Type . pack <$> parseCode)


-- | Attempt to parse a code block surrounded in @```@. If that does not
-- work, attempt to parse inline code surrounded in @`@. Finally, if that
-- does not work, just treat the whole string as code.
parseCode :: Parser String
parseCode = try parseCodeBlock <|> try parseInlineCode <|> anyString where
  -- | Parses a code block surrounded in @```@.
  parseCodeBlock :: Parser String
  parseCodeBlock = between (string "```\n") (string "```") (many $ noneOf "`")

  -- | Parses inline code surrounded in @`@.
  parseInlineCode :: Parser String
  parseInlineCode = between (char '`') (char '`') (many $ noneOf "`")

  -- | Parse any string.
  anyString :: Parser String
  anyString = many anyChar
