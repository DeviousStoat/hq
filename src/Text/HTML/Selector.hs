module Text.HTML.Selector
  ( Selector(..)
  , SelectorAttr(..)
  , SelectorAttrOp(..)
  , parseSelector
  ) where

import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char (char, hspace1, string)

data Selector = Selector
  { selTag   :: Maybe Text
  , selAttrs :: [SelectorAttr]
  } deriving (Eq, Show)

data SelectorAttrOp = Equals | Contains | StartsWith | EndsWith | Includes
  deriving (Eq, Show)

data SelectorAttr = SelectorAttr
  { selAttr  :: Text
  , selOp    :: SelectorAttrOp
  , selValue :: Text
  } deriving (Eq, Show)

type Parser = Parsec Void Text

pTag :: Parser (Maybe Text)
pTag =  optional $ takeWhile1P (Just "tag") (/= '[')

pAttrs :: Parser [SelectorAttr]
pAttrs = between (char '[') (char ']') $ sepBy pAttr hspace1

pAttr :: Parser SelectorAttr
pAttr = do
  (attr, op) <- someTill_ anySingle pAttrOp
  SelectorAttr (T.pack attr) op <$> takeWhile1P (Just "value") (`notElem` [' ', ']'])

pAttrOp :: Parser SelectorAttrOp
pAttrOp = Includes <$ string "~="
      <|> StartsWith <$ string "^="
      <|> EndsWith <$ string "$="
      <|> Contains <$ string "*="
      <|> Equals <$ char '='

pSelector :: Parser Selector
pSelector = Selector <$> pTag <*> (concat <$> optional pAttrs)

parseSelector :: Text -> Either (ParseErrorBundle Text Void) Selector
parseSelector = parse pSelector ""
