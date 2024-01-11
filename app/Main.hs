{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Data.Foldable              (foldl')
import           Data.Maybe                 (mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Options.Applicative
import           Text.HTML.Selector
import           Text.HTML.Selector.Matcher
import           Text.HTML.TagSoup
import           Text.Megaparsec            (errorBundlePretty)

data Options = Options
  { optSelector  :: [Text]
  , optAttribute :: [Text]
  , optBefore    :: Maybe Text
  , optAfter     :: Maybe Text
  , optTake      :: Maybe Take
  , optRemove    :: [Text]
  , optText      :: Bool
  , optNoText    :: Bool
  }
  deriving (Show)

data Take = Take Int | TakeRev Int
  deriving (Show)

options :: Parser Options
options = Options
  <$> many (argument str (metavar "SELECTOR" <> help "Selects nodes that match `SELECTOR`"))
  <*> many (
    strOption
      (long "attribute" <> short 'a' <> metavar "NAME" <> help "Takes the attributes `NAME`'s values for each node")
  )
  <*> optional (
    strOption
      (long "before" <> short 'b' <> metavar "SELECTOR" <> help "Selects nodes before `SELECTOR`")
  )
  <*> optional (
    strOption
      (long "after" <> short 'a' <> metavar "SELECTOR" <> help "Selects nodes after `SELECTOR`")
  )
  <*> optional (
    (Take <$> option auto
      (long "take" <> short 't' <> metavar "N" <> help "Takes `N` nodes"))
    <|> (TakeRev <$> option auto
      (long "take-rev" <> short 'r' <> metavar "N" <> help "Take `N` nodes from the end"))
  )
  <*> many (
    strOption
      (long "remove" <> short 'R' <> metavar "SELECTOR" <> help "Removes nodes that match `SELECTOR`")
  )
  <*> switch (long "text" <> short 'T' <> help "Prints only text nodes")
  <*> switch (long "no-text" <> short 'N' <> help "Prints only non-text nodes")

run :: Options -> Text -> Text
run opts@(Options {..}) =
  output opts
  . maybeApply doTake optTake
  . chainApply (fmap remove (parseSelectorWithFail <$> optRemove))
  . collectApply (fmap select (parseSelectorWithFail <$> optSelector))
  . maybeApply after (parseSelectorWithFail <$> optAfter)
  . maybeApply before (parseSelectorWithFail <$> optBefore)
  . parseTags

chainApply :: [a -> a] -> a -> a
chainApply = flip $ foldl' (flip ($))

collectApply :: Monoid a => [a -> a] -> a -> a
collectApply [] a = a
collectApply fs a = mconcat $ fmap ($ a) fs

output :: Options -> [Tag Text] -> Text
output (Options { optAttribute, optText, optNoText }) tags = case optAttribute of
  []    -> showTags optText tagsToPrint
  attrs -> T.unlines $ concatMap (`attribute` tags) attrs
  where
    tagsToPrint :: [Tag Text]
    tagsToPrint = if
      | optText   -> filter isTagText tags
      | optNoText -> filter (not . isTagText) tags
      | otherwise -> tags

showTags :: Bool -> [Tag Text] -> Text
showTags True = T.concat . mapMaybe maybeTagText
showTags False = T.unlines . fmap (renderTagsOptions renderOptions{optEscape = id} . pure)

maybeApply :: (a -> b -> b) -> Maybe a -> b -> b
maybeApply f ma b = maybe b (`f` b) ma

doTake :: Take -> [Tag Text] -> [Tag Text]
doTake (Take n) tags    = take n tags
doTake (TakeRev n) tags = drop (length tags - n) tags

parseSelectorWithFail :: Text -> Selector
parseSelectorWithFail t = case parseSelector t of
  Left err  -> error $ "Invalid selector: " ++ errorBundlePretty err
  Right sel -> sel

main :: IO ()
main = execParser opts >>= T.interact . run
  where
    opts = info (options <**> helper)
      ( fullDesc
      <> header "hq - Simple html nodes filtering tool"
      <> progDesc (unlines
          [ "Filters html nodes by selector."
          , " `SELECTOR` is a small subset of CSS selectors."
          , " It only supports the `<tag>[<attr_name><op><attr_value> ...]` syntax."
          , " `op` can be `=`, `^=`, `$=`, `*=`, `~=`."
          ]
        )
      )
