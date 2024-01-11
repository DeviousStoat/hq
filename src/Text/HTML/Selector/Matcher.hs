{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

module Text.HTML.Selector.Matcher
  ( after
  , attribute
  , before
  , match
  , remove
  , select
  , selectOne
  ) where

import           Data.Bifunctor          (bimap)
import           Data.Maybe              (listToMaybe, mapMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Text.HTML.Selector
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match
import           Text.StringLike

select :: (StringLike str) => Selector -> [Tag str] -> [Tag str]
select sel = filter (match sel)

selectOne :: (StringLike str) => Selector -> [Tag str] -> Maybe (Tag str)
selectOne sel = listToMaybe . select sel

attribute :: (StringLike str) => str -> [Tag str] -> [str]
attribute name = mapMaybe \case
  TagOpen _ attrs -> lookup name attrs
  _               -> Nothing

before :: (StringLike str) => Selector -> [Tag str] -> [Tag str]
before sel = takeWhile (not . match sel)

after :: (StringLike str) => Selector -> [Tag str] -> [Tag str]
after sel = dropWhile (not . match sel)

remove :: (StringLike str) => Selector -> [Tag str] -> [Tag str]
remove _ [] = []
remove sel (t : ts)
  | match sel t = remove sel $ go t ts
  | otherwise   = t : remove sel ts
  where
    go :: (StringLike str) => Tag str -> [Tag str] -> [Tag str]
    go curr@(TagOpen name _) (t' : ts')
      | t' == TagClose name = ts'
      | otherwise           = go curr ts'
    go _ _                   = error "cannot match on non-open tag"

match :: (StringLike str) => Selector -> Tag str -> Bool
match (Selector mTagMatcher attrMatchers) = tagOpen matchTag matchAttrs
  where
    matchTag :: (StringLike str) => str -> Bool
    matchTag t = maybe True ((== t) . castString) mTagMatcher

    matchAttrs :: (StringLike str) => [Attribute str] -> Bool
    matchAttrs attrs = all (matchAttr attrs) attrMatchers

    matchAttr :: (StringLike str) => [Attribute str] -> SelectorAttr -> Bool
    matchAttr attrs (SelectorAttr name Equals value)     =
      anyAttrLit (castString name, castString value) attrs
    matchAttr attrs (SelectorAttr name Contains value)   =
      anyAttrNameLitValue name (value `T.isInfixOf`) (textAttributes attrs)
    matchAttr attrs (SelectorAttr name StartsWith value) =
      anyAttrNameLitValue name (value `T.isPrefixOf`) (textAttributes attrs)
    matchAttr attrs (SelectorAttr name EndsWith value)   =
      anyAttrNameLitValue name (value `T.isSuffixOf`) (textAttributes attrs)
    matchAttr attrs (SelectorAttr name Includes value)   =
      anyAttrNameLitValue name ((value `elem`) . T.words) (textAttributes attrs)

anyAttrNameLitValue ::
  (StringLike str) => str -> (str -> Bool) -> [Attribute str] -> Bool
anyAttrNameLitValue name pValue =
  anyAttr (\(name', value) -> name == name' && pValue value)

textAttributes :: (StringLike str) => [Attribute str] -> [Attribute Text]
textAttributes = map $ bimap castString castString
