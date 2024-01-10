module Main (main) where

import           Test.Hspec                 hiding (Selector)
import           Test.Hspec.Megaparsec
import           Text.HTML.Selector
import           Text.HTML.Selector.Matcher
import           Text.HTML.TagSoup

testParser :: SpecWith ()
testParser = describe "parseSelector" $ do
  it "parses a single tag" $
    parseSelector "div" `shouldParse` Selector (Just "div") []

  it "parses a single attribute without tag" $
    parseSelector "[class=foo]" `shouldParse`
      Selector Nothing [SelectorAttr "class" Equals "foo"]

  it "parses a tag with an attribute" $
    parseSelector "div[id=foo]" `shouldParse`
      Selector (Just "div") [SelectorAttr "id" Equals "foo"]

  it "parses a tag with multiple attributes" $
    parseSelector "div[id=foo class=bar]" `shouldParse`
      Selector
        (Just "div")
        [SelectorAttr "id" Equals "foo", SelectorAttr "class" Equals "bar"]

  it "parses a tag with start attribute check" $
    parseSelector "div[id^=foo]" `shouldParse`
      Selector (Just "div") [SelectorAttr "id" StartsWith "foo"]

  it "parses a tag with end attribute check" $
    parseSelector "div[id$=foo]" `shouldParse`
      Selector (Just "div") [SelectorAttr "id" EndsWith "foo"]

  it "parses a tag with substring attribute check" $
    parseSelector "div[id*=foo]" `shouldParse`
      Selector (Just "div") [SelectorAttr "id" Contains "foo"]

  it "parses a tag with word attribute check" $
    parseSelector "div[id~=foo]" `shouldParse`
      Selector (Just "div") [SelectorAttr "id" Includes "foo"]

  it "parses a tag with multiple attribute checks" $
    parseSelector "div[id^=foo class~=bar]" `shouldParse`
      Selector
        (Just "div")
        [SelectorAttr "id" StartsWith "foo", SelectorAttr "class" Includes "bar"]

tags :: [Tag String]
tags = parseTags "<div id=\"foo\" class=\"bar blap glop\" other=\"awesome\">"

testMatcher :: SpecWith ()
testMatcher = describe "select tags" $ do
  it "matches a tag" $
    select (Selector (Just "div") []) tags `shouldBe` tags

  it "matches a tag by attribute" $
    select (Selector Nothing [SelectorAttr "id" Equals "foo"]) tags `shouldBe` tags

  it "matches a tag by attribute start" $
    select (Selector Nothing [SelectorAttr "id" StartsWith "fo"]) tags `shouldBe` tags

  it "matches a tag by attribute end" $
    select (Selector Nothing [SelectorAttr "id" EndsWith "oo"]) tags `shouldBe` tags

  it "matches a tag by attribute contains" $
    select (Selector Nothing [SelectorAttr "other" Contains "wes"]) tags `shouldBe` tags

  it "matches a tag by attribute includes" $
    select (Selector Nothing [SelectorAttr "class" Includes "blap"]) tags `shouldBe` tags

  it "doesn't match when value is different" $
    select (Selector Nothing [SelectorAttr "id" Equals "bar"]) tags `shouldBe` []

  it "doesn't match when tag is wrong" $
    select (Selector (Just "a") [SelectorAttr "id" Equals "foo"]) tags `shouldBe` []

  it "doesn't match when all attributes don't match" $
    select
      (Selector
        (Just "a")
        [SelectorAttr "id" Equals "foo", SelectorAttr "other" Equals "bar"]
      ) tags `shouldBe` []

main :: IO ()
main = hspec $ do
  testParser
  testMatcher
