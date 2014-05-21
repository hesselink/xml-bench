{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding ((.), id)

import Control.Category
import Control.DeepSeq
import Control.Monad
import Criterion.Main
import Text.XML.HXT.Core (runLA, getText, deep, isText, hasName, xread)
import Text.XML.Light
import Text.XML.Light.Lexer (XmlSource)
import Text.XML (parseText_, def)
import Text.XML.Cursor (fromDocument, descendant, orSelf, content, checkName)

import qualified Data.Text         as Strict
import qualified Data.Text.Lazy    as Lazy
import qualified Data.Text.IO      as Strict
import qualified Data.Text.Lazy.IO as Lazy

main :: IO ()
main = do
  str  <- readFile "data/collect-bold-text.xml"
  txt  <- Strict.readFile "data/collect-bold-text.xml"
  ltxt <- Lazy.readFile "data/collect-bold-text.xml"
  deepseq str $ deepseq txt $ deepseq ltxt
    defaultMain
      [ bgroup "collect bold text"
        [ bench "hxt"                (nf hxtCollect str)
        , bench "xml (String)"       (nf xmlCollect str)
        , bench "xml (Text)"         (nf xmlCollect txt)
        , bench "xml-conduit (Text)" (nf xmlConduitCollect ltxt)
        ]
      ]

hxtCollect :: String -> [String]
hxtCollect = runLA
  ( getText
  . deep (deep isText . hasName "b")
  . xread
  )

xmlCollect :: XmlSource a => a -> [String]
xmlCollect = map strContent
           . concatMap (findElements (QName "b" Nothing Nothing))
           . onlyElems
           . parseXML

xmlConduitCollect :: Lazy.Text -> [Strict.Text]
xmlConduitCollect =  content
                 <=< orSelf descendant
                 <=< checkName (== "b")
                 <=< orSelf descendant
                  .  fromDocument
                  .  parseText_ def
