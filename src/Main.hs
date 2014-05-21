{-# LANGUAGE
    OverloadedStrings
  , StandaloneDeriving
  , DeriveGeneric
  #-}
module Main where

import Prelude hiding ((.), id)

import Control.Category
import Control.DeepSeq
import Control.DeepSeq.Generics
import Control.Monad
import Criterion.Main
import GHC.Generics
import Text.XML.HXT.Core (runLA, getText, deep, isText, hasName, xread, XmlTrees)
import Text.XML.Light
import Text.XML.Light.Lexer (XmlSource)
import Text.XML (parseText_, def, Document)
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
      [ bgroup "parse"
        [ bench "hxt"                (nf hxtParse str)
        , bench "xml (String)"       (nf xmlParse str)
        , bench "xml (Text)"         (nf xmlParse txt)
        , bench "xml-conduit (Text)" (nf xmlConduitParse ltxt)
        ]
      , bgroup "collect bold text"
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

hxtParse :: String -> XmlTrees
hxtParse = runLA xread

xmlCollect :: XmlSource a => a -> [String]
xmlCollect = map strContent
           . concatMap (findElements (QName "b" Nothing Nothing))
           . onlyElems
           . parseXML

xmlParse :: XmlSource a => a -> [Content]
xmlParse = parseXML

deriving instance Generic Content
deriving instance Generic CData
deriving instance Generic Element
deriving instance Generic CDataKind
deriving instance Generic Attr
deriving instance Generic QName

instance NFData Content   where rnf = genericRnf
instance NFData CData     where rnf = genericRnf
instance NFData Element   where rnf = genericRnf
instance NFData CDataKind where rnf = genericRnf
instance NFData Attr      where rnf = genericRnf
instance NFData QName     where rnf = genericRnf

xmlConduitCollect :: Lazy.Text -> [Strict.Text]
xmlConduitCollect =  content
                 <=< orSelf descendant
                 <=< checkName (== "b")
                 <=< orSelf descendant
                  .  fromDocument
                  .  parseText_ def

xmlConduitParse :: Lazy.Text -> Document
xmlConduitParse = parseText_ def
