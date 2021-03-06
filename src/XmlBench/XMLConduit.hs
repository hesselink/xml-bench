{-# LANGUAGE OverloadedStrings #-}
module XmlBench.XMLConduit where

import Control.Monad
import Text.XML
import Text.XML.Cursor

import qualified Data.Text         as Strict
import qualified Data.Text.Lazy    as Lazy

collect :: Document -> [Strict.Text]
collect =  content
       <=< orSelf descendant
       <=< checkName (== "b")
       <=< orSelf descendant
        .  fromDocument

parse :: Lazy.Text -> Document
parse = parseText_ def

print :: Document -> Lazy.Text
print = renderText def

update :: Document -> Document
update d = d { documentRoot = replaceBWithI (documentRoot d) }
  where
    replaceBWithI el | elementName el == "b" = el { elementName = "i" }
                     | otherwise             = el { elementNodes = map replaceOnNodes (elementNodes el) }
    replaceOnNodes (NodeElement el) = NodeElement (replaceBWithI el)
    replaceOnNodes n                = n
