{-# LANGUAGE OverloadedStrings #-}
module XmlBench.XMLConduit where

import Control.Monad
import Text.XML
import Text.XML.Cursor

import qualified Data.Text         as Strict
import qualified Data.Text.Lazy    as Lazy

collect :: Lazy.Text -> [Strict.Text]
collect =  content
       <=< orSelf descendant
       <=< checkName (== "b")
       <=< orSelf descendant
        .  fromDocument
        .  parseText_ def

parse :: Lazy.Text -> Document
parse = parseText_ def