{-# LANGUAGE
    OverloadedStrings
  , StandaloneDeriving
  , DeriveGeneric
  #-}
module Main where

import Control.DeepSeq
import Criterion.Main

import qualified Data.Text.IO      as Strict
import qualified Data.Text.Lazy.IO as Lazy

import qualified XmlBench.HXT        as HXT
import qualified XmlBench.XMLLight   as XMLLight
import qualified XmlBench.XMLConduit as XMLConduit

main :: IO ()
main = do
  str  <- readFile "data/collect-bold-text.xml"
  txt  <- Strict.readFile "data/collect-bold-text.xml"
  ltxt <- Lazy.readFile "data/collect-bold-text.xml"
  deepseq str $ deepseq txt $ deepseq ltxt
    defaultMain
      [ bgroup "parse"
        [ bench "hxt"                (nf HXT.parse        str)
        , bench "xml (String)"       (nf XMLLight.parse   str)
        , bench "xml (Text)"         (nf XMLLight.parse   txt)
        , bench "xml-conduit (Text)" (nf XMLConduit.parse ltxt)
        ]
      , bgroup "collect bold text"
        [ bench "hxt"                (nf HXT.collect        str)
        , bench "xml (String)"       (nf XMLLight.collect   str)
        , bench "xml (Text)"         (nf XMLLight.collect   txt)
        , bench "xml-conduit (Text)" (nf XMLConduit.collect ltxt)
        ]
      ]
