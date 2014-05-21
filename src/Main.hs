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
  let hxt        = HXT.parse str
      xmllight   = XMLLight.parse str
      xmlconduit = XMLConduit.parse ltxt
  deepseq str $ deepseq txt $ deepseq ltxt $ deepseq hxt $ deepseq xmllight $ deepseq xmlconduit $
    defaultMain
      [ bgroup "parse"
        [ bench "hxt"                (nf HXT.parse        str)
        , bench "xml-light (String)" (nf XMLLight.parse   str)
        , bench "xml-light (Text)"   (nf XMLLight.parse   txt)
        , bench "xml-conduit (Text)" (nf XMLConduit.parse ltxt)
        ]
      , bgroup "collect bold text"
        [ bench "hxt"         (nf HXT.collect        hxt)
        , bench "xml-light"   (nf XMLLight.collect   xmllight)
        , bench "xml-conduit" (nf XMLConduit.collect xmlconduit)
        ]
      , bgroup "print"
        [ bench "hxt"                (nf HXT.print        hxt)
        , bench "xml-light"          (nf XMLLight.print   xmllight)
        , bench "xml-conduit (Text)" (nf XMLConduit.print xmlconduit)
        ]
      ]
