module XmlBench.HXT where

import Prelude hiding ((.), id)

import Control.Category
import Text.XML.HXT.Core

collect :: String -> [String]
collect = runLA
  ( getText
  . deep (deep isText . hasName "b")
  . xread
  )

parse :: String -> XmlTrees
parse = runLA xread
