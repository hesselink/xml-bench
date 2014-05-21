module XmlBench.HXT where

import Prelude hiding ((.), id)

import Control.Category
import Text.XML.HXT.Core

collect :: XmlTrees -> [String]
collect = runLA
  ( getText
  . deep (deep isText . hasName "b")
  . unlistA
  )

parse :: String -> XmlTrees
parse = runLA xread

print :: XmlTrees -> String
print = concat . runLA (xshow unlistA)

update :: XmlTrees -> XmlTrees
update = runLA
  ( processTopDown (setElemName (mkName "i") `when` hasName "b")
  . unlistA
  )
