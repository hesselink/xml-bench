{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
    DeriveGeneric
  , StandaloneDeriving
  #-}
module XmlBench.XMLLight where

import Control.DeepSeq.Generics
import GHC.Generics
import Text.XML.Light
import Text.XML.Light.Lexer

collect :: [Content] -> [String]
collect = map strContent
        . concatMap (findElements (QName "b" Nothing Nothing))
        . onlyElems

parse :: XmlSource a => a -> [Content]
parse = parseXML

print :: [Content] -> String
print = concatMap showContent

update :: [Content] -> [Content]
update = map replaceBWithI
  where
    replaceBWithI (Elem el) | qName (elName el) == "b" = Elem el { elName = QName "i" Nothing Nothing }
                            | otherwise                = Elem el { elContent = map replaceBWithI (elContent el) }
    replaceBWithI n         = n

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
