{-# LANGUAGE NoImplicitPrelude #-}
module Xml.XPath (run) where

import Control.Category
import Control.Arrow.List
import Data.Text
import Text.XmlHtml
import Text.XmlHtml.Arrow
import Xml.XPath.Evaluator

run :: Text -> Node -> [Node]
run path = runListArrow (unZ . nodeV . evaluate path . mkZ)
