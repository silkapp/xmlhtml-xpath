{-# LANGUAGE NoImplicitPrelude #-}
module Xml.XPath (run, valueToNode) where

import Control.Arrow
import Control.Category
import Control.Arrow.List
import Data.Text
import Text.XmlHtml
import Text.XmlHtml.Arrow
import Xml.XPath.Evaluator

run :: Text -> Node -> [Value]
run path = runListArrow (evaluate path . arr NodeValue . mkZ)
