{-# LANGUAGE NoImplicitPrelude #-}
module Xml.XPath (run, parser) where

import Control.Arrow
import Control.Category
import Control.Arrow.List
import Data.Text
import Text.XmlHtml

import Text.XmlHtml.Arrow
import Xml.XPath.Evaluator
import Xml.XPath.Parser (parser)

run :: Text -> Node -> [Node]
run path = runListArrow (unZ . nodeV . evaluate path . arr NodeValue . mkZ)

