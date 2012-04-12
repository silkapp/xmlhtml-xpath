{-# LANGUAGE OverloadedStrings #-}
module Test where

import Control.Category
import Control.Arrow
import Control.Arrow.List
import Control.Arrow.ArrowF
import Text.XmlHtml
import Prelude hiding ((.), id, elem)

import qualified Data.ByteString as B

import Text.XmlHtml.Arrow
import Xml.XPath.Evaluator

main :: IO ()
main =
  do file <- B.readFile "../data/test.html"
     case parseHTML "../data/test.html" file of
       Left err  -> error err
       Right doc -> mapM_ print (runListArrow (arrow . mkZ . embed) (docContent doc))

  where arrow = name . evaluate "child::node()"

