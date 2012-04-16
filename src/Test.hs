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
import Xml.XPath.Parser
import Xml.XPath.Evaluator

main :: IO ()
main =
  do file <- B.readFile "../data/test.html"

     print (parser thePath)

     case parseHTML "../data/test.html" file of
       Left err  -> error err
       Right doc -> mapM_ print (runListArrow (arrow . mkZ . embed) (docContent doc))

  where arrow = unZ . nodeV . evaluate thePath
        thePath = "//li[a = 'the Anchor' and a[@data-uri='nope-2']]//b"

--  //permission[(canRead = 'true' or canWrite='true') and isAdmin='false']//site

