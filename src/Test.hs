{-# LANGUAGE OverloadedStrings #-}
module Test where

import Control.Category
import Control.Arrow
import Control.Arrow.List
import Control.Arrow.ArrowF
import Text.XmlHtml
import Text.XmlHtml.Arrow
import Prelude hiding ((.), id, elem)

import qualified Data.ByteString as B

main :: IO ()
main =
  do file <- B.readFile "../data/test.html"
     case parseHTML "../data/test.html" file of
       Left err  -> error err
       Right doc -> mapM_ print (runListArrow (arrow . mkZ . embed) (docContent doc))

  where arrow = arr focus
              . name
              . siblings
              . ancestors
              . deep (elem (== "br"))

