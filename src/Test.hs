{-# LANGUAGE OverloadedStrings #-}
module Test where

import Control.Category
import Control.Arrow.ArrowF
import Data.ByteString (ByteString)
import Text.XmlHtml
import Text.XmlHtml.Arrow
import Prelude hiding ((.), id, elem)

import qualified Data.ByteString as B



main :: IO ()
main =
  do file <- B.readFile "../data/test.html"
     case parseHTML "../data/test.html" file of
       Left err  -> error err
       Right doc -> mapM_ print (run arrow (docContent doc))

  where arrow = parent . child (== "a") . mkElem "div" [] [id] . deep (child (=="a"))

