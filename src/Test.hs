{-# LANGUAGE OverloadedStrings #-}
module Test where

import Text.XmlHtml
import Prelude hiding ((.), id, elem)

import qualified Data.ByteString as B

import Xml.XPath

main :: IO ()
main =
  do file <- B.readFile "data/test.html"

     print (parser thePath)

     case parseHTML "data/test.html" file of
       Left err  -> error err
       Right doc -> mapM_ print (run thePath (Element "" [] (docContent doc)))

  where thePath = "html/body//a[position() > 3 and position() <= 5]"

--  //permission[(canRead = 'true' or canWrite='true') and isAdmin='false']//site

