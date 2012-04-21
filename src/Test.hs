{-# LANGUAGE OverloadedStrings #-}
module Test where

import Text.XmlHtml

import qualified Data.Text.IO    as T
import qualified Text.XmlHtml    as X
import qualified Data.ByteString as B

import Xml.XPath

main :: IO ()
main =
  do file <- B.readFile "data/test.html"

     let parsed =
           case parser thePath of
             Left  e      -> error e
             Right parsed -> parsed

     print thePath
     print parsed
     T.putStrLn (printer parsed)

--      case parseHTML "data/test.html" file of
--        Left err  -> error err
--        Right doc -> mapM_ (print . X.tagName) (evaluate thePath (Element "" [] (docContent doc)))

  where thePath = ".//a/../a[b/c][position(1,//a)]"
--   where thePath = "(//a/*)[@href='te\"st' or $a > 20]"

--  //permission[(canRead = 'true' or canWrite='true') and isAdmin='false']//site

