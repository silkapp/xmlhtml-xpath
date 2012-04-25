{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import Test.HUnit (assertEqual)
import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Text.XmlHtml
import qualified Data.ByteString as BS
import qualified Blaze.ByteString.Builder as Builder
import qualified Xml.XPath       as XPath

selectParentNode :: Text
selectParentNode = "/parent"

testCasePath = "tests/cases"

tests :: Document -> Document -> [Test]
tests xml res = [ testCase "blah" (assertEqual "select parent node" (Builder.toByteString . renderXmlFragment UTF8 $ XPath.evaluate selectParentNode node) (Builder.toByteString . renderXmlFragment UTF8 . init $ resNodes)) ]
  where
    node = head (docContent xml)
    resNodes = docContent res

mkTest :: Node -> String  -> IO Test
mkTest input testName =
  do let fileName = testCasePath </> testName </> test.xml
     testDescr <- readTest fileName
     TODO

mkTests :: Node -> IO [Test]
mkTests input =
  do ts <- drop 2 <$> getDirectoryContents testCasePath
     forM ts (mkTest xml)

main :: IO ()
main =
  do xml <- readTestFile
     tests <- mkTests xml
     defaultMain tests

readTestFile =
  do let fileName = "tests/test-file.xml"
     bs <- BS.readFile fileName 
     let xml = either (error . ("Error parsing xml: " ++)) id (parseXML fileName bs)
     return xml

readResultFile =
  do let fileName = "tests/cases/select-parent-node/expected-result.xml"
     bs <- BS.readFile fileName 
     let xml = either (error . ("Error parsing xml: " ++)) id (parseXML fileName bs)
     return xml
