{-# LANGUAGE TypeOperators, Arrows #-}
{- | List arrows for querying, creating and modifying XML/HTML trees.  -}
module Text.XmlHtml.Arrow
(

-- * Selection.

  tag
, children
, attributes
, key
, value
, text

-- * Filter based on type.

, isElem
, isText
, isComment

-- * Filter by name.

, elem
, attr
, child
, hasAttr

-- * Deep selection.

, deep
, deepWhen
, deepText

-- * Creation with only arrow components.

, toElem
, toAttr
, toText

-- * Creation with some fixed components.

, mkElem
, mkAttr
, mkAttrValue
, mkText

-- * Processing child nodes, attributes and text.

, processChildren
, processChild
, processDeep
, processAttrs
, processAttr
, processText

-- * Arrows for parsing.

, parseHtml
, parseXml

-- * Convenient type for attributes.
, Attribute
)
where

import Control.Arrow
import Control.Arrow.ArrowList
import Control.Category
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text)
import Prelude hiding (elem, (.), id)
import Text.XmlHtml (Node)

import qualified Text.XmlHtml as X

type Attribute = (Text, Text)

tag :: ArrowList (~>) => Node ~> Text
tag = arr X.elementTag . isElem

children :: ArrowList (~>) => Node ~> Node
children = arrL X.elementChildren . isElem

attributes :: ArrowList (~>) => Node ~> Attribute
attributes = arrL X.elementAttrs . isElem

isElem, isText, isComment :: ArrowList (~>) => Node ~> Node
isElem    = isA (\c -> case c of X.Element  {} -> True; _ -> False)
isText    = isA (\c -> case c of X.TextNode {} -> True; _ -> False)
isComment = isA (\c -> case c of X.Comment  {} -> True; _ -> False)

key :: Arrow (~>) => Attribute ~> Text
key = arr fst

value :: Arrow (~>) => Attribute ~> Text
value = arr snd

text :: ArrowList (~>) => Node ~> Text
text = arrL (\c -> case c of X.TextNode t -> [t]; _ -> [])

elem :: ArrowList (~>) => (Text -> Bool) -> Node ~> Node
elem f = arrL (\n -> case n of X.Element e _ _ | f e -> [n]; _ -> [])

attr :: (ArrowList (~>), ArrowChoice (~>)) => (Text -> Bool) -> Node ~> Text
attr f = (isA f . key `guards` value) . attributes

child :: ArrowList (~>) => (Text -> Bool) -> Node ~> Node
child f = elem f . children

hasAttr :: (ArrowList (~>), ArrowChoice (~>)) => (Text -> Bool) -> Node ~> Node
hasAttr f = filterA (isA f . key . attributes)

----------------

deep :: (ArrowList (~>), ArrowPlus (~>)) => (Node ~> a) -> (Node ~> a)
deep e = e <+> deep e . children

deepWhen :: (ArrowList (~>), ArrowPlus (~>), ArrowChoice (~>)) => Node ~> c -> Node ~> a -> Node ~> a
deepWhen g e = e <+> g `guards` deepWhen g e . children

deepText :: (ArrowPlus (~>), ArrowList (~>)) => Node ~> Text
deepText = arr mconcat . list (deep text)

----------------

toElem :: (ArrowPlus (~>), ArrowList (~>)) => (a ~> Text) -> [a ~> Attribute] -> [a ~> Node] -> a ~> Node
toElem q as cs = proc i ->
  do n <- q -< i
     a <- list (concatA as) -< i
     c <- list (concatA cs) -< i
     id -< X.Element n a c

toAttr :: Arrow (~>) => (a ~> Text) -> (a ~> Text) -> a ~> Attribute
toAttr q s = proc i ->
  do n <- q -< i
     v <- s -< i
     id -< (n, v)

toText :: Arrow (~>) => Text ~> Node
toText = arr X.TextNode

----------------

mkElem :: (ArrowPlus (~>), ArrowList (~>)) => Text -> [a ~> Attribute] -> [a ~> Node] -> a ~> Node
mkElem q = toElem (arr (const q))

mkAttr :: Arrow (~>) => Text -> Text ~> Attribute
mkAttr k = toAttr (arr (const k)) id

mkAttrValue :: Arrow (~>) => Text -> Text -> a ~> Attribute
mkAttrValue k v = mkAttr k . arr (const v)

mkText :: Arrow (~>) => Text -> a ~> Node
mkText t = toText . arr (const t)

----------------

-- | Process the list of children of an element.

processChildren :: (ArrowPlus (~>), ArrowList (~>), ArrowChoice (~>)) => [Node] ~> [Node] -> Node ~> Node
processChildren a = toElem tag [attributes] [unlist . a . list children]

-- | Process every child of an element one by one.

processChild :: (ArrowPlus (~>), ArrowList (~>), ArrowChoice (~>)) => Node ~> Node -> Node ~> Node
processChild a = toElem tag [attributes] [a . children]

-- | If the condition holds, apply the arrow and continue processing the
-- children recursively with the same condition. Otherwise, do nothing and stop
-- recursing.

processDeep :: (ArrowPlus (~>), ArrowList (~>), ArrowChoice (~>)) => Node ~> c -> Node ~> Node -> Node ~> Node
processDeep c a = (processChild (processDeep c a) . a) `when` c

-- | Process the text of text node.

processText :: ArrowList (~>) => Text ~> Text -> Node ~> Node
processText a = toText . a . text

-- | Process the list of attributes of an element.

processAttrs :: (ArrowPlus (~>), ArrowList (~>)) => ([Attribute] ~> [Attribute]) -> Node ~> Node
processAttrs a = toElem tag [unlist . a . list attributes] [children]

-- | Process every attribute of an element one by one.

processAttr :: (ArrowPlus (~>), ArrowList (~>)) => (Attribute ~> Attribute) -> Node ~> Node
processAttr a = toElem tag [a . attributes] [children]

----------------

parseHtml :: ArrowList (~>) => ByteString ~> Node
parseHtml = arrL (either (const []) X.docContent . X.parseHTML "")

parseXml :: ArrowList (~>) => ByteString ~> Node
parseXml = arrL (either (const []) X.docContent . X.parseXML "")

