{-# LANGUAGE
    TypeOperators
  , Arrows
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , FlexibleContexts
  , TemplateHaskell
  #-}
{- | List arrows for querying, creating and modifying XML/HTML trees.  -}
module Text.XmlHtml.Arrow
(

-- * Run an arrow.
  run

-- * Selection.

, name
, attributes
, children
, key
, value
, text

-- * Going up and side-ways.

, parent
, root
-- , lefts
-- , rights

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

import Control.Applicative
import Control.Arrow
import Control.Arrow.ArrowF
import Control.Arrow.List
import Control.Category
import Data.ByteString (ByteString)
import Data.Foldable hiding (elem)
import Data.Text (Text)
import Data.Traversable
import Prelude hiding (elem, const, (.), id, mapM)
import Text.XmlHtml (Node)

import qualified Text.XmlHtml as X

type Attribute = (Text, Text)

data Z a = Z
  { focus     :: a
  , ancestors :: [Node]
--   , lefts     :: [Node]
--   , rights    :: [Node]
  } deriving (Functor, Foldable, Traversable)

mkZ :: Arrow (~>) => a ~> Z a
mkZ = arr (\a -> Z a [])

run :: ListArrow (Z a) (Z b) -> [a] -> [b]
run a = map focus . runListArrow (a . mkZ . embed)

name :: (ArrowF f (~>), Alternative f) => Z Node ~> Z Text
name = arr (fmap X.elementTag) . isElem

children :: ArrowF [] (~>) => Z Node ~> Z Node
children = embed . arr (\(Z x xs) -> let ys = x:xs in (\c -> Z c ys) <$> X.elementChildren x) . isElem

attributes :: ArrowF [] (~>) => Z Node ~> Z Attribute
attributes = embed . arr (\(Z x xs) -> let ys = x:xs in flip Z ys <$> X.elementAttrs x) . isElem

isElem, isText, isComment :: (ArrowF f (~>), Alternative f) => Z Node ~> Z Node
isElem    = isA (\z -> case focus z of X.Element  {} -> True; _ -> False)
isText    = isA (\z -> case focus z of X.TextNode {} -> True; _ -> False)
isComment = isA (\z -> case focus z of X.Comment  {} -> True; _ -> False)

key :: Arrow (~>) => Z Attribute ~> Z Text
key = arr (fmap fst)

value :: Arrow (~>) => Z Attribute ~> Z Text
value = arr (fmap snd)

text :: ArrowF [] (~>) => Z Node ~> Z Text
text = embed . arr (mapM (\c -> case c of X.TextNode t -> [t]; _ -> []))

elem :: ArrowF [] (~>) => (Text -> Bool) -> Z Node ~> Z Node
elem f = embed . arr (mapM (\n -> case n of X.Element e _ _ | f e -> [n]; _ -> []))

attr :: (ArrowF [] (~>), ArrowChoice (~>)) => (Text -> Bool) -> Z Node ~> Z Text
attr f = (isA (f . focus) . key `guards` value) . attributes

child :: ArrowF [] (~>) => (Text -> Bool) -> Z Node ~> Z Node
child f = elem f . children

hasAttr :: (ArrowF [] (~>), ArrowChoice (~>)) => (Text -> Bool) -> Z Node ~> Z Node
hasAttr f = filterA (isA (f . focus) . key . attributes)

----------------

parent :: ArrowF [] (~>) => Z a ~> Z Node
parent = embed . arr (\z -> case ancestors z of x:xs -> [Z x xs]; _ -> [])

root :: ArrowF [] (~>) => Z a ~> Z Node
root = mkZ . embed . arr (take 1 . reverse . ancestors)

----------------

deep :: (ArrowF [] (~>), ArrowPlus (~>)) => (Z Node ~> a) -> Z Node ~> a
deep e = e <+> deep e . children

deepWhen :: (ArrowF [] (~>), ArrowChoice (~>), ArrowPlus (~>)) => (Z Node ~> c) -> Z Node ~> a -> Z Node ~> a
deepWhen g e = e <+> g `guards` deepWhen g e . children

deepText :: (ArrowF [] (~>), ArrowPlus (~>)) => Z Node ~> Z Text
deepText = deep text

----------------

-- TODO: how to dertermine the zipper evironment for the newly created?

toElem :: (ArrowF [] (~>), ArrowPlus (~>)) => (a ~> Z Text) -> [a ~> Z Attribute] -> [a ~> Z Node] -> a ~> Z Node
toElem q as cs = proc i ->
  do n <- arr focus . q -< i
     a <- observe (arr focus . concatA as) -< i
     c <- observe (arr focus . concatA cs) -< i
     mkZ -< X.Element n a c

toAttr :: Arrow (~>) => (a ~> Z Text) -> (a ~> Z Text) -> a ~> Z Attribute
toAttr q s = proc i ->
  do n <- arr focus . q -< i
     v <- arr focus . s -< i
     mkZ -< (n, v)

toText :: Arrow (~>) => Z Text ~> Z Node
toText = arr (fmap X.TextNode)

----------------

mkElem :: (ArrowF [] (~>), ArrowPlus (~>)) => Text -> [a ~> Z Attribute] -> [a ~> Z Node] -> a ~> Z Node
mkElem q = toElem (mkZ . const q)

mkAttr :: Arrow (~>) => Text -> Z Text ~> Z Attribute
mkAttr k = toAttr (mkZ . const k) id

mkAttrValue :: Arrow (~>) => Text -> Text -> a ~> Z Attribute
mkAttrValue k v = mkAttr k . mkZ . const v

mkText :: Arrow (~>) => Text -> a ~> Z Node
mkText t = toText . mkZ . const t

----------------

-- | Process the list of children of an element.

processChildren :: (ArrowF [] (~>), ArrowPlus (~>)) => ([Z Node] ~> [Z Node]) -> Z Node ~> Z Node
processChildren a = toElem name [attributes] [embed . a . observe children]

-- | Process every child of an element one by one.

processChild :: (ArrowF [] (~>), ArrowPlus (~>)) => (Z Node ~> Z Node) -> Z Node ~> Z Node
processChild a = toElem name [attributes] [a . children]

-- | If the condition holds, apply the arrow and continue processing the
-- children recursively with the same condition. Otherwise, do nothing and stop
-- recursing.

processDeep :: (ArrowF [] (~>), ArrowPlus (~>), ArrowChoice (~>)) => (Z Node ~> c) -> (Z Node ~> Z Node) -> Z Node ~> Z Node
processDeep c a = processChild (processDeep c a) . a  `when` c

-- | Process the text of text node.

processText :: ArrowF [] (~>) => (Z Text ~> Z Text) -> Z Node ~> Z Node
processText a = toText . a . text

-- | Process the list of attributes of an element.

processAttrs :: (ArrowF [] (~>), ArrowPlus (~>)) => ([Z Attribute] ~> [Z Attribute]) -> Z Node ~> Z Node
processAttrs a = toElem name [embed . a . observe attributes] [children]

-- | Process every attribute of an element one by one.

processAttr :: (ArrowF [] (~>), ArrowPlus (~>)) => (Z Attribute ~> Z Attribute) -> Z Node ~> Z Node
processAttr a = toElem name [a . attributes] [children]

----------------

parseHtml :: ArrowF [] (~>) => ByteString ~> Z Node
parseHtml = mkZ . embed . arr (either (const []) X.docContent . X.parseHTML "")

parseXml :: ArrowF [] (~>) => ByteString ~> Z Node
parseXml = mkZ . embed . arr (either (const []) X.docContent . X.parseXML "")

