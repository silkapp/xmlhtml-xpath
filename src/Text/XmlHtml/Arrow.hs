{-# LANGUAGE
    Arrows
  , DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , FlexibleContexts
  , TemplateHaskell
  , TypeOperators
  #-}
{- | List arrows for querying, creating and modifying XML/HTML trees.  -}
module Text.XmlHtml.Arrow
  (

    Z (focus)
  , mkZ
  , unZ

  -- * Selection.

  , name
  , attributes
  , children
  , key
  , value
  , text

  -- * Going up and side-ways.

  , parent
  , ancestors
  , root
  , lefts
  , rights
  , siblings
  , position

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
  , Attr
  ) where

import Prelude hiding (const, elem, id, mapM, (.))

import Control.Applicative
import Control.Arrow
import Control.Arrow.ArrowF
import Control.Category
import Data.ByteString (ByteString)
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Text (Text)
import Data.Traversable
import Text.XmlHtml (Node, isElement)

import qualified Text.XmlHtml as X

type Attr = (Text, Text)

data Z a = Z
  { focus   :: a
  , _parent :: Maybe (Z Node)
  , _lefts  :: [a]
  , _rights :: [a]
  } deriving (Functor, Foldable, Traversable)

instance Show a => Show (Z a) where
  show (Z f _ _ _) = "(Z) " ++ show f

mkZ :: Arrow arr => a `arr` Z a
mkZ = arr (\a -> Z a Nothing [] [])

unZ :: Arrow arr => Z a `arr` a
unZ = arr focus

name :: (ArrowF f arr, Alternative f) => Z Node `arr` Z Text
name = arr (fmap X.elementTag) . isElem

children :: ArrowF [] arr => Z Node `arr` Z Node
children = embed . arr (down X.elementChildren) . isElem

down :: (Node -> [a]) -> Z Node -> [Z a]
down f z = groupSiblings z (f (focus z))

groupSiblings :: Z Node -> [a] -> [Z a]
groupSiblings z xs =
      (\(c, before, after) -> Z c (Just z) before after)
  <$> (\(x, i) -> (x, take i xs, drop (i + 1) xs))
  <$> zip xs [0..]

attributes :: ArrowF [] arr => Z Node `arr` Z Attr
attributes = embed . arr (down X.elementAttrs) . isElem

isElem, isText, isComment :: (ArrowF f arr, Alternative f) => Z Node `arr` Z Node
isElem    = isA (\z -> case focus z of X.Element  {} -> True; _ -> False)
isText    = isA (\z -> case focus z of X.TextNode {} -> True; _ -> False)
isComment = isA (\z -> case focus z of X.Comment  {} -> True; _ -> False)

key :: Arrow arr => Z Attr `arr` Z Text
key = arr (fmap fst)

value :: Arrow arr => Z Attr `arr` Z Text
value = arr (fmap snd)

text :: ArrowF [] arr => Z Node `arr` Z Text
text = mkZ . embed . arr (\c -> case focus c of X.TextNode t -> [t]; _ -> [])

elem :: ArrowF [] arr => (Text -> Bool) -> Z Node `arr` Z Node
elem f = isA (\z -> case focus z of X.Element e _ _ | f e -> True; _ -> False)

attr :: (ArrowF [] arr, ArrowChoice arr) => (Text -> Bool) -> Z Node `arr` Z Text
attr f = (isA (f . focus) . key `guards` value) . attributes

child :: ArrowF [] arr => (Text -> Bool) -> Z Node `arr` Z Node
child f = elem f . children

hasAttr :: (ArrowF [] arr, ArrowChoice arr) => (Text -> Bool) -> Z Node `arr` Z Node
hasAttr f = filterA (isA (f . focus) . key . attributes)

----------------

parent :: ArrowF [] arr => Z a `arr` Z Node
parent = embed . arr (maybeToList . _parent)

ancestors :: (ArrowF [] arr, ArrowPlus arr) => Z Node `arr` Z Node
ancestors = (id <+> ancestors) . parent

root :: Arrow arr => Z Node `arr` Z Node
root = arr up where up p = maybe p up (_parent p)

lefts :: ArrowF [] arr => Z Node `arr` Z Node
lefts = embed . arr (\x -> take (length (_lefts x)) (grouped x))

rights :: ArrowF [] arr => Z Node `arr` Z Node
rights = embed . arr (\x -> drop (length (_lefts x) + 1) (grouped x))

siblings :: (ArrowF [] arr, ArrowPlus arr) => Z Node `arr` Z Node
siblings = lefts <+> rights

position :: Arrow arr => Z Node `arr` Int
position = arr (length . filter isElement . _lefts)

grouped :: Z Node -> [Z Node]
grouped z = groupSiblings z (_lefts z ++ focus z : _rights z)

----------------

deep :: (ArrowF [] arr, ArrowPlus arr) => (Z Node `arr` a) -> Z Node `arr` a
deep e = e <+> deep e . children

deepWhen :: (ArrowF [] arr, ArrowChoice arr, ArrowPlus arr) => (Z Node `arr` c) -> (Z Node `arr` a) -> Z Node `arr` a
deepWhen g e = e <+> g `guards` deepWhen g e . children

deepText :: (ArrowF [] arr, ArrowPlus arr) => Z Node `arr` Z Text
deepText = deep text

----------------

-- TODO: how to dertermine the zipper evironment for the newly created?

toElem :: (ArrowF [] arr, ArrowPlus arr) => (a `arr` Z Text) -> [a `arr` Z Attr] -> [a `arr` Z Node] -> a `arr` Z Node
toElem q as cs = proc i ->
  do n <- arr focus . q -< i
     a <- observe (arr focus . concatA as) -< i
     c <- observe (arr focus . concatA cs) -< i
     mkZ -< X.Element n a c

toAttr :: Arrow arr => (a `arr` Z Text) -> (a `arr` Z Text) -> a `arr` Z Attr
toAttr q s = proc i ->
  do n <- arr focus . q -< i
     v <- arr focus . s -< i
     mkZ -< (n, v)

toText :: Arrow arr => Z Text `arr` Z Node
toText = arr (fmap X.TextNode)

----------------

mkElem :: (ArrowF [] arr, ArrowPlus arr) => Text -> [a `arr` Z Attr] -> [a `arr` Z Node] -> a `arr` Z Node
mkElem q = toElem (mkZ . const q)

mkAttr :: Arrow arr => Text -> Z Text `arr` Z Attr
mkAttr k = toAttr (mkZ . const k) id

mkAttrValue :: Arrow arr => Text -> Text -> a `arr` Z Attr
mkAttrValue k v = mkAttr k . mkZ . const v

mkText :: Arrow arr => Text -> a `arr` Z Node
mkText t = toText . mkZ . const t

----------------

-- | Process the list of children of an element.

processChildren :: (ArrowF [] arr, ArrowPlus arr) => ([Z Node] `arr` [Z Node]) -> Z Node `arr` Z Node
processChildren a = toElem name [attributes] [embed . a . observe children]

-- | Process every child of an element one by one.

processChild :: (ArrowF [] arr, ArrowPlus arr) => (Z Node `arr` Z Node) -> Z Node `arr` Z Node
processChild a = toElem name [attributes] [a . children]

-- | If the condition holds, apply the arrow and continue processing the
-- children recursively with the same condition. Otherwise, do nothing and stop
-- recursing.

processDeep :: (ArrowF [] arr, ArrowPlus arr, ArrowChoice arr) => (Z Node `arr` c) -> (Z Node `arr` Z Node) -> Z Node `arr` Z Node
processDeep c a = processChild (processDeep c a) . a  `when` c

-- | Process the text of text node.

processText :: ArrowF [] arr => (Z Text `arr` Z Text) -> Z Node `arr` Z Node
processText a = toText . a . text

-- | Process the list of attributes of an element.

processAttrs :: (ArrowF [] arr, ArrowPlus arr) => ([Z Attr] `arr` [Z Attr]) -> Z Node `arr` Z Node
processAttrs a = toElem name [embed . a . observe attributes] [children]

-- | Process every Attr of an element one by one.

processAttr :: (ArrowF [] arr, ArrowPlus arr) => (Z Attr `arr` Z Attr) -> Z Node `arr` Z Node
processAttr a = toElem name [a . attributes] [children]

----------------

parseHtml :: ArrowF [] arr => ByteString `arr` Z Node
parseHtml = mkZ . embed . arr (either (const []) X.docContent . X.parseHTML "")

parseXml :: ArrowF [] arr => ByteString `arr` Z Node
parseXml = mkZ . embed . arr (either (const []) X.docContent . X.parseXML "")
