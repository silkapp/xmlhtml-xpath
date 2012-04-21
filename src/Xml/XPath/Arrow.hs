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
module Xml.XPath.Arrow
(

  Z (focus)
, mkZ
, unZ

, bind
, merge

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
)
where

import Control.Applicative
import Control.Arrow
import Control.Arrow.ArrowF
import Control.Category
import Data.Function (on)
import Data.Maybe
import Data.ByteString (ByteString)
import Data.Foldable hiding (elem, concat)
import Data.Text (Text)
import Data.Traversable
import Prelude hiding (elem, const, (.), id, mapM)
import Text.XmlHtml (Node, isElement)

import qualified Text.XmlHtml as X

type Attr = (Text, Text)

type Set a = [Z a]

data Z a = Z
  { focus     :: a
  , _pos      :: [Int]
  , _parent   :: Maybe (Z Node)
  , _lefts    :: [a]
  , _rights   :: [a]
  } deriving (Functor, Foldable, Traversable)

instance Show a => Show (Z a) where
  show (Z f p _ _ _) = "(Z) " ++ show p ++ " " ++ show f

instance Eq (Z a) where
  a == b = compare a b == EQ

instance Ord (Z a) where
  compare = compare `on` _pos

-- | Merge a series of ordered node sets into a single node set. This defintion
-- is a modified copied from the standard Haskell Data.List.sortBy.

merge :: [Set a] -> Set a
merge = mAll
  where mAll []             = []
        mAll [x]            = x
        mAll xs             = mAll (mPairs xs)
        mPairs (a:b:xs)     = m a b : mPairs xs
        mPairs xs           = xs
        m as@(a:as')
              bs@(b:bs')
          | a `compare` b == GT = b:m as  bs'
          | otherwise           = a:m as' bs
        m [] bs             = bs
        m as []             = as

bind :: (Z a -> Set b) -> Set a -> Set b
bind f = merge . map f

mkZ :: Arrow (~>) => a ~> Z a
mkZ = arr (\a -> Z a [0] Nothing [] [])

unZ :: Arrow (~>) => Z a ~> a
unZ = arr focus

name :: (ArrowF f (~>), Alternative f) => Z Node ~> Z Text
name = arr (fmap X.elementTag) . isElem

children :: ArrowF [] (~>) => Z Node ~> Z Node
children = embed . arr (down X.elementChildren) . isElem

down :: (Node -> [a]) -> Z Node -> [Z a]
down f z = groupSiblings z (f (focus z))

groupSiblings :: Z Node -> [a] -> [Z a]
groupSiblings z xs =
      (\(c, l, r) -> Z c (_pos z ++ [length l]) (Just z) l r)
  <$> (\(x, i) -> (x, take i xs, drop (i + 1) xs))
  <$> zip xs [0..]

attributes :: ArrowF [] (~>) => Z Node ~> Z Attr
attributes = embed . arr (down X.elementAttrs) . isElem

isElem, isText, isComment :: (ArrowF f (~>), Alternative f) => Z Node ~> Z Node
isElem    = isA (\z -> case focus z of X.Element  {} -> True; _ -> False)
isText    = isA (\z -> case focus z of X.TextNode {} -> True; _ -> False)
isComment = isA (\z -> case focus z of X.Comment  {} -> True; _ -> False)

key :: Arrow (~>) => Z Attr ~> Z Text
key = arr (fmap fst)

value :: Arrow (~>) => Z Attr ~> Z Text
value = arr (fmap snd)

text :: ArrowF [] (~>) => Z Node ~> Z Text
text = mkZ . embed . arr (\c -> case focus c of X.TextNode t -> [t]; _ -> [])

elem :: ArrowF [] (~>) => (Text -> Bool) -> Z Node ~> Z Node
elem f = isA (\z -> case focus z of X.Element e _ _ | f e -> True; _ -> False)

attr :: (ArrowF [] (~>), ArrowChoice (~>)) => (Text -> Bool) -> Z Node ~> Z Text
attr f = (isA (f . focus) . key `guards` value) . attributes

child :: ArrowF [] (~>) => (Text -> Bool) -> Z Node ~> Z Node
child f = elem f . children

hasAttr :: (ArrowF [] (~>), ArrowChoice (~>)) => (Text -> Bool) -> Z Node ~> Z Node
hasAttr f = filterA (isA (f . focus) . key . attributes)

----------------

parent :: ArrowF [] (~>) => Z a ~> Z Node
parent = embed . arr (maybeToList . _parent)

ancestors :: (ArrowF [] (~>), ArrowPlus (~>)) => Z Node ~> Z Node
ancestors = (id <+> ancestors) . parent

root :: Arrow (~>) => Z Node ~> Z Node
root = arr up where up p = maybe p up (_parent p)

lefts :: ArrowF [] (~>) => Z Node ~> Z Node
lefts = embed . arr (\x -> take (length (_lefts x)) (grouped x))

rights :: ArrowF [] (~>) => Z Node ~> Z Node
rights = embed . arr (\x -> drop (length (_lefts x) + 1) (grouped x))

siblings :: (ArrowF [] (~>), ArrowPlus (~>)) => Z Node ~> Z Node
siblings = lefts <+> rights

position :: Arrow (~>) => Z Node ~> Int
position = arr (length . filter isElement . _lefts)

grouped :: Z Node -> [Z Node]
grouped z = groupSiblings z (_lefts z ++ focus z : _rights z)

----------------

deep :: (ArrowF [] (~>), ArrowPlus (~>)) => (Z Node ~> a) -> Z Node ~> a
deep e = e <+> deep e . children

deepWhen :: (ArrowF [] (~>), ArrowChoice (~>), ArrowPlus (~>)) => (Z Node ~> c) -> (Z Node ~> a) -> Z Node ~> a
deepWhen g e = e <+> g `guards` deepWhen g e . children

deepText :: (ArrowF [] (~>), ArrowPlus (~>)) => Z Node ~> Z Text
deepText = deep text

----------------

-- TODO: how to dertermine the zipper evironment for the newly created?

toElem :: (ArrowF [] (~>), ArrowPlus (~>)) => (a ~> Z Text) -> [a ~> Z Attr] -> [a ~> Z Node] -> a ~> Z Node
toElem q as cs = proc i ->
  do n <- arr focus . q -< i
     a <- observe (arr focus . concatA as) -< i
     c <- observe (arr focus . concatA cs) -< i
     mkZ -< X.Element n a c

toAttr :: Arrow (~>) => (a ~> Z Text) -> (a ~> Z Text) -> a ~> Z Attr
toAttr q s = proc i ->
  do n <- arr focus . q -< i
     v <- arr focus . s -< i
     mkZ -< (n, v)

toText :: Arrow (~>) => Z Text ~> Z Node
toText = arr (fmap X.TextNode)

----------------

mkElem :: (ArrowF [] (~>), ArrowPlus (~>)) => Text -> [a ~> Z Attr] -> [a ~> Z Node] -> a ~> Z Node
mkElem q = toElem (mkZ . const q)

mkAttr :: Arrow (~>) => Text -> Z Text ~> Z Attr
mkAttr k = toAttr (mkZ . const k) id

mkAttrValue :: Arrow (~>) => Text -> Text -> a ~> Z Attr
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

processAttrs :: (ArrowF [] (~>), ArrowPlus (~>)) => ([Z Attr] ~> [Z Attr]) -> Z Node ~> Z Node
processAttrs a = toElem name [embed . a . observe attributes] [children]

-- | Process every Attr of an element one by one.

processAttr :: (ArrowF [] (~>), ArrowPlus (~>)) => (Z Attr ~> Z Attr) -> Z Node ~> Z Node
processAttr a = toElem name [a . attributes] [children]

----------------

parseHtml :: ArrowF [] (~>) => ByteString ~> Z Node
parseHtml = mkZ . embed . arr (either (const []) X.docContent . X.parseHTML "")

parseXml :: ArrowF [] (~>) => ByteString ~> Z Node
parseXml = mkZ . embed . arr (either (const []) X.docContent . X.parseXML "")

