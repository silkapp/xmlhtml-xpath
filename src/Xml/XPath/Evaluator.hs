{-# LANGUAGE
    TypeOperators
  , FlexibleContexts
  , FlexibleInstances
  #-}
module Xml.XPath.Evaluator where

import Control.Category
import Control.Arrow
import Control.Arrow.ArrowF
import Data.Attoparsec.Text
import Data.Function (on)
import Data.Text (Text)
import Text.XmlHtml (Node)
import Prelude hiding ((.), id, elem, const)

import Text.XmlHtml (nodeText)
import Text.XmlHtml.Arrow
import Xml.XPath.Types

import qualified Data.Text        as T
import qualified Xml.XPath.Parser as Parser

data Value
  = NodeValue (Z Node)
  | AttrValue (Z Attr)
  | TextValue (Z Text)
  deriving Show

nodeV :: ArrowF [] (~>) => Value ~> Z Node
nodeV = embed . arr (\n -> case n of NodeValue z -> [z]; _ -> [])

attrV :: ArrowF [] (~>) => Value ~> Z Attr
attrV = embed . arr (\n -> case n of AttrValue z -> [z]; _ -> [])

textV :: ArrowF [] (~>) => Value ~> Z Text
textV = embed . arr (\n -> case n of TextValue z -> [z]; _ -> [])

-------------------------------------------------------------------------------

evaluate :: (ArrowF [] (~>), ArrowChoice (~>), ArrowPlus (~>)) => Text -> Z Node ~> Value
evaluate path =
  case parser path of
    Left  e -> error (show e)
    Right r -> locationPath r

parser :: Text -> Either String LocationPath
parser = parseOnly Parser.locationPath

-------------------------------------------------------------------------------

locationPath :: (ArrowF [] (~>), ArrowChoice (~>), ArrowPlus (~>)) => LocationPath -> Z Node ~> Value
locationPath path =
  case path of
    Relative xs -> go xs
    Absolute xs -> go xs . root
  where go []     = arr NodeValue
        go [x]    = step x
        go (x:xs) = go xs . nodeV . step x

step :: (ArrowF [] (~>), ArrowChoice (~>), ArrowPlus (~>)) => Step -> Z Node ~> Value
step (Step axis test exprs)
  = foldr (\e b -> filterA (expression e) . b) id exprs
  . filterA (nodeTest test)
  . axisSpecifier axis

nodeTest :: (ArrowF [] (~>), ArrowPlus (~>), ArrowChoice (~>)) => NodeTest -> Value ~> Value
nodeTest (NameTest t) = filterA (nameTest t . name . nodeV)
                    <+> filterA (nameTest t . key  . attrV)
nodeTest (NodeType t) = filterA (nodeType t .        nodeV)

nameTest :: ArrowF [] (~>) => NameTest -> Z Text ~> Z Text
nameTest Star        = id
nameTest (NsStar ns) = isA ((== ns) . T.takeWhile (/= ':') . focus)
nameTest (QName  ns) = isA ((== ns) . focus)

axisSpecifier :: (ArrowF [] (~>), ArrowPlus (~>)) => AxisSpecifier -> Z Node ~> Value
axisSpecifier (NamedAxis axis) = axisName axis
axisSpecifier NodeAxis         = arr NodeValue
axisSpecifier AttrAxis         = arr AttrValue . attributes

nodeType :: ArrowF [] (~>) => NodeType -> Z Node ~> Z Node
nodeType Comment               = isComment
nodeType Text                  = isText
nodeType ProcessingInstruction = none
nodeType Node                  = id

axisName :: (ArrowF [] (~>), ArrowPlus (~>)) => AxisName -> Z Node ~> Value
axisName Ancestor         = arr NodeValue . ancestors
axisName AncestorOrSelf   = arr NodeValue . (ancestors <+> id)
axisName Attribute        = arr AttrValue . attributes
axisName Child            = arr NodeValue . children
axisName Descendant       = arr NodeValue . deep id . children
axisName DescendantOrSelf = arr NodeValue . deep id
-- axisName Following        = arr NodeValue
axisName FollowingSibling = arr NodeValue . rights
-- axisName Namespace        = none
axisName Parent           = arr NodeValue . parent
-- axisName Preceding        = arr NodeValue
axisName PrecedingSibling = arr NodeValue . lefts
axisName Self             = arr NodeValue . id

expression :: (ArrowF [] (~>), ArrowPlus (~>), ArrowChoice (~>)) => Expr -> Value ~> Value
expression (Is  a b )  = arr fst . isA (uncurry eqValue) . (expression a &&& expression b)
expression (Or  a b )  = expression a <+> expression b
expression (And a b )  = arr fst . (expression a &&& expression b)
expression (Path    p) = locationPath p . nodeV
expression (Literal t) = arr TextValue . mkZ . const t

eqValue :: Value -> Value -> Bool
eqValue = (==) `on` stringValue

stringValue :: Value -> Text
stringValue (NodeValue a) = nodeText (focus a)
stringValue (TextValue a) = focus a
stringValue (AttrValue a) = snd (focus a)

