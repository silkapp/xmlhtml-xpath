{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , OverloadedStrings
  , TypeOperators
  #-}
module Xml.XPath.Evaluator where

import Prelude hiding (const, elem, id, (.))

import Control.Arrow
import Control.Arrow.ListLike.Class
import Control.Category
import Data.Attoparsec.Text
import Data.Text (Text)
import Text.XmlHtml (Node (TextNode))

import Text.XmlHtml (nodeText)
import Text.XmlHtml.Arrow
import Xml.XPath.Types

import qualified Data.Text        as T
import qualified Xml.XPath.Parser as Parser

import Safe

data Value
  = NodeValue (Z Node)
  | AttrValue (Z Attr)
  | TextValue Text
  | NumValue  Number
  deriving Show

valueToNode :: Value -> Node
valueToNode (NodeValue nz) = focus nz
valueToNode v              = TextNode (stringValue v)


nodeV :: ArrowListLike [] arr => Value `arr` Z Node
nodeV = embed . arr (\n -> case n of NodeValue z -> [z]; _ -> [])

attrV :: ArrowListLike [] arr => Value `arr` Z Attr
attrV = embed . arr (\n -> case n of AttrValue z -> [z]; _ -> [])

textV :: ArrowListLike [] arr => Value `arr` Text
textV = embed . arr (\n -> case n of TextValue t -> [t]; _ -> [])

numberV :: ArrowListLike [] arr => Value `arr` Number
numberV = embed . arr (\n -> case n of NumValue m -> [m]; _ -> [])


-------------------------------------------------------------------------------

evaluate :: (ArrowListLike [] arr, ArrowChoice arr, ArrowPlus arr) => Text -> Value `arr` Value
evaluate path =
  case parser path of
    Left  e -> error (show e)
    Right e -> expression e

parser :: Text -> Either String Expr
parser = parseOnly Parser.expr

-------------------------------------------------------------------------------

locationPath :: (ArrowListLike [] arr, ArrowChoice arr, ArrowPlus arr) => LocationPath -> Z Node `arr` Value
locationPath path =
  case path of
    Relative xs -> go xs
    Absolute xs -> go xs . root
  where go []     = arr NodeValue
        go [x]    = step x
        go (x:xs) = go xs . nodeV . step x

step :: (ArrowListLike [] arr, ArrowChoice arr, ArrowPlus arr) => Step -> Z Node `arr` Value
step (Step axis test exprs)
  = foldr (\e b -> filterA (expression e) . b) id exprs
  . filterA (nodeTest test)
  . axisSpecifier axis

nodeTest :: (ArrowListLike [] arr, ArrowPlus arr, ArrowChoice arr) => NodeTest -> Value `arr` Value
nodeTest (NameTest t) = filterA (nameTest t . name . nodeV)
                    <+> filterA (nameTest t . key  . attrV)
nodeTest (NodeType t) = filterA (nodeType t .        nodeV)
nodeTest (PiTest   _) = none

nameTest :: ArrowListLike [] arr => NameTest -> Z Text `arr` Z Text
nameTest Star        = id
nameTest (NsStar ns) = isA ((== ns) . T.takeWhile (/= ':') . focus)
nameTest (QName  ns) = isA ((== ns) . focus)

axisSpecifier :: (ArrowListLike [] arr, ArrowPlus arr) => AxisSpecifier -> Z Node `arr` Value
axisSpecifier (NamedAxis axis) = axisName axis
axisSpecifier NodeAxis         = arr NodeValue
axisSpecifier AttrAxis         = arr AttrValue . attributes

nodeType :: ArrowListLike [] arr => NodeType -> Z Node `arr` Z Node
nodeType Comment               = isComment
nodeType Text                  = isText
nodeType ProcessingInstruction = none
nodeType Node                  = id

axisName :: (ArrowListLike [] arr, ArrowPlus arr) => AxisName -> Z Node `arr` Value
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

expression :: (ArrowListLike [] arr, ArrowPlus arr, ArrowChoice arr) => Expr -> Value `arr` Value
expression expr =
  case expr of
    Number _ -> go (Is (FunctionCall "position" []) expr)
    _        -> go expr
  where
    go (Is  a b             ) = arr fst . isA ((==EQ) . uncurry cmpValue) . (go a &&& go b)
    go (Lt  a b             ) = arr fst . isA ((==LT) . uncurry cmpValue) . (go a &&& go b)
    go (Gt  a b             ) = arr fst . isA ((==GT) . uncurry cmpValue) . (go a &&& go b)
    go (Lte a b             ) = arr fst . isA ((/=GT) . uncurry cmpValue) . (go a &&& go b)
    go (Gte a b             ) = arr fst . isA ((/=LT) . uncurry cmpValue) . (go a &&& go b)
    go (Or  a b             ) = go a <+> go b
    go (And a b             ) = arr fst . (go a &&& go b)
    go (Path    p           ) = locationPath p . nodeV
    go (Literal t           ) = arr TextValue . const t
    go (FunctionCall nm args) = functionCall nm args
    go (Number n            ) = const (NumValue n)

functionCall :: (ArrowListLike [] arr, ArrowPlus arr, ArrowChoice arr) => Text -> [Expr] -> Value `arr` Value
functionCall "position" _   = arr (NumValue . fromIntegral . (+1)) . position . nodeV
functionCall "count"    [x] = embed . arr (return . NumValue . fromIntegral . length) . observe (expression x)
functionCall "count"    _   = error $ "functionCall for count expects exactly one argument."
functionCall "number"   _   = maybeA . arr (fmap NumValue . numberValue)
functionCall nm         _   = error $ "functionCall for " ++ T.unpack nm ++ " not implemented."

cmpValue :: Value -> Value -> Ordering
cmpValue (NumValue n) (NumValue m) = compare n m
cmpValue n            m            = compare (stringValue n) (stringValue m)

stringValue :: Value -> Text
stringValue (NodeValue a) = nodeText (focus a)
stringValue (TextValue a) = a
stringValue (AttrValue a) = snd (focus a)
stringValue (NumValue  a) = T.pack (show a)

numberValue :: Value -> Maybe Number
numberValue (NumValue n)  = Just n
numberValue v             = fmap D ((readMay $ T.unpack $ stringValue v) :: Maybe Double)
