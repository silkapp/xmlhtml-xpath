{-# LANGUAGE
    TypeOperators
  , FlexibleContexts
  , FlexibleInstances
  , OverloadedStrings
  #-}
module Xml.XPath.Evaluator where

import Control.Category
import Control.Arrow
import Control.Arrow.ArrowF
import Data.Attoparsec.Text
import Data.Text (Text)
import Text.XmlHtml (Node (TextNode))
import Prelude hiding ((.), id, elem, const)

import Text.XmlHtml (nodeText)
import Text.XmlHtml.Arrow
import Xml.XPath.Types

import qualified Data.Text        as T
import qualified Xml.XPath.Parser as Parser

data Value
  = NodeValue (Z Node)
  | AttrValue (Z Attr)
  | TextValue Text
  | NumValue  Number
  deriving Show

valueToNode :: Value -> Node
valueToNode (NodeValue nz) = focus nz
valueToNode v              = TextNode (stringValue v)


nodeV :: ArrowF [] (~>) => Value ~> Z Node
nodeV = embed . arr (\n -> case n of NodeValue z -> [z]; _ -> [])

attrV :: ArrowF [] (~>) => Value ~> Z Attr
attrV = embed . arr (\n -> case n of AttrValue z -> [z]; _ -> [])

textV :: ArrowF [] (~>) => Value ~> Text
textV = embed . arr (\n -> case n of TextValue t -> [t]; _ -> [])

numberV :: ArrowF [] (~>) => Value ~> Number
numberV = embed . arr (\n -> case n of NumValue m -> [m]; _ -> [])


-------------------------------------------------------------------------------

evaluate :: (ArrowF [] (~>), ArrowChoice (~>), ArrowPlus (~>)) => Text -> Value ~> Value
evaluate path =
  case parser path of
    Left  e -> error (show e)
    Right e -> expression e

parser :: Text -> Either String Expr
parser = parseOnly Parser.expr

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
nodeTest (PiTest   _) = none

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
expression expr =
  case expr of
    Number _ -> go (Is (FunctionCall "position" []) expr)
    _        -> go expr
  where
    go (Is  a b             ) = arr fst . isA (uncurry eqValue) . (go a &&& go b)
    go (Or  a b             ) = go a <+> go b
    go (And a b             ) = arr fst . (go a &&& go b)
    go (Path    p           ) = locationPath p . nodeV
    go (Literal t           ) = arr TextValue . const t
    go (FunctionCall nm args) = functionCall nm args
    go (Number n            ) = const (NumValue n)

functionCall :: ArrowF [] (~>) => Text -> [Expr] -> Value ~> Value
functionCall "position" _ = arr (NumValue . fromIntegral . (+1)) . position . nodeV
functionCall nm         _ = error $ "functionCall for " ++ T.unpack nm ++ " not implemented."

eqValue :: Value -> Value -> Bool
eqValue (NumValue n) (NumValue m) = n == m
eqValue a            b            = stringValue a == stringValue b

stringValue :: Value -> Text
stringValue (NodeValue a) = nodeText (focus a)
stringValue (TextValue a) = a
stringValue (AttrValue a) = snd (focus a)
stringValue (NumValue  a) = T.pack (show a)

