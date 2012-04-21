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
import Data.Attoparsec.Text (Number)
import Data.Text (Text)
import Text.XmlHtml (Node)
import Prelude hiding ((.), id, elem, const)

import Text.XmlHtml (nodeText)
import Text.XmlHtml.Arrow
import Xml.XPath.Types
import Xml.XPath.Parser (parser)

import qualified Data.Text as T

{-
todo:
  - What about Ord and Num instances for non-numeric values?
  - What about parent selectors et al?
-}

data Value
  = NodeValue (Z Node)
  | AttrValue (Z Attr)
  | TextValue Text
  | NumValue  Number
  deriving Show

instance Eq Value where
  NumValue n == NumValue m = n == m
  a          ==  b         = stringValue a == stringValue b

instance Ord Value where
  compare (NumValue n) (NumValue m) = compare n m
  compare a            b            = compare (stringValue a) (stringValue b)

instance Num Value where
  NumValue a + NumValue b = NumValue (a + b)
  NumValue a * NumValue b = NumValue (a * b)
  abs (NumValue a) = NumValue (abs a)
  signum (NumValue a) = NumValue (signum a)
  fromInteger = NumValue . fromInteger

instance Fractional Value where
  fromRational = NumValue . fromRational

type Result = (Integer, Value)

nodeV :: ArrowF [] (~>) => Value ~> Z Node
nodeV = embed . arr (\n -> case n of NodeValue z -> [z]; _ -> [])

attrV :: ArrowF [] (~>) => Value ~> Z Attr
attrV = embed . arr (\n -> case n of AttrValue z -> [z]; _ -> [])

textV :: ArrowF [] (~>) => Value ~> Text
textV = embed . arr (\n -> case n of TextValue t -> [t]; _ -> [])

numV :: ArrowF [] (~>) => Value ~> Number
numV = embed . arr (\n -> case n of NumValue m -> [m]; _ -> [])

reindex :: ArrowF [] (~>) => (a ~> Value) -> a ~> Result
reindex ar = embed . arr (\xs -> zip [1..] xs) . observe ar

-------------------------------------------------------------------------------

evaluate :: (ArrowF [] (~>), ArrowChoice (~>), ArrowPlus (~>)) => Text -> Value ~> Value
evaluate path =
  case parser path of
    Left  e         -> error (show e)
    Right (XPath e) -> arr snd . expression e . (const 1 &&& id)

locationPath :: (ArrowF [] (~>), ArrowChoice (~>), ArrowPlus (~>)) => LocationPath -> Z Node ~> Value
locationPath path =
  case path of
    Relative xs -> steps xs
    Absolute xs -> steps xs . root

steps :: (ArrowF [] (~>), ArrowChoice (~>), ArrowPlus (~>)) => [Step] -> Z Node ~> Value
steps xs = foldr (\s b -> step s (nodeV . b)) (arr NodeValue) (reverse xs)

step :: (ArrowF [] (~>), ArrowChoice (~>), ArrowPlus (~>)) => Step -> (Z Node ~> Z Node) -> Z Node ~> Value
step (Step axis test exprs) prev
  = foldr (\e b -> arr snd . filterA (expression e) . reindex b)
    (filterA (nodeTest test) . axisSpecifier axis . prev)
    (reverse exprs)

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
axisName FollowingSibling = arr NodeValue . rights
axisName Parent           = arr NodeValue . parent
axisName PrecedingSibling = arr NodeValue . lefts
axisName Self             = arr NodeValue . id
-- axisName Preceding        = arr NodeValue
-- axisName Following        = arr NodeValue
-- axisName Namespace        = none

expression :: (ArrowF [] (~>), ArrowChoice (~>), ArrowPlus (~>)) => Expr -> Result ~> Result
expression expr = reindex (go expr)
  where go :: (ArrowF [] (~>), ArrowChoice (~>), ArrowPlus (~>)) => Expr -> Result ~> Value
        go ( Is           a b  ) = arr fst . isA (uncurry (==)) . (go a &&& go b)
        go ( IsNot        a b  ) = arr fst . isA (uncurry (/=)) . (go a &&& go b)
        go ( Lt           a b  ) = arr fst . isA (uncurry (< )) . (go a &&& go b)
        go ( Gt           a b  ) = arr fst . isA (uncurry (> )) . (go a &&& go b)
        go ( Lte          a b  ) = arr fst . isA (uncurry (<=)) . (go a &&& go b)
        go ( Gte          a b  ) = arr fst . isA (uncurry (>=)) . (go a &&& go b)
        go ( Add          a b  ) = arr (uncurry (+)) . (go a &&& go b)
        go ( Sub          a b  ) = arr (uncurry (-)) . (go a &&& go b)
        go ( Mul          a b  ) = arr (uncurry (*)) . (go a &&& go b)
        go ( Div          a b  ) = arr (uncurry (/)) . (go a &&& go b)
        go ( Or           a b  ) = go a <+> go b
        go ( And          a b  ) = arr fst . (go a &&& go b)
        go ( Literal      t    ) = arr TextValue . const t
        go ( Path         p    ) = locationPath p . nodeV . arr snd
        go ( Filter       e p  ) = arr snd . filterA (go p) . expression e
        go ( FunctionCall n as ) = fun n as
        go ( Number       n    ) = const (NumValue n)

        fun :: ArrowF [] (~>) => Text -> [Expr] -> (Integer, Value) ~> Value
        fun "position" _ = arr (NumValue . fromIntegral . fst)
        fun nm         _ = error $ "function " ++ T.unpack nm ++ " not implemented."

stringValue :: Value -> Text
stringValue (NodeValue a) = nodeText (focus a)
stringValue (TextValue a) = a
stringValue (AttrValue a) = snd (focus a)
stringValue (NumValue  a) = T.pack (show a)

