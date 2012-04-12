{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Xml.XPath.Evaluator where

import Control.Applicative
import Control.Category
import Control.Arrow
import Control.Arrow.ArrowF
import Data.Attoparsec.Text
import Data.Text
import Text.XmlHtml (Node)
import Prelude hiding ((.), id, elem)

import Text.XmlHtml.Arrow
import Xml.XPath.Types

import qualified Xml.XPath.Parser as Parser

data Value
  = AttrValue (Z Attr)
  | NodeValue (Z Node)
  | TextValue (Z Text)
  deriving Show

evalLocationPath :: (ArrowF [] (~>), ArrowPlus (~>)) => LocationPath -> Z Node ~> Value
evalLocationPath (Relative (x:xs)) = evalStep x

evalStep :: (ArrowF [] (~>), ArrowPlus (~>)) => Step -> Z Node ~> Value
evalStep (Step axis _ _) = evalAxisSpecifier axis

evalAxisSpecifier :: (ArrowF [] (~>), ArrowPlus (~>)) => AxisSpecifier -> Z Node ~> Value
evalAxisSpecifier (NamedAxis axis) = evalAxisName axis

evalNodeType :: ArrowF [] (~>) => NodeType -> Z Node ~> Z Node
evalNodeType Comment               = isComment
evalNodeType Text                  = isText
evalNodeType ProcessingInstruction = none
evalNodeType Node                  = id

evalAxisName :: (ArrowF [] (~>), ArrowPlus (~>)) => AxisName -> Z Node ~> Value
evalAxisName Ancestor         = arr NodeValue . ancestors
evalAxisName AncestorOrSelf   = arr NodeValue . (ancestors <+> id)
evalAxisName Attribute        = arr AttrValue . attributes
evalAxisName Child            = arr NodeValue . children
evalAxisName Descendant       = arr NodeValue . deep id
evalAxisName DescendantOrSelf = arr NodeValue . (id <+> deep id)
-- evalAxisName Following        = arr NodeValue . undefined
evalAxisName FollowingSibling = arr NodeValue . rights
evalAxisName Namespace        = none
evalAxisName Parent           = arr NodeValue . parent
-- evalAxisName Preceding        = arr NodeValue . undefined
evalAxisName PrecedingSibling = arr NodeValue . lefts
evalAxisName Self             = arr NodeValue . id


evaluate :: (ArrowF [] (~>), ArrowPlus (~>)) => Text -> Z Node ~> Value
evaluate path =
  case parseOnly Parser.locationPath path of
    Left  e -> error (show e)
    Right r -> evalLocationPath r

