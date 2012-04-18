module Xml.XPath.Types where

import Data.Attoparsec.Text
import Data.Text (Text)
import Prelude hiding (takeWhile)

newtype XPath = XPath Expr
  deriving Show

data LocationPath
  = Absolute [Step]
  | Relative [Step]
  deriving Show

data Step = Step AxisSpecifier NodeTest [Expr]
  deriving Show

data NodeTest
  = NameTest NameTest
  | NodeType NodeType
  | PiTest   Text
  deriving Show

data NameTest
  = Star
  | NsStar Text
  | QName  Text
  deriving Show

data AxisSpecifier
  = NamedAxis AxisName
  | NodeAxis
  | AttrAxis
  deriving Show

data AxisName
  = Ancestor
  | AncestorOrSelf
  | Attribute
  | Child
  | Descendant
  | DescendantOrSelf
  | Following
  | FollowingSibling
  | Namespace
  | Parent
  | Preceding
  | PrecedingSibling
  | Self
  deriving Show

data NodeType
  = Comment
  | Text
  | ProcessingInstruction
  | Node
  deriving Show

data Expr
  = Or           Expr Expr
  | And          Expr Expr
  | Is           Expr Expr
  | IsNot        Expr Expr
  | Lt           Expr Expr
  | Gt           Expr Expr
  | Lte          Expr Expr
  | Gte          Expr Expr
  | Add          Expr Expr
  | Sub          Expr Expr
  | Mul          Expr Expr
  | Div          Expr Expr
  | Mod          Expr Expr
  | Unary        Expr
  | Union        Expr Expr
  | Path         LocationPath
  | Children     Expr [Step]
  | DeepChildren Expr [Step]
  | Filter       Expr Expr
  | Variable     Text
  | Literal      Text
  | Number       Number
  | FunctionCall Text [Expr]
  deriving Show

