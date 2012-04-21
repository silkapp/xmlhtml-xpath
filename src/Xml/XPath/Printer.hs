{-# LANGUAGE OverloadedStrings #-}
module Xml.XPath.Printer (printer) where

import Data.Attoparsec.Number
import Data.Foldable
import Data.List
import Data.Monoid
import Data.String
import Data.Text (Text, replace)

import Xml.XPath.Types

printer :: XPath -> Text
printer (XPath e) = expr e

locationPath :: LocationPath -> Text
locationPath (Absolute p) = "/" `mappend` steps p
locationPath (Relative p) = steps p

steps :: [Step] -> Text
steps = mconcat . intersperse "/" . map step

step :: Step -> Text
step (Step a t p) = mconcat [axisSpecifier a, nodeTest t, predicates p]

predicates :: [Expr] -> Text
predicates = foldMap (\a -> "[" `mappend` expr a `mappend` "]")

axisSpecifier :: AxisSpecifier -> Text
axisSpecifier (NamedAxis n) = axisName n

nodeTest :: NodeTest -> Text
nodeTest (NameTest t) = nameTest t
nodeTest (NodeType t) = nodeType t
nodeTest (PiTest   t) = "processing-instruction(" `mappend` t `mappend` ")"

nameTest :: NameTest -> Text
nameTest Star       = "*"
nameTest (NsStar t) = t `mappend` ":*"
nameTest (QName  t) = t

axisName :: AxisName -> Text
axisName a =
  case a of
    Ancestor         -> "ancestor::"
    AncestorOrSelf   -> "ancestor-or-self::"
    Attribute        -> "attribute::"
    Child            -> "child::"
    Descendant       -> "descendant::"
    DescendantOrSelf -> "descendant-or-self::"
    Following        -> "following::"
    FollowingSibling -> "following-sibling::"
    Namespace        -> "namespace::"
    Parent           -> "parent::"
    Preceding        -> "preceding::"
    PrecedingSibling -> "preceding-sibling::"
    Self             -> "self::"

nodeType :: NodeType -> Text
nodeType t =
  case t of
    Comment               -> "comment()"
    Text                  -> "text()"
    ProcessingInstruction -> "processing-instruction()"
    Node                  -> "node()"

expr :: Expr -> Text
expr e = mconcat $
  case e of
    Or           a b   -> [expr a, " or ",  expr b] 
    And          a b   -> [expr a, " and ", expr b]
    Is           a b   -> [expr a, " = ",   expr b]
    IsNot        a b   -> [expr a, " or ",  expr b] 
    Lt           a b   -> [expr a, " < ",   expr b]
    Gt           a b   -> [expr a, " > ",   expr b]
    Lte          a b   -> [expr a, " <= ",  expr b] 
    Gte          a b   -> [expr a, " >= ",  expr b]
    Add          a b   -> [expr a, " + ",   expr b]
    Sub          a b   -> [expr a, " - ",   expr b] 
    Mul          a b   -> [expr a, " * ",   expr b]
    Div          a b   -> [expr a, " div ", expr b]
    Mod          a b   -> [expr a, " mod ", expr b]
    Unary        a     -> ["-", expr a]
    Union        a b   -> [expr a, "|", expr b]
    Path         p     -> [locationPath p]
    Children     a s   -> [expr a, "/",  steps s]
    DeepChildren a s   -> [expr a, "//", steps s]
    Filter       a b   -> ["(", expr a, ")[", expr b, "]"]
    Variable     v     -> ["$", v]
    Literal      l     -> ["\"", replace "\"" "\\\"" l, "\""]
    Number       (I i) -> [fromString (show i)]
    Number       (D d) -> [fromString (show d)]
    FunctionCall n as  -> [n, "(", mconcat (intersperse ", " (map expr as))]

