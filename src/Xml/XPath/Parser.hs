{-# LANGUAGE OverloadedStrings #-}
module Xml.XPath.Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Attoparsec.Text
import Data.Attoparsec.Expr
import Data.Text (Text)
import Prelude hiding (takeWhile)

import qualified Data.Text as T

import Xml.XPath.Types

xpath :: Parser XPath
xpath = XPath <$> expr <* endOfInput

locationPath :: Parser LocationPath
locationPath = Absolute <$> absoluteLocationPath
           <|> Relative <$> relativeLocationPath

relativeLocationPath :: Parser [Step]
relativeLocationPath = (++)
                   <$> (step `sepBy1` token "/")
                   <*> option [] abbreviatedRelativeLocationPath

abbreviatedRelativeLocationPath :: Parser [Step]
abbreviatedRelativeLocationPath = (descendantOrSelf :) <$> (token "//" *> (pure <$> step))

absoluteLocationPath :: Parser [Step]
absoluteLocationPath = abbreviatedAbsoluteLocationPath
                   <|> token "/" *> option [] relativeLocationPath

abbreviatedAbsoluteLocationPath :: Parser [Step]
abbreviatedAbsoluteLocationPath = (descendantOrSelf :) <$> (token "//" *> relativeLocationPath)

step :: Parser Step
step = Step <$> axisSpecifier <*> nodeTest <*> many predicate
   <|> abbreviatedStep

abbreviatedStep :: Parser Step
abbreviatedStep = parent <$ token ".."
              <|> self   <$ token "."

descendantOrSelf :: Step
descendantOrSelf = Step (NamedAxis DescendantOrSelf) (NodeType Node) []

parent :: Step
parent = Step (NamedAxis Parent) (NodeType Node) []

self :: Step
self = Step (NamedAxis Self) (NodeType Node) []

axisSpecifier :: Parser AxisSpecifier
axisSpecifier = NamedAxis <$> axisName
            <|> abbreviatedAxisSpecifier

axisName :: Parser AxisName
axisName = p Ancestor         "ancestor"
       <|> p AncestorOrSelf   "ancestor-or-self"
       <|> p Attribute        "attribute"
       <|> p Child            "child"
       <|> p Descendant       "descendant"
       <|> p DescendantOrSelf "descendant-or-self"
       <|> p Following        "following"
       <|> p FollowingSibling "following-sibling"
       <|> p Namespace        "namespace"
       <|> p Parent           "parent"
       <|> p Preceding        "preceding"
       <|> p PrecedingSibling "preceding-sibling"
       <|> p Self             "self"
  where p f t = f <$ (token t <* token "::")

abbreviatedAxisSpecifier :: Parser AxisSpecifier
abbreviatedAxisSpecifier = option NodeAxis (AttrAxis <$ token "@")

nodeTest :: Parser NodeTest
nodeTest = NodeType <$> nodeType
       <|> PiTest   <$> processingInstruction
       <|> NameTest <$> nameTest

nameTest :: Parser NameTest
nameTest = Star   <$  token "*"
       <|> NsStar <$> free unqualified <* token ":*"
       <|> QName  <$> free qualified

nodeType :: Parser NodeType
nodeType = p Comment               "comment"
       <|> p Text                  "text"
       <|> p ProcessingInstruction "processing-instruction"
       <|> p Node                  "node"
  where p f t = f <$ token t <* token "(" <* token ")"

processingInstruction :: Parser Text
processingInstruction = PiTest
                    <$> token "processing-instruction"
                    <*  token "(" *> literal <* token ")"

literal :: Parser Text
literal = free (char '"'  *> takeTill (=='"' ) <* char '"')
      <|> free (char '\'' *> takeTill (=='\'') <* char '\'')

predicate :: Parser Expr
predicate = token "[" *> expr <* token "]"

expr :: Parser Expr
expr = buildExpressionParser table primaryExpr

primaryExpr :: Parser Expr
primaryExpr = token "(" *> expr <* token ")"
          <|> Literal  <$> literal
          <|> Number   <$> free number
          <|> Variable <$> variableReference
          <|> functionCall
          <|> Path     <$> locationPath

variableReference :: Parser Text
variableReference = free (char '$' *> qualified)

functionCall :: Parser Expr
functionCall = FunctionCall
           <$> functionName
           <*> (token "(" *> (expr `sepBy` token ",") <* token ")")

functionName :: Parser Text
functionName = join (valid <$> free qualified)
  where valid n = if n `elem` ["comment", "text", "processing-instruction", "node"]
                  then empty
                  else return n

table :: [[Operator Text Expr]]
table =
  [ [ Infix (DeepChildren <$ token "//" ) AssocLeft ]
  , [ Infix (Children     <$ token "/"  ) AssocLeft ]
  , [ Infix (Union        <$ token "|"  ) AssocLeft ]
  , [ Infix (Mod          <$ token "mod") AssocLeft ]
  , [ Infix (Div          <$ token "div") AssocLeft ]
  , [ Infix (Mul          <$ token "*"  ) AssocLeft ]
  , [ Infix (Sub          <$ token "-"  ) AssocLeft ]
  , [ Infix (Add          <$ token "+"  ) AssocLeft ]
  , [ Infix (Gte          <$ token ">=" ) AssocLeft ]
  , [ Infix (Lte          <$ token "<=" ) AssocLeft ]
  , [ Infix (Gt           <$ token ">"  ) AssocLeft ]
  , [ Infix (Lt           <$ token "<"  ) AssocLeft ]
  , [ Infix (IsNot        <$ token "!=" ) AssocLeft ]
  , [ Infix (Is           <$ token "="  ) AssocLeft ]
  , [ Infix (And          <$ token "and") AssocLeft ]
  , [ Infix (Or           <$ token "or" ) AssocLeft ]
  ]

unqualified :: Parser Text
unqualified = T.cons
          <$> satisfy (\d -> isAlpha d || d == '_')
          <*> takeWhile (\d -> d == '-' || isAlpha d || isDigit d)

qualified :: Parser Text
qualified = T.append
        <$> unqualified
        <*> (T.cons <$> char ':' <*> unqualified <|> pure "")

free :: Parser a -> Parser a
free = (<* many space)

token :: Text -> Parser Text
token = free . stringCI

