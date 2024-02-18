module Syntax.Parser.Group
  ( ParseGroup (..)
  )
where

data ParseGroup
  = Parenthesis
  | Bold
  | Italics
  | Tex
  | Paragraph
  deriving (Show, Eq)
