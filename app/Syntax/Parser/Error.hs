module Syntax.Parser.Error
  ( ParseError (..)
  )
where

import Data.Source (SourcePos)
import Syntax.Parser.Group (ParseGroup)

data ParseError
  = UnexpectedEof !SourcePos
  | UnexpectedInput !SourcePos
  | UnterminatedGroup !ParseGroup
  deriving (Show)
