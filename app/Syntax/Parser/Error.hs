module Syntax.Parser.Error
  ( ParseError (..)
  )
where

import Data.Source (SourcePos)
import Syntax.Parser.Group (ParseGroup)
import Syntax.Parser.Token (Token)

data ParseError
  = UnexpectedEof !SourcePos
  | UnexpectedInput !SourcePos
  | UnterminatedGroup !ParseGroup
  | UnexpectedToken !Token [String]
  deriving (Show)
