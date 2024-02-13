module Syntax.Parser.Error
  ( ParseError (..)
  ) where 

import Data.Source (SourcePos)

data ParseError
  = PeUnexpectedEof SourcePos
  | PeUnexpectedInput SourcePos
  | PeFailure SourcePos String
  deriving Show