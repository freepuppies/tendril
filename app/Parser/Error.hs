module Parser.Error
  ( ParseError (..)
  ) where 

import Data.Source (SourcePos)

data ParseError
  = PUnexpectedEof SourcePos
  | PUnexpectedInput SourcePos
  | PFailure SourcePos String
  deriving Show