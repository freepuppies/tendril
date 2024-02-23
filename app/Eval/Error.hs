module Eval.Error
  ( EvalError (..)
  )
where

import Data.Source (SourceSpan)
import Syntax (NodeClass)

data EvalError
  = EeDuplicate !SourceSpan NodeClass
  deriving (Show)
