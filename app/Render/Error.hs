module Render.Error
  ( RenderError (..)
  )
where

import GHC.IO.Exception (ExitCode)

data RenderError
  = ReLatexError ExitCode String
  | ReDvipsError ExitCode String
  | ReConvertError ExitCode String
  deriving (Show)
