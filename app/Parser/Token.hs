module Parser.Token 
  ( TokenClass(..)
  , Token(..)
  ) where

import qualified Data.Text   as T
import           Data.Source      (SourceSpan)

data TokenClass
  = TcBeginCommand T.Text
  | TcEndCommand
  | TcText T.Text
  | TcEof
  deriving Show 

data Token = Token 
  { tokenSpan  :: !SourceSpan 
  , tokenClass :: !TokenClass }
  deriving Show 