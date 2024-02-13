module Syntax.Parser.Token 
  ( TokenClass(..)
  , Token(..)
  ) where

import qualified Data.Text   as T
import           Data.Source      (SourceSpan)

data TokenClass
  = TcTitle           -- ^ > @Title
  | TcDate            -- ^ > @Date
  | TcH1              -- ^ > @H1
  | TcH2              -- ^ > @H2
  | TcH3              -- ^ > @H3
  | TcH4              -- ^ > @H4
  | TcH5              -- ^ > @H5
  | TcH6              -- ^ > @H6
  | TcLink            -- ^ > @Link
  | TcReference       -- ^ > @Reference
  | TcTransclude      -- ^ > @Transclude
  | TcBeginVBlock     -- ^ > @Begin
  | TcEndVBlock       -- ^ > @End
  | TcBeginParams     -- ^ > (
  | TcEndParams       -- ^ > )
  | TcParamsSep       -- ^ > |
  | TcBeginCBlock     -- ^ > @{
  | TcEndCBlock       -- ^ > }@
  | TcBeginLatex      -- ^ > $$
  | TcEndLatex        -- ^ > $$
  | TcBeginBold       -- ^ > **
  | TcEndBold         -- ^ > **
  | TcBeginItalics    -- ^ > *
  | TcEndItalics      -- ^ > *
  | TcParagraphBreak  -- ^ > \n\n
  | TcLineBreak       -- ^ > \n
  | TcText !T.Text    -- ^ Text literal
  | TcEof             -- ^ End of file
  deriving (Show, Eq)

data Token = Token 
  { tokenSpan  :: !SourceSpan 
  , tokenClass :: !TokenClass }
  deriving Show 