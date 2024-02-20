module Syntax
  ( NodeClass (..)
  , Node (..)
  )
where

import Data.Sequence (Seq)
import Data.Source (SourceSpan)
import Data.Text qualified as T

data NodeClass
  = NcTitle
  | NcDate
  | NcH1
  | NcH2
  | NcH3
  | NcH4
  | NcH5
  | NcH6
  | NcLink
  | NcReference
  | NcTransclude
  | NcBold
  | NcItalics
  | NcTex
  | NcParagraph
  | NcText !T.Text
  deriving (Show)

data Node = Node
  { nodeSpan :: SourceSpan
  , nodeClass :: NodeClass
  , nodeChildren :: Seq Node
  }
  deriving (Show)
