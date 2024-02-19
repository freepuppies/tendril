module Syntax.Parser.Token
  ( TokenClass (..)
  , Token (..)
  )
where

import Data.Source (SourceSpan)
import Data.Text qualified as T

data TokenClass
  = TcTitle
  | TcDate
  | TcH1
  | TcH2
  | TcH3
  | TcH4
  | TcH5
  | TcH6
  | TcLink
  | TcReference
  | TcTransclude
  | TcBeginParameters
  | TcParameterSeparator
  | TcEndParameters
  | TcBeginBold
  | TcEndBold
  | TcBeginItalics
  | TcEndItalics
  | TcBeginTex
  | TcEndTex
  | TcBeginParagraphs
  | TcParagraphSeparator
  | TcEndParagraphs
  | TcLinebreak
  | TcText !T.Text
  | TcEof
  deriving (Show, Eq)

data Token = Token
  { tokenSpan :: !SourceSpan
  , tokenClass :: !TokenClass
  }
  deriving (Show)
