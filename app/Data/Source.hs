module Data.Source
  ( SourcePos (..)
  , SourceSpan (..)
  , mkSourceSpan
  )
where

import Control.Exception (assert)

data SourcePos = SourcePos
  { posOrigin :: FilePath
  , posLine :: !Int
  , posColumn :: !Int
  }
  deriving (Show)

data SourceSpan = SourceSpan
  { spanOrigin :: FilePath
  , spanBegin :: !(Int, Int)
  , spanEnd :: !(Int, Int)
  }
  deriving (Show)

instance Semigroup SourceSpan where
  (SourceSpan o begin _) <> (SourceSpan o' _ end) =
    assert (o == o') $ SourceSpan o begin end

mkSourceSpan :: SourcePos -> SourcePos -> SourceSpan
mkSourceSpan (SourcePos o bl bc) (SourcePos o' el ec) =
  assert (o == o' && ((bl == el && bc <= ec) || (bl < el))) $
    SourceSpan o (bl, bc) (el, ec)
