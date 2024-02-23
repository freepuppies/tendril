module Dossier.Document
  ( DocumentPath
  , toFilePath
  , parsePath
  , ObjectClass (..)
  , Object (..)
  , Frontmatter (..)
  , Document (..)
  , path
  , title
  , setTitle
  , date
  , setDate
  , modifyBody
  )
where

import Data.Sequence as Seq (Seq (..), fromList)
import Data.Source (SourceSpan)
import Data.Text qualified as T
import Dossier.Document.Config (DocumentConfig)
import System.FilePath ((-<.>), (</>))

type DocumentPath = Seq T.Text

toFilePath :: DocumentPath -> FilePath
toFilePath = (-<.> "td") . foldr ((</>) . T.unpack) mempty

parsePath :: T.Text -> DocumentPath
parsePath =
  -- TODO: Remove this and properly handle
  -- contexual path parsing in the lexer
  Seq.fromList . T.split (== '/')

data Frontmatter = Frontmatter
  { fmPath :: DocumentPath
  , fmTitle :: Maybe T.Text
  , fmDate :: Maybe T.Text
  }
  deriving (Show)

data ObjectClass
  = OcH1
  | OcH2
  | OcH3
  | OcH4
  | OcH5
  | OcH6
  | OcLink !T.Text (Maybe T.Text)
  | OcReference Document
  | OcTransclude Document
  | OcBold
  | OcItalics
  | OcTex
  | OcParagraph
  | OcText !T.Text
  deriving (Show)

data Object = Object
  { objectSpan :: Maybe SourceSpan
  , objectClass :: ObjectClass
  , objectChildren :: Seq Object
  }
  deriving (Show)

data Document = Document
  { docFrontmatter :: Frontmatter
  , docConfig :: DocumentConfig
  , docBody :: Seq Object
  }
  deriving (Show)

path :: Document -> DocumentPath
path (Document (Frontmatter path' _ _) _ _) = path'

title :: Document -> Maybe T.Text
title (Document (Frontmatter _ title' _) _ _) = title'

setTitle :: T.Text -> Document -> Document
setTitle title' doc@(Document fm _ _) = doc{docFrontmatter = fm{fmTitle = Just title'}}

date :: Document -> Maybe T.Text
date (Document (Frontmatter _ _ date') _ _) = date'

setDate :: T.Text -> Document -> Document
setDate date' doc@(Document fm _ _) = doc{docFrontmatter = fm{fmDate = Just date'}}

modifyBody :: (Seq Object -> Seq Object) -> Document -> Document
modifyBody f doc@(Document _ _ body) = doc{docBody = f body}
