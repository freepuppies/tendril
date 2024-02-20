module Document
  ( DocumentPath
  , toFilePath
  , parseDocumentPath
  , ObjectClass (..)
  , Object (..)
  , Frontmatter (..)
  , Document (..)
  , documentPath
  , documentTitle
  , setDocumentTitle
  , documentDate
  , setDocumentDate
  , modifyDocumentBody
  , DocumentTree (..)
  , documentTreeInsert
  , documentTreeLookup
  )
where

import Data.Composition ((.:))
import Data.HashMap.Strict qualified as H
import Data.Sequence as Seq (Seq (..), fromList)
import Data.Source (SourceSpan)
import Data.Text qualified as T
import Document.Config (DocumentConfig)
import System.FilePath ((-<.>), (</>))

type DocumentPath = Seq T.Text

toFilePath :: DocumentPath -> FilePath
toFilePath = (-<.> "td") . foldr ((</>) . T.unpack) mempty

parseDocumentPath :: T.Text -> DocumentPath
parseDocumentPath path =
  -- TODO: Remove this and properly handle contexual path parsing in the lexer
  Seq.fromList $ T.split (== '/') path

data Frontmatter = Frontmatter
  { fmPath :: DocumentPath
  , fmTitle :: Maybe T.Text
  , fmDate :: Maybe T.Text
  }
  deriving (Show)

data ObjectClass
  = OcDocument Document
  | OcH1
  | OcH2
  | OcH3
  | OcH4
  | OcH5
  | OcH6
  | OcLink
  | OcReference
  | OcTransclude
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

documentPath :: Document -> DocumentPath
documentPath (Document (Frontmatter path _ _) _ _) = path

documentTitle :: Document -> Maybe T.Text
documentTitle (Document (Frontmatter _ title _) _ _) = title

setDocumentTitle :: T.Text -> Document -> Document
setDocumentTitle title doc@(Document fm _ _) = doc{docFrontmatter = fm{fmTitle = Just title}}

documentDate :: Document -> Maybe T.Text
documentDate (Document (Frontmatter _ _ date) _ _) = date

setDocumentDate :: T.Text -> Document -> Document
setDocumentDate date doc@(Document fm _ _) = doc{docFrontmatter = fm{fmDate = Just date}}

modifyDocumentBody :: (Seq Object -> Seq Object) -> Document -> Document
modifyDocumentBody f doc@(Document _ _ body) = doc{docBody = f body}

data DocumentTree = DocumentNode
  { rootDocuments :: H.HashMap T.Text Document
  , subForests :: H.HashMap T.Text DocumentTree
  }
  deriving (Show)

instance Semigroup DocumentTree where
  (DocumentNode docs sub) <> (DocumentNode docs' sub') = DocumentNode (docs <> docs') (H.unionWith (<>) sub sub')

instance Monoid DocumentTree where
  mempty = DocumentNode mempty mempty

documentTreeInsert :: Document -> DocumentTree -> DocumentTree
documentTreeInsert doc@(Document (Frontmatter (docPathInit :|> docPathTail) _ _) _ _) tree =
  foldr (DocumentNode mempty .: H.singleton) (DocumentNode (H.singleton docPathTail doc) mempty) docPathInit <> tree
documentTreeInsert (Document (Frontmatter Empty _ _) _ _) _ = error "Invalid document path"

documentTreeLookup :: DocumentPath -> DocumentTree -> Maybe Document
documentTreeLookup Empty _ = Nothing
documentTreeLookup (x :<| Empty) (DocumentNode docs _) = H.lookup x docs
documentTreeLookup (x :<| xs) (DocumentNode _ sub) = H.lookup x sub >>= documentTreeLookup xs
