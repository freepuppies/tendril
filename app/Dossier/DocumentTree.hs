module Dossier.DocumentTree
  ( DocumentTree (..)
  , insert
  , lookup
  , flatten
  )
where

import Control.Applicative ((<|>))
import Data.Composition ((.:))
import Data.HashMap.Strict qualified as H
import Data.Sequence (Seq (..))
import Data.Text qualified as T
import Dossier.Document qualified as D
import Dossier.Document.Config qualified as DC
import Prelude hiding (lookup)

data DocumentTree = DocumentNode
  { rootDocuments :: H.HashMap T.Text D.Document
  , subForests :: H.HashMap T.Text DocumentTree
  }
  deriving (Show)

instance Semigroup DocumentTree where
  (DocumentNode docs sub) <> (DocumentNode docs' sub') =
    DocumentNode root unionSub'
    where
      root = docs <> docs'
      unionSub = H.unionWith (<>) sub sub'
      intersectionSub = H.intersectionWith (\tree doc -> configTower (D.docConfig doc) tree) unionSub root
      unionSub' = intersectionSub `H.union` unionSub
      configTower cfg (DocumentNode docs'' sub'') =
        DocumentNode
          ((\d -> d{D.docConfig = D.docConfig d <> cfg}) <$> docs'')
          (configTower cfg <$> sub'')

instance Monoid DocumentTree where
  mempty = DocumentNode mempty mempty

insert :: D.Document -> DocumentTree -> DocumentTree
insert doc@(D.Document (D.Frontmatter (docPathInit :|> docPathTail) _ _) _ _) tree =
  foldr (DocumentNode mempty .: H.singleton) tailNode docPathInit <> tree
  where
    tailNode = DocumentNode (H.singleton docPathTail doc) mempty
insert (D.Document (D.Frontmatter Empty _ _) _ _) _ = error "Invalid document path"

lookup :: D.DocumentPath -> DocumentTree -> Maybe D.Document
lookup Empty _ = Nothing
lookup (x :<| Empty) (DocumentNode docs _) = H.lookup x docs
lookup (x :<| xs) (DocumentNode _ sub) = H.lookup x sub >>= lookup xs

flatten :: DocumentTree -> [D.Document]
flatten (DocumentNode docs sub) = H.elems docs <> foldMap flatten (H.elems sub)
