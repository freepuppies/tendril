module Eval
  ( eval
  )
where

import Data.Foldable (traverse_)
import Data.Functor (($>), (<&>))
import Data.Sequence as Seq (Seq (..), singleton, (|>))
import Dossier
import Dossier.Document qualified as D
import Dossier.Document.Config qualified as DC
import Effectful
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.State.Static.Local
import Eval.Error (EvalError (..))
import Syntax (Node (..), NodeClass (..))
import Syntax.Parser (parseFile)
import Syntax.Parser.Error (ParseError)
import System.FilePath ((-<.>))

evalNode
  :: ( IOE :> es
     , FileSystem :> es
     , Dossier :> es
     , Error EvalError :> es
     , Error ParseError :> es
     , State D.Document :> es
     )
  => Node
  -> Eff es D.Object
evalNode (Node sourceSpan class_ children) =
  case (class_, children) of
    -- Heading
    (NcH1, _) -> heading D.OcH1 children
    (NcH2, _) -> heading D.OcH2 children
    (NcH3, _) -> heading D.OcH3 children
    (NcH4, _) -> heading D.OcH4 children
    (NcH5, _) -> heading D.OcH5 children
    (NcH6, _) -> heading D.OcH6 children
    -- Link
    (NcLink, Node _ (NcText url) Empty :<| Empty) ->
      pure $ D.Object (Just sourceSpan) (D.OcLink url Nothing) Empty
    (NcLink, Node _ (NcText url) Empty :<| Node _ (NcText title) Empty :<| Empty) ->
      pure $ D.Object (Just sourceSpan) (D.OcLink url $ Just title) Empty
    -- Reference
    (NcReference, Node _ (NcText path) Empty :<| Empty) -> do
      doc <- eval $ D.parsePath path
      pure $ D.Object (Just sourceSpan) (D.OcReference doc) Empty
    -- Transclude
    (NcTransclude, Node _ (NcText path) Empty :<| Empty) -> do
      doc <- eval $ D.parsePath path
      pure $ D.Object (Just sourceSpan) (D.OcTransclude doc) Empty
    -- Scripts
    (NcBold, _) -> container D.OcBold
    (NcItalics, _) -> container D.OcItalics
    (NcTex, _) -> container D.OcTex
    -- Paragraph
    (NcParagraph, _) -> container D.OcParagraph
    -- Text
    (NcText text, _) -> pure $ D.Object (Just sourceSpan) (D.OcText text) Empty
    _ -> error "Malformed node"
  where
    heading f (Node textSpan (NcText text) Empty :<| Empty) =
      pure
        . D.Object (Just sourceSpan) f
        . Seq.singleton
        $ D.Object (Just textSpan) (D.OcText text) Empty
    heading _ _ = error "Malformed heading"
    container f = traverse evalNode children <&> D.Object (Just sourceSpan) f

execNode
  :: ( IOE :> es
     , FileSystem :> es
     , Dossier :> es
     , Error EvalError :> es
     , Error ParseError :> es
     , State D.Document :> es
     )
  => Node
  -> Eff es ()
execNode node@(Node sourceSpan class_ children) =
  case (class_, children) of
    -- Title
    (NcTitle, Node _ (NcText title) Empty :<| Empty) ->
      gets D.title >>= \case
        Just _ -> throwError $ EeDuplicate sourceSpan NcTitle
        Nothing -> modify $ D.setTitle title
    -- Date
    (NcDate, Node _ (NcText date) Empty :<| Empty) ->
      gets D.date >>= \case
        Just _ -> throwError $ EeDuplicate sourceSpan NcDate
        Nothing -> modify $ D.setDate date
    -- Expression
    _ -> evalNode node >>= \o -> modify $ D.modifyBody (|> o)

eval
  :: ( IOE :> es
     , FileSystem :> es
     , Dossier :> es
     , Error EvalError :> es
     , Error ParseError :> es
     )
  => D.DocumentPath
  -> Eff es D.Document
eval docPath =
  query docPath >>= \case
    Just doc -> pure doc
    Nothing -> do
      let
        fullDocPath = D.toFilePath docPath
        fullConfigPath = fullDocPath -<.> "dhall"
      nodes <- parseFile fullDocPath
      docConfig <- DC.readOrDefault fullConfigPath
      let
        inFrontmatter = D.Frontmatter docPath Nothing Nothing
        inDoc = D.Document inFrontmatter docConfig mempty
      outDoc <- execState inDoc $ traverse_ execNode nodes
      include outDoc $> outDoc
