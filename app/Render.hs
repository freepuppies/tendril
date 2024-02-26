module Render
  ( render
  )
where

import Data.Foldable (traverse_, toList)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..))
import Data.Text qualified as T
import Dossier (Dossier, documents)
import Dossier.Document qualified as D
import Dossier.Document.Config qualified as DC
import Effectful
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.FileSystem.IO
import Effectful.FileSystem.IO.ByteString.Lazy
import Effectful.Process
import Effectful.Reader.Static
import Effectful.Temporary
import Render.Error (RenderError)
import Render.Latex (renderLatexFormula)
import Render.Settings (RenderSettings (RenderSettings))
import System.FilePath (dropExtension, takeFileName, (-<.>), (</>), takeDirectory)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

objectHtml
  :: ( Reader RenderSettings :> es
     , Reader DC.DocumentConfig :> es
     , Process :> es
     , FileSystem :> es
     , Temporary :> es
     , Error RenderError :> es
     )
  => D.Object
  -> Eff es H.Html
objectHtml (D.Object sourceSpan class_ children) =
  case (class_, children) of
    -- Heading
    (D.OcH1, _) -> heading H.h1 children
    (D.OcH2, _) -> heading H.h2 children
    (D.OcH3, _) -> heading H.h3 children
    (D.OcH4, _) -> heading H.h4 children
    (D.OcH5, _) -> heading H.h5 children
    (D.OcH6, _) -> heading H.h6 children
    -- Link
    (D.OcLink url title, Empty) ->
      pure $ H.a H.! A.href (H.textValue url) $ H.text $ fromMaybe url title
    -- Reference
    (D.OcReference doc, Empty) -> do
      -- TODO: This is gross, find another way
      let
        docPath = D.path doc
        docUrl = ("/" <>) . (-<.> "html") . T.unpack . T.intercalate "/" $ toList docPath
      pure $ H.a H.! A.href (H.stringValue docUrl) $ H.string . dropExtension $ takeFileName docUrl 
    -- Scripts
    (D.OcBold, _) -> traverse objectHtml children <&> H.b . sequence_
    (D.OcItalics, _) -> traverse objectHtml children <&> H.i . sequence_
    (D.OcTex, D.Object _ (D.OcText formula) Empty :<| Empty) -> do
      -- TODO: This is gross, find another way
      RenderSettings _ _ resourceDir <- ask
      pngPath <- renderLatexFormula formula
      pure $ H.img H.! A.src (H.stringValue $ "/" <> resourceDir <> "/" <> takeFileName pngPath) H.! A.class_ "tex"
    -- Paragraph
    (D.OcParagraph, _) -> traverse objectHtml children <&> H.p . sequence_
    -- Text
    (D.OcText text, Empty) -> pure $ H.text text
    _ -> error "Malformed object"
  where
    heading f (D.Object _ (D.OcText text) Empty :<| Empty) = pure . f $ H.text text
    heading _ _ = error "Malformed heading object"

renderDocument
  :: ( Reader RenderSettings :> es
     , Dossier :> es
     , Process :> es
     , FileSystem :> es
     , Temporary :> es
     , Error RenderError :> es
     )
  => D.Document
  -> Eff es ()
renderDocument (D.Document (D.Frontmatter docPath title date) config body) = do
  RenderSettings _ staticDir _ <- ask
  objects <- runReader config $ traverse objectHtml body
  let html = renderHtml . H.docTypeHtml $ do
        H.head $ do
          maybe (pure ()) (H.title . H.text) title
          maybe (pure ()) (H.style . H.text) $ DC.dcCss config
        H.body $ do
          sequence_ objects
      htmlPath = staticDir </> D.toFilePath docPath -<.> "html"
  createDirectoryIfMissing True $ takeDirectory htmlPath
  withFile htmlPath WriteMode $ flip hPutStrLn html

render
  :: ( Reader RenderSettings :> es
     , Dossier :> es
     , Process :> es
     , FileSystem :> es
     , Temporary :> es
     , Error RenderError :> es
     )
  => Eff es ()
render = documents >>= traverse_ renderDocument
