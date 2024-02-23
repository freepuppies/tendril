module Dossier.Document.Config
  ( DocumentConfig (..)
  , readOrDefault
  )
where

import Control.Applicative ((<|>))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Dhall (input)
import Dhall.Marshal.Decode qualified as D
import Dhall.Marshal.Encode qualified as E
import Effectful
import Effectful.FileSystem
import Prelude hiding (readFile)

data DocumentConfig = DocumentConfig
  { dcCss :: Maybe T.Text
  , dcLatexDpi :: Maybe Int
  , dcLatexPreamble :: Maybe T.Text
  , dcLatexEnvironment :: Maybe T.Text
  }
  deriving (Show)

instance Semigroup DocumentConfig where
  (DocumentConfig css dpi preamble env) <> (DocumentConfig css' dpi' preamble' env') =
    DocumentConfig (css <|> css') (dpi <|> dpi') (preamble <|> preamble') (env <|> env')

instance Monoid DocumentConfig where
  mempty = DocumentConfig Nothing Nothing Nothing Nothing

decodeDocumentConfig :: D.Decoder DocumentConfig
decodeDocumentConfig = D.record decoder
  where
    decoder =
      DocumentConfig
        <$> D.field "css" (D.maybe D.strictText)
        <*> D.field "latexDPI" (D.maybe D.int)
        <*> D.field "latexPreamble" (D.maybe D.strictText)
        <*> D.field "latexEnvironment" (D.maybe D.strictText)

encodeDocumentConfig :: E.Encoder DocumentConfig
encodeDocumentConfig = E.recordEncoder encoder
  where
    encoder =
      adapt
        E.>$< E.encodeFieldWith "css" E.inject
          E.>*< E.encodeFieldWith "latexDPI" E.inject
          E.>*< E.encodeFieldWith "latexPreamble" E.inject
          E.>*< E.encodeFieldWith "latexEnvironment" E.inject
    adapt (DocumentConfig{..}) =
      (dcCss, (dcLatexDpi, (dcLatexPreamble, dcLatexEnvironment)))

-- | Attempts to read a @DocumentConfig@ from the provided path.
-- If the file doesn't exist, an empty @DocumentConfig@ will be returned.
readOrDefault :: (IOE :> es, FileSystem :> es) => FilePath -> Eff es DocumentConfig
readOrDefault path =
  doesFileExist path >>= \case
    True -> liftIO $ T.readFile path >>= input decodeDocumentConfig
    False -> pure mempty
