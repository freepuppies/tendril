module Render.Latex
  ( renderLatexFormula
  )
where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Dossier.Document.Config (DocumentConfig (..))
import Effectful
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.FileSystem.IO
import Effectful.FileSystem.IO.ByteString
import Effectful.Process
import Effectful.Reader.Static
import Effectful.Temporary
import Render.Error (RenderError (..))
import Render.Settings (RenderSettings (..))
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (-<.>), (</>))

renderLatexFormula
  :: ( Reader RenderSettings :> es
     , Reader DocumentConfig :> es
     , Process :> es
     , FileSystem :> es
     , Temporary :> es
     , Error RenderError :> es
     )
  => T.Text
  -> Eff es FilePath
renderLatexFormula formula = do
  RenderSettings tempDir staticDir resDir <- ask
  DocumentConfig _ dpi preamble environment <- ask

  workingDir <- getCurrentDirectory
  destinationDir <- makeAbsolute $ staticDir </> resDir

  createDirectoryIfMissing True destinationDir
  createDirectoryIfMissing True tempDir
  setCurrentDirectory tempDir

  let
    preamble' = fromMaybe mempty preamble
    environmentBegin = maybe mempty (\e -> "\\begin{" <> e <> "}\n") environment
    environmentEnd = maybe mempty (\e -> "\\end{" <> e <> "}\n") environment
    document =
      mconcat
        [ "\\nonstopmode\n"
        , "\\documentclass[12pt]{article}\n"
        , "\\pagestyle{empty}\n"
        , preamble'
        , "\\begin{document}\n"
        , environmentBegin
        , formula
        , environmentEnd
        , "\\end{document}"
        ]

  outPngPath <- withTempFile "." "formula.tex" $ \path handle -> do
    hPutStr handle $ T.encodeUtf8 document
    hClose handle

    let
      outPngPath = destinationDir </> takeFileName path -<.> "png"
      latexArgs = [path]
      dvipsArgs = ["-q", "-E", "-o", path -<.> "ps", path -<.> "dvi"]
      convertArgs =
        [ "-density"
        , show (fromMaybe 200 dpi)
        , "-bordercolor"
        , "none"
        , "-border"
        , "1x1"
        , "-background"
        , "none"
        , "-trim"
        , "-splice"
        , "1x0"
        , path -<.> "ps"
        , outPngPath
        ]
      process command args f = do
        (code, out, err) <- readProcessWithExitCode command args ""
        -- TODO: Write to stdout if verbose
        when (code /= ExitSuccess) . throwError $ f code err

    process "latex" latexArgs ReLatexError
    process "dvips" dvipsArgs ReDvipsError
    process "convert" convertArgs ReConvertError

    pure outPngPath

  setCurrentDirectory workingDir
  pure outPngPath
