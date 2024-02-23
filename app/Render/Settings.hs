module Render.Settings
  ( RenderSettings (..)
  )
where

data RenderSettings = RenderSettings
  { temporaryDirectory :: FilePath
  , staticDirectory :: FilePath
  , resourceDirectory :: FilePath
  }
