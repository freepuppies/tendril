module Main where

import Data.Sequence (Seq (..))
import Dossier (runDossier)
import Effectful
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.Process
import Effectful.Reader.Static
import Effectful.Temporary
import Eval (eval)
import Eval.Error (EvalError)
import Render (render)
import Render.Error (RenderError)
import Render.Settings (RenderSettings (..))
import Syntax.Parser.Error (ParseError)

main :: IO ()
main = do
  result <- run do
    _ <- eval ("index" :<| Empty)
    render

  case result of 
    Left (_, e) -> print e
    Right (Left (_, e)) -> print e
    Right (Right (Left (_, e))) -> print e 
    Right (Right (Right (_, _))) -> putStrLn "done!"
  
  where
    run = runEff 
      . runFileSystem 
      . runTemporary 
      . runProcess 
      . runError @ParseError 
      . runError @EvalError 
      . runError @RenderError 
      . runDossier mempty 
      . runReader (RenderSettings ".tendril" "static" "resources")