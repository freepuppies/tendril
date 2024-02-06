{
module Parser.Lexer
  ( next
  ) where

import Control.Monad.State
import Data.Functor
import Data.Source
import Parser.Error
import Parser.Token
import Parser.Internal
}

%encoding "utf8"

$hspace   = $white # [\n] 
$large    = [A-Z \xc0-\xd6 \xd8-\xde]
$small    = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha    = [$small $large]
$digit    = 0-9
$alphanum = [$alpha $digit]

@id     = $alphanum*
@escape = \\ [\(\)\{\}]
@pparam = $printable # [\(\\] | @escape
@cparam = $printable # [\{\\] | @escape
@text   = $printable # [\@] | \\ \@

tokens :-
  <0>       "@" @id          { beginCommand }
  <command> "(" @pparam* ")" { commandParameter }
  <command> "{" @cparam* "}" { commandParameter }
  <command> $hspace* \n      { endCommand }

  <0>       @text+ { text }

{
beginCommand, endCommand, commandParameter, text :: AlexAction Token

beginCommand input@AlexInput{..} len = do
  setStartCode command
  sourceSpan <- mkSourceSpan aSourcePos <$> getSourcePos
  pure . Token sourceSpan . TcBeginCommand $ alexExcerpt input 1 len

endCommand AlexInput{..} _ = do
  setStartCode 0
  sourceSpan <- mkSourceSpan aSourcePos <$> getSourcePos
  pure $ Token sourceSpan TcEndCommand

commandParameter input@AlexInput{..} len = do
  sourceSpan <- mkSourceSpan aSourcePos <$> getSourcePos
  pure . Token sourceSpan . TcText . alexExcerpt input 1 $ len - 1

text input@AlexInput{..} len = do
  sourceSpan <- mkSourceSpan aSourcePos <$> getSourcePos
  pure . Token sourceSpan . TcText $ alexExcerpt input 0 len

next :: Parser Token
next = do
  input <- gets pAlexInput 
  startCode <- gets pStartCode
  case alexScan input startCode of
    AlexEOF -> do
      gets pStartCode >>= \case
        0 -> getSourcePos <&> (\p -> Token (mkSourceSpan p p) TcEof)
        n | n == command -> do
          setStartCode 0
          getSourcePos <&> (\p -> Token (mkSourceSpan p p) TcEndCommand)
        _ -> failWith . PUnexpectedEof $ aSourcePos input
    AlexError input' -> 
      if alexEOF input'
        then failWith . PUnexpectedEof $ aSourcePos input'
        else failWith . PUnexpectedInput $ aSourcePos input'
    AlexSkip input' _ -> do
      setAlexInput input'
      next
    AlexToken input' len action -> do
      setAlexInput input'
      -- Invoke the action with the old input
      action input len
}