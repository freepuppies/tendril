{
module Syntax.Parser.Lexer
  ( next
  , lexFile
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Data.Functor
import           Data.Source
import qualified Data.Text              as T
import qualified Data.ByteString.Lazy   as LBS
import           Syntax.Parser.Error
import           Syntax.Parser.Token
import           Syntax.Parser.Internal
}

%encoding "utf8"

$hwhite      = $white # [\n]

@escape      = \\ [\(\)\|\$\*\@\\]
@paramelem   = $printable # [\(\)\|] | @escape
@paramtext   = @paramelem+
@latexelem   = $printable # [\$] | $white 
@latex       = @latexelem+
@italicselem = $printable # [\*] | $white | @escape
@italics     = @italicselem+
@boldelem    = $printable # [\*] | $white | @escape
@bold        = @boldelem+
@textelem    = $printable # [\@\$\n] | $hwhite | @escape
@text        = @textelem+

tokens :-
  <0>                  "@Title"         { beginCommand TcTitle }
  <0>                  "@Date"          { beginCommand TcDate }
  <0>                  "@H1"            { beginCommand TcH1 }
  <0>                  "@H2"            { beginCommand TcH2 }
  <0>                  "@H3"            { beginCommand TcH3 }
  <0>                  "@H4"            { beginCommand TcH4 }
  <0>                  "@H5"            { beginCommand TcH5 }
  <0>                  "@H6"            { beginCommand TcH6 }
  <0>                  "@Link"          { beginCommand TcLink }
  <0>                  "@Reference"     { beginCommand TcReference }
  <0>                  "@Transclude"    { beginCommand TcTransclude }
  <0>                  "@Begin"         { beginCommand TcBeginVBlock }
  <0>                  "@End"           { beginCommand TcEndVBlock }
  <command>            "("              { commandParamsBegin }
  <command,params>     "("              { commandParamsInc }
  <command,params>     ")"              { commandParamsDec }
  <command,params>     "|"              { commandParamsSep }
  <command,params>     @paramtext       { textBufferAppendInput }

  <0>                  "@{"             { beginCBlock }
  <0>                  "@}"             { endCBlock }

  <0>                  "$$"             { beginLatex }
  <latex>              "$$"             { endLatex }
  <latex>              @latex           { textBufferAppendInput }
  <latex>              "$"              { textBufferAppend "$" }

  <0>                  "**"             { beginBold }
  <bold>               "**"             { endBold }
  <bold>               @bold            { textBufferAppendInput }
  <bold>               "*"              { textBufferAppend "*" }

  <0>                  "*"              { beginItalics }
  <italics>            "*"              { endItalics }
  <italics>            @italics         { textBufferAppendInput }

  <0>                  \n\n             { paragraphBreak }
  <0>                  \n               { lineBreak }
  <0>                  @text            { textLiteral }

{
beginCommand :: TokenClass -> AlexAction
beginCommand tc _ ss _ = do
  setStartCode command
  pure $ Token ss tc 

textBufferAppendInput,
  commandParamsBegin, 
  commandParamsInc, 
  commandParamsDec, 
  commandParamsSep, 
  beginCBlock, 
  endCBlock, 
  beginLatex, 
  endLatex, 
  beginBold, 
  endBold, 
  beginItalics, 
  endItalics, 
  paragraphBreak, 
  lineBreak, 
  textLiteral :: AlexAction
textBufferAppend :: T.Text -> AlexAction

textBufferAppendInput input ss len = do
  modifyTextBuffer ss (<> alexExcerpt input 0 len)
  next 

commandParamsBegin _ ss _ = do
  setStartCode params
  setParenDepth 1
  pure $ Token ss TcBeginParams

commandParamsInc _ ss _ = do
  modifyParenDepth (+ 1)
  modifyTextBuffer ss (<> "(")
  next

commandParamsDec _ ss _ = do
  modifyParenDepth $ \n -> n - 1 
  getParenDepth >>= \case
    0 -> do 
      setStartCode 0
      clearTextBuffer >>= \case
        Just (bufferSpan, text) -> do
          pushToken $ Token ss TcEndParams
          pure . Token bufferSpan $ TcText text
        Nothing -> pure $ Token ss TcEndParams
    _ -> modifyTextBuffer ss (<> ")") *> next

commandParamsSep AlexInput{..} ss _ = do 
  -- In the future there might be a request to ignore
  -- seperators within nested parentheses.
  -- Fix: Only add seperators if the parenthesis depth 
  -- is equal to 1, otherwise append a seperator to the 
  -- text buffer.
  clearTextBuffer >>= \case
    Just (bufferSpan, text) -> do
      pushToken $ Token ss TcParamsSep
      pure . Token bufferSpan $ TcText text
    Nothing -> failWith $ PeUnexpectedInput aSourcePos

beginCBlock _ ss _ = pure . Token ss $ TcBeginCBlock 
endCBlock _ ss _ = pure . Token ss $ TcEndCBlock 

beginLatex _ ss _ = setStartCode latex $> Token ss TcBeginLatex

endLatex AlexInput{..} ss _ = do
  setStartCode 0
  clearTextBuffer >>= \case
    Just (bufferSpan, text) -> do
      pushToken $ Token ss TcEndLatex 
      pure . Token bufferSpan $ TcText text
    Nothing -> failWith $ PeUnexpectedInput aSourcePos

beginBold _ ss _ = setStartCode bold $> Token ss TcBeginBold

endBold AlexInput{..} ss _ = do
  setStartCode 0
  clearTextBuffer >>= \case
    Just (bufferSpan, text) -> do
      pushToken $ Token ss TcEndBold 
      pure . Token bufferSpan $ TcText text
    Nothing -> failWith $ PeUnexpectedInput aSourcePos

beginItalics _ ss _ = setStartCode italics $> Token ss TcBeginItalics

endItalics AlexInput{..} ss _ = do
  setStartCode 0
  clearTextBuffer >>= \case
    Just (bufferSpan, text) -> do
      pushToken $ Token ss TcEndItalics 
      pure . Token bufferSpan $ TcText text
    Nothing -> failWith $ PeUnexpectedInput aSourcePos

paragraphBreak _ ss _ = pure . Token ss $ TcParagraphBreak 
lineBreak _ ss _ = pure . Token ss $ TcLineBreak 
textLiteral input ss len = pure . Token ss . TcText $ alexExcerpt input 0 len
textBufferAppend text _ ss _ = modifyTextBuffer ss (<> text) *> next

scan :: Parser Token
scan = do 
  input <- getAlexInput
  startCode <- getStartCode
  case alexScan input startCode of
    AlexEOF -> do
      getStartCode >>= \case
        0 -> getSourcePos <&> (\p -> Token (mkSourceSpan p p) TcEof)
        _ -> failWith . PeUnexpectedEof $ aSourcePos input
    AlexError input' -> 
      if alexEOF input'
        then failWith . PeUnexpectedEof $ aSourcePos input'
        else failWith . PeUnexpectedInput $ aSourcePos input'
    AlexSkip input' _ -> do
      setAlexInput input'
      next
    AlexToken input' len action -> do
      setAlexInput input'
      sp <- getSourcePos
      action input (mkSourceSpan (aSourcePos input') sp) len

next :: Parser Token
next = popToken >>= \case
  Just t -> pure t
  Nothing -> scan

lexFile :: MonadIO m => FilePath -> m (Either ParseError [Token])
lexFile path = do
  input <- liftIO $ LBS.readFile path
  let
    alexInput = AlexInput 0 (SourcePos path 1 1) input
    parserState = ParserState alexInput 0 [] 0 Nothing
  pure $ evalParser lexer parserState 
  -- TODO: This doesnt add the EOF token
  where lexer = unfoldWhileM (\t -> tokenClass t /= TcEof) next
}