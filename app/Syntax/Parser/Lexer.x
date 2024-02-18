{
module Syntax.Parser.Lexer
  ( next
  , lex
  , lexFile
  )
where 

import Control.Monad
import Data.ByteString.Lazy qualified as LBS
import Data.Source
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Syntax.Parser.Group
import Syntax.Parser.Error
import Syntax.Parser.Internal
import Syntax.Parser.Token
import Prelude hiding (length)
}

%encoding "utf8"

@special = [\*\(\)\|\$\@]
@escape = \\ @special

@paramelem = $printable # [\(\)\|] | @escape
@param     = @paramelem+

@textelem = $printable # [\*\@\$\n] | @escape 
@text     = @textelem+

tokens :-
  <0> "@title"      { beginDirective TcTitle }
  <0> "@date"       { beginDirective TcDate }
  <0> "@h1"         { beginDirective TcH1 }
  <0> "@h2"         { beginDirective TcH2 }
  <0> "@h3"         { beginDirective TcH3 }
  <0> "@h4"         { beginDirective TcH4 }
  <0> "@h5"         { beginDirective TcH5 }
  <0> "@h6"         { beginDirective TcH6 }
  <0> "@link"       { beginDirective TcLink }
  <0> "@reference"  { beginDirective TcReference }
  <0> "@transclude" { beginDirective TcTransclude }
  
  <directive> "(" { beginParams }
  
  <directive,params> "("    { paramsInc }
  <directive,params> ")"    { paramsDec }
  <directive,params> "|"    { paramsSep }
  <directive,params> @param { bufferAppendInput }

  <0> "$$"   { group Tex TcBeginTex TcEndTex }
  <0> "**"   { group Bold TcBeginBold TcEndBold }
  <0> "*"    { group Italics TcBeginItalics TcEndItalics }
  <0> \n \n+ { paragraphGroup }
  
  <0> \n     { linebreak }

  <0> @text { textLiteral }

{
bufferAppendInput :: Lexlet
bufferAppendInput alex tokenSpan length = do 
  let excerpt = alexExcerpt alex 0 length 
  modifyTextBuffer tokenSpan (<> excerpt)
  next

beginDirective :: TokenClass -> Lexlet
beginDirective tokenClass _ tokenSpan _ = do 
  setStartCode directive 
  pure $ Token tokenSpan tokenClass

beginParams, paramsInc, paramsDec, paramsSep :: Lexlet
beginParams _ tokenSpan _ = do
  setStartCode params
  pure $ Token tokenSpan TcBeginParameters

paramsInc _ tokenSpan _ = do
  pushGroup Parenthesis
  modifyTextBuffer tokenSpan (<> "(")
  next

paramsDec _ tokenSpan _ = do
  peekGroup >>= \case
    Just Parenthesis -> do
      _ <- popGroup
      modifyTextBuffer tokenSpan (<> ")")
      next
    _ -> do
      setStartCode 0
      clearTextBuffer >>= \case
        Just (bufferSpan, bufferText) -> do
          pushToken $ Token tokenSpan TcEndParameters
          pure . Token bufferSpan $ TcText bufferText
        Nothing -> pure $ Token tokenSpan TcEndParameters

paramsSep AlexInput{..} tokenSpan _ = 
  clearTextBuffer >>= \case 
    Just (bufferSpan, bufferText) -> do
      pushToken $ Token tokenSpan TcParameterSeparator
      pure . Token bufferSpan $ TcText bufferText
    Nothing -> throwError $ UnexpectedInput alexSourcePos

linebreak :: Lexlet
linebreak _ tokenSpan _ = pure $ Token tokenSpan TcLinebreak 

textLiteral :: Lexlet
textLiteral alex tokenSpan length = 
  pure . Token tokenSpan . TcText $ alexExcerpt alex 0 length

group :: ParseGroup -> TokenClass -> TokenClass -> Lexlet
group parseGroup beginClass endClass _ tokenSpan _ = 
  peekGroup >>= \case
    Just pg | pg == parseGroup -> do
      _ <- popGroup
      pure $ Token tokenSpan endClass 
    _ -> do
      pushGroup parseGroup
      pure $ Token tokenSpan beginClass 

paragraphGroup :: Lexlet
paragraphGroup alex tokenSpan _ = 
  peekGroup >>= \case
    Just Paragraph -> do
      alexEof <$> get >>= \case 
        True -> void popGroup
        False -> pushToken $ Token tokenSpan TcBeginParagraph
      pure $ Token tokenSpan TcEndParagraph
    _ -> do
      pushGroup Paragraph
      pure $ Token tokenSpan TcBeginParagraph

scan :: Parse es => Eff es Token
scan = do
  alex <- get
  code <- gets parseStartCode
  case alexScan alex code of
    AlexEOF -> do 
      ((,) <$> popGroup <*> gets parseStartCode) >>= \case
        (Just Paragraph, 0) -> 
          pure $ Token (mkSourceSpan (alexSourcePos alex) (alexSourcePos alex)) TcEndParagraph
        (Just pg, _) -> throwError $ UnterminatedGroup pg
        (Nothing, 0) -> 
          pure $ Token (mkSourceSpan (alexSourcePos alex) (alexSourcePos alex)) TcEof
        (Nothing, _) -> throwError . UnexpectedEof $ alexSourcePos alex
    AlexError alex' ->
      if alexEof alex' 
        then throwError . UnexpectedEof $ alexSourcePos alex'
        else throwError . UnexpectedInput $ alexSourcePos alex'
    AlexSkip alex' _ -> put alex' *> next
    AlexToken alex' length lexlet -> do
      tokenSpan <- mkSourceSpan 
        <$> gets alexSourcePos <* put alex' 
        <*> gets alexSourcePos
      inject $ lexlet alex tokenSpan length

next :: Parse es => Eff es Token
next = popToken >>= \case
  Just token -> pure token 
  Nothing -> scan

lexFile 
  :: (Error ParseError :> es, IOE :> es) 
  => FilePath 
  -> Eff es [Token]
lexFile path = do
  input <- liftIO $ LBS.readFile path
  let
    alexInput = AlexInput 0 (SourcePos path 1 1) input
    parseState = ParseState 0 mempty mempty Nothing
  evalState parseState $ evalState alexInput go
  where
    go = next >>= \case 
      eof@(Token _ TcEof) -> pure [eof]
      token@_ -> (token :) <$> go
 }