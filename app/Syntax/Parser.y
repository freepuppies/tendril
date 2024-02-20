{
module Syntax.Parser
  ( parseFile
  )
where 

import Control.Monad
import Data.ByteString.Lazy qualified as LBS
import Data.Sequence
import Data.Source
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Syntax
import Syntax.Parser.Error
import Syntax.Parser.Internal
import Syntax.Parser.Token
import Syntax.Parser.Lexer
}

%name parseDocument document 

%tokentype { Token }
%monad { Parser } { (>>=) } { return }
%lexer { (next >>=) } { Token _ TcEof }
%error { parseError }
%errorhandlertype explist

%token
  title              { Token _ TcTitle }
  date               { Token _ TcDate }
  h1                 { Token _ TcH1 }
  h2                 { Token _ TcH2 }
  h3                 { Token _ TcH3 }
  h4                 { Token _ TcH4 }
  h5                 { Token _ TcH5 }
  h6                 { Token _ TcH6 }
  link               { Token _ TcLink }
  reference          { Token _ TcReference }
  transclude         { Token _ TcTransclude }
  '('                { Token _ TcBeginParameters }
  '|'                { Token _ TcParameterSeparator }
  ')'                { Token _ TcEndParameters }
  beginBold          { Token _ TcBeginBold }
  endBold            { Token _ TcEndBold }
  beginItalics       { Token _ TcBeginItalics }
  endItalics         { Token _ TcEndItalics }
  beginTex           { Token _ TcBeginTex }
  endTex             { Token _ TcEndTex }
  beginParagraphs    { Token _ TcBeginParagraphs }
  paragraphSeparator { Token _ TcParagraphSeparator }
  endParagraphs      { Token _ TcEndParagraphs }
  linebreak          { Token _ TcLinebreak }
  textLiteral        { Token _ (TcText _) }

%%

opt(p) : p { Just $1 } 
       |   { Nothing }

seq1(p) : p seq1(p) { $1 <| $2 }
        | p         { singleton $1 }

seq(p) : seq1(p) { $1 }
       |         { mempty }

text : textLiteral { (\(Token tokenSpan (TcText t)) -> Node tokenSpan (NcText t) mempty) $1 }

command1(p) : p '(' text ')' { \nc -> Node (tokenSpan $1 <> tokenSpan $4) nc (singleton $3) }

command2(p) : p '(' text '|' text ')' { \nc -> Node (tokenSpan $1 <> tokenSpan $6) nc ($3 <| $5 <| mempty) }

command : command1(title)      { $1 NcTitle }
        | command1(date)       { $1 NcDate }
        | command1(h1)         { $1 NcH1 }
        | command1(h2)         { $1 NcH2 }
        | command1(h3)         { $1 NcH3 }
        | command1(h4)         { $1 NcH4 }
        | command1(h5)         { $1 NcH5 }
        | command1(h6)         { $1 NcH6 }
        | command2(link)       { $1 NcLink }
        | command1(link)       { $1 NcLink }
        | command2(reference)  { $1 NcReference }
        | command1(reference)  { $1 NcReference }
        | command2(transclude) { $1 NcTransclude }
        | command1(transclude) { $1 NcTransclude }

bold : beginBold scripts1 endBold { Node (tokenSpan $1 <> tokenSpan $3) NcBold $2 }

italics : beginItalics scripts1 endItalics { Node (tokenSpan $1 <> tokenSpan $3) NcItalics $2 }

tex : beginTex scripts1 endTex { Node (tokenSpan $1 <> tokenSpan $3) NcTex $2 }

script : bold    { $1 }
       | italics { $1 }
       | tex     { $1 }
       | text    { $1 }

scripts1 : seq1(script) { $1 }

paragraphItem : script               { $1 }
              | command2(link)       { $1 NcLink }
              | command1(link)       { $1 NcLink }
              | command2(reference)  { $1 NcReference }
              | command1(reference)  { $1 NcReference }

paragraphItems : seq(paragraphItem) { $1 }

paragraphsTail : paragraphItems opt(paragraphSeparator) endParagraphs { \beginSpan -> singleton (Node (beginSpan <> tokenSpan $3) NcParagraph $1) }
               | paragraphItems paragraphSeparator paragraphsTail     { \beginSpan -> Node (beginSpan <> tokenSpan $2) NcParagraph $1 <| $3 beginSpan }

paragraphs : beginParagraphs paragraphsTail { $2 (tokenSpan $1) }

documentItems : seq1(command) { $1 }
              | paragraphs    { $1 }

document : seq(documentItems) { join $1 }

{
parseError :: (Token, [String]) -> Parser a 
parseError = throwError . uncurry UnexpectedToken

parseFile 
  :: (Error ParseError :> es, IOE :> es)
  => FilePath
  -> Eff es (Seq Node)
parseFile path = do  
  input <- liftIO $ LBS.readFile path
  let 
    alexInput = AlexInput 0 (SourcePos path 1 1) input
    parseState = ParseState 0 mempty mempty Nothing
  evalState parseState . evalState alexInput $ inject parseDocument
}