module Syntax.Parser.Internal
  ( AlexAction
  , AlexInput(..)
  , ParserState(..)
  , Parser(..)
  , alexGetByte
  , alexInputPrevChar
  , alexEOF
  , alexExcerpt
  , evalParser
  , failWith
  , getAlexInput
  , getOffset
  , getStartCode
  , getTokenBuffer
  , getSourcePos
  , getParenDepth
  , getTextBuffer
  , setAlexInput
  , setStartCode
  , setParenDepth
  , modifyParenDepth
  , modifyTextBuffer
  , pushToken
  , popToken
  , clearTextBuffer
  ) where

import           Control.Monad               (liftM, ap)
import           Control.Monad.State         (MonadState(..), gets, modify)
import           Control.Exception           (assert)
import           Data.Int                    (Int64)
import           Data.Word                   (Word8)
import           Data.List                   (uncons)
import           Data.Source                 (SourcePos(..), SourceSpan(..))
import           Data.Functor                ((<&>))
import           Data.Bifunctor              (Bifunctor(..))
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.ByteString.Lazy as LBS
import           Syntax.Parser.Error         (ParseError(..))
import           Syntax.Parser.Token         (Token)

type AlexAction = AlexInput -> SourceSpan -> Int -> Parser Token

data AlexInput = AlexInput
  { aOffset    :: !Int64
  , aSourcePos :: !SourcePos
  , aInput     :: !LBS.ByteString }
  deriving Show

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar =
  -- No patterns with a left context
  undefined

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@AlexInput {..} =
  case LBS.uncons aInput of
    Nothing -> Nothing
    Just (byte, aInput') ->
      let
        aOffset' = aOffset + 1
        aSourcePos' = nextSourcePos aSourcePos byte
        input' = input { aSourcePos = aSourcePos'
                       , aInput = aInput'
                       , aOffset = aOffset' }
      in
        Just (byte, input')
  where
    nextSourcePos (SourcePos o l _) 0x0a = SourcePos o (l + 1) 1
    nextSourcePos (SourcePos o l c) _ = SourcePos o l $ c + 1

alexEOF :: AlexInput -> Bool
alexEOF = LBS.null . aInput

alexExcerpt :: AlexInput -> Int -> Int -> T.Text
alexExcerpt AlexInput{..} offset len =
  T.decodeUtf8
    . LBS.toStrict
    . LBS.drop (fromIntegral offset)
    $ LBS.take (fromIntegral len) aInput

data ParserState = ParserState
  { pAlexInput       :: !AlexInput
  , pStartCode       :: !Int
  , pTokenBuffer     :: ![Token]
  , pParenDepth      :: !Int
  , pTextBuffer      :: !(Maybe (SourceSpan, T.Text)) }
  deriving Show

newtype Parser a = Parser
  { runParser
     :: ParserState
     -> Either ParseError (a, ParserState) }

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure !a = Parser $ Right . (a,)
  (<*>) = ap

instance Monad Parser where
  (Parser p) >>= f = Parser $ \s -> case p s of
    Right (a, s') -> runParser (f a) s'
    Left e -> Left e

instance MonadFail Parser where
  fail msg = Parser $ \s ->
    Left $ PeFailure (aSourcePos $ pAlexInput s) msg

instance MonadState ParserState Parser where
  state = Parser . (Right .)

evalParser :: Parser a -> ParserState -> Either ParseError a
evalParser p s = second fst $ runParser p s

failWith :: ParseError -> Parser a
failWith = Parser . const . Left

getAlexInput :: Parser AlexInput
getAlexInput = gets pAlexInput

getOffset :: Parser Int64
getOffset = gets $ aOffset . pAlexInput

getSourcePos :: Parser SourcePos
getSourcePos = gets $ aSourcePos . pAlexInput

getStartCode :: Parser Int
getStartCode = gets pStartCode

getTokenBuffer :: Parser [Token]
getTokenBuffer = gets pTokenBuffer

getParenDepth :: Parser Int
getParenDepth = gets pParenDepth

getTextBuffer :: Parser (Maybe (SourceSpan, T.Text))
getTextBuffer = gets pTextBuffer

setAlexInput :: AlexInput -> Parser ()
setAlexInput ai = modify $ \s -> s { pAlexInput = ai }

setStartCode :: Int -> Parser ()
setStartCode sc = modify $ \s -> s { pStartCode = sc }

setParenDepth :: Int -> Parser ()
setParenDepth d = modify $ \s -> s { pParenDepth = d }

modifyParenDepth :: (Int -> Int) -> Parser ()
modifyParenDepth f = modify $ \s -> s { pParenDepth = f $ pParenDepth s }

modifyTextBuffer :: SourceSpan -> (T.Text -> T.Text) -> Parser ()
modifyTextBuffer ss@(SourceSpan o _ end) f = do
  textBuffer <- getTextBuffer <&> \case
    Just (SourceSpan o' begin _, text) -> 
      assert (o == o') (SourceSpan o begin end, f text)
    Nothing -> (ss, f mempty)
  modify $ \s -> s { pTextBuffer = Just textBuffer }

pushToken :: Token -> Parser ()
pushToken t = modify $ \s -> s { pTokenBuffer = t : pTokenBuffer s }

popToken :: Parser (Maybe Token)
popToken = getTokenBuffer >>= (\case
  Just (x, xs) ->
    state (\s -> (Just x, s { pTokenBuffer = xs }))
  Nothing -> pure Nothing) . uncons

clearTextBuffer :: Parser (Maybe (SourceSpan, T.Text))
clearTextBuffer = state $ \s -> (pTextBuffer s, s { pTextBuffer = Nothing })