module Parser.Internal
  ( AlexAction
  , AlexInput(..)
  , ParserState(..)
  , Parser(..)
  , alexGetByte
  , alexInputPrevChar
  , alexEOF
  , alexExcerpt
  , failWith
  , getOffset
  , getSourcePos
  , setAlexInput
  , setStartCode
  ) where 

import           Control.Monad               (liftM, ap)
import           Control.Monad.State         (MonadState(..), gets, modify)
import           Data.Int                    (Int64)
import           Data.Word                   (Word8)
import           Data.Source                 (SourcePos(..))
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.ByteString.Lazy as LBS
import           Parser.Error                (ParseError(..))

type AlexAction a = AlexInput -> Int -> Parser a

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
  { pAlexInput :: !AlexInput
  , pStartCode :: !Int } 
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
    Left $ PFailure (aSourcePos $ pAlexInput s) msg

instance MonadState ParserState Parser where
  state = Parser . (Right .)

failWith :: ParseError -> Parser a
failWith = Parser . const . Left 

getOffset :: Parser Int64
getOffset = gets $ aOffset . pAlexInput

getSourcePos :: Parser SourcePos
getSourcePos = gets $ aSourcePos . pAlexInput

setAlexInput :: AlexInput -> Parser ()
setAlexInput ai = modify $ \s -> s { pAlexInput = ai }

setStartCode :: Int -> Parser ()
setStartCode sc = modify $ \s -> s { pStartCode = sc }