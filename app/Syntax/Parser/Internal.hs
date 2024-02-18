module Syntax.Parser.Internal
  ( Parse
  , Parser
  , Lexlet
  , AlexInput (..)
  , alexInputPrevChar
  , alexGetByte
  , alexEof
  , alexExcerpt
  , ParseState (..)
  , setStartCode
  , pushToken
  , popToken
  , pushGroup
  , peekGroup
  , popGroup
  , modifyTextBuffer
  , clearTextBuffer
  )
where

import Data.ByteString.Lazy qualified as LBS
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Source (SourcePos (..), SourceSpan (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector qualified as V
import Data.Word (Word8)
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Syntax.Parser.Error (ParseError)
import Syntax.Parser.Group (ParseGroup)
import Syntax.Parser.Token (Token)
import Prelude hiding (length)

type Parse es = (State AlexInput :> es, State ParseState :> es, Error ParseError :> es)

type Parser = Eff '[State AlexInput, State ParseState, Error ParseError]

type Lexlet = AlexInput -> SourceSpan -> Int -> Parser Token

data AlexInput = AlexInput
  { alexOffset :: !Int64
  , alexSourcePos :: !SourcePos
  , alexInput :: !LBS.ByteString
  }
  deriving (Show)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar =
  -- No patterns with a left context
  undefined

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte alex@AlexInput{..} =
  case LBS.uncons alexInput of
    Nothing -> Nothing
    Just (byte, alexInput') ->
      let
        alexOffset' = alexOffset + 1
        alexSourcePos' = nextSourcePos alexSourcePos byte
        alex' =
          alex
            { alexSourcePos = alexSourcePos'
            , alexInput = alexInput'
            , alexOffset = alexOffset'
            }
       in
        Just (byte, alex')
  where
    -- FIXME: Positions invalid for codepoints larger than 1
    nextSourcePos (SourcePos o l _) 0x0a = SourcePos o (l + 1) 1
    nextSourcePos (SourcePos o l c) _ = SourcePos o l $ c + 1

alexEof :: AlexInput -> Bool
alexEof = LBS.null . alexInput

alexExcerpt :: AlexInput -> Int -> Int -> T.Text
alexExcerpt AlexInput{..} offset length =
  T.decodeUtf8
    . LBS.toStrict
    . LBS.drop (fromIntegral offset)
    $ LBS.take (fromIntegral length) alexInput

data ParseState = ParseState
  { parseStartCode :: !Int
  , parseTokenBuffer :: !(V.Vector Token)
  , parseGroupStack :: !(V.Vector ParseGroup)
  , parseTextBuffer :: !(Maybe (SourceSpan, T.Text))
  }
  deriving (Show)

setStartCode :: State ParseState :> es => Int -> Eff es ()
setStartCode code = modify (\ps -> ps{parseStartCode = code})

pushToken :: State ParseState :> es => Token -> Eff es ()
pushToken token =
  modify (\ps -> ps{parseTokenBuffer = V.cons token $ parseTokenBuffer ps})

popToken :: State ParseState :> es => Eff es (Maybe Token)
popToken =
  state $ \ps -> case V.uncons $ parseTokenBuffer ps of
    Just (token, tokenBuffer) ->
      (Just token, ps{parseTokenBuffer = tokenBuffer})
    Nothing -> (Nothing, ps)

pushGroup :: State ParseState :> es => ParseGroup -> Eff es ()
pushGroup pg =
  modify (\ps -> ps{parseGroupStack = V.cons pg $ parseGroupStack ps})

peekGroup :: State ParseState :> es => Eff es (Maybe ParseGroup)
peekGroup = gets $ (V.!? 0) . parseGroupStack

popGroup :: State ParseState :> es => Eff es (Maybe ParseGroup)
popGroup =
  state $ \ps -> case V.uncons $ parseGroupStack ps of
    Just (pg, groupStack) ->
      (Just pg, ps{parseGroupStack = groupStack})
    Nothing -> (Nothing, ps)

modifyTextBuffer :: State ParseState :> es => SourceSpan -> (T.Text -> T.Text) -> Eff es ()
modifyTextBuffer sourceSpan@(SourceSpan o _ end) f = do
  textBuffer <-
    gets parseTextBuffer <&> \case
      Just (SourceSpan _ begin _, text) -> (SourceSpan o begin end, f text)
      Nothing -> (sourceSpan, f mempty)
  modify $ \ps -> ps{parseTextBuffer = Just textBuffer}

clearTextBuffer :: State ParseState :> es => Eff es (Maybe (SourceSpan, T.Text))
clearTextBuffer = state $ \ps -> (parseTextBuffer ps, ps{parseTextBuffer = Nothing})
