{-# LANGUAGE UnboxedTuples #-}

{-|
This module implements a `Parser` supporting a custom reader environment, custom
error types and an `Int` state.
-}

module FlatParse.PolyStateful (
  -- * Parser types and constructors
    type Parser(..)
  , type Res#
  , pattern OK#
  , pattern Fail#
  , pattern Err#
  , Result(..)

  -- * Running parsers
  , runParser
  , runParserS

  -- * Actions on the state and the environment
  , get
  , put
  , modify
  , ask
  , local

  -- * Errors and failures
  , empty
  , err
  , lookahead
  , fails
  , try
  , optional
  , optional_
  , withOption
  , cut
  , cutting

  -- * Basic lexing and parsing
  , eof
  , takeBs
  , takeRestBs
  , char
  , byte
  , bytes
  , string
  , switch
  , switchWithPost
  , rawSwitchWithPost
  , satisfy
  , satisfy_
  , satisfyASCII
  , satisfyASCII_
  , fusedSatisfy
  , fusedSatisfy_
  , anyWord8
  , anyWord8_
  , anyWord16
  , anyWord16_
  , anyWord32
  , anyWord32_
  , anyWord64
  , anyWord64_
  , anyWord
  , anyWord_
  , anyInt8
  , anyInt16
  , anyInt32
  , anyInt64
  , anyInt
  , anyChar
  , anyChar_
  , anyCharASCII
  , anyCharASCII_
  , isDigit
  , isGreekLetter
  , isLatinLetter
  , FlatParse.PolyStateful.readInt
  , FlatParse.PolyStateful.readInteger

  -- ** Explicit-endianness machine integers
  , anyWord16le
  , anyWord16be
  , anyWord32le
  , anyWord32be
  , anyWord64le
  , anyWord64be
  , anyInt16le
  , anyInt16be
  , anyInt32le
  , anyInt32be
  , anyInt64le
  , anyInt64be

  -- * Combinators
  , (<|>)
  , branch
  , chainl
  , chainr
  , many
  , many_
  , some
  , some_
  , notFollowedBy
  , isolate

  -- * Positions and spans
  , Pos(..)
  , Span(..)
  , getPos
  , setPos
  , endPos
  , spanOf
  , withSpan
  , byteStringOf
  , withByteString
  , inSpan

  -- ** Position and span conversions
  , Basic.validPos
  , Basic.posLineCols
  , unsafeSpanToByteString
  , Basic.unsafeSlice
  , Basic.mkPos
  , Basic.lines

  -- * Getting the rest of the input as a 'String'
  , takeLine
  , traceLine
  , takeRest
  , traceRest

  -- * `String` conversions
  , packUTF8
  , Basic.unpackUTF8

  -- * Internal functions
  , ensureBytes#
  , scan8#
  , scan16#
  , scan32#
  , scan64#
  , scanAny8#
  , scanBytes#
  , setBack#

  , withAnyWord8#
  , withAnyWord16#
  , withAnyWord32#
  , withAnyWord64#
  , withAnyInt8#
  , withAnyInt16#
  , withAnyInt32#
  , withAnyInt64#

  ) where

import           Control.Monad
import           Data.Foldable
import           Data.Map (Map)
import           GHC.Exts
import           GHC.ForeignPtr
import           GHC.Int
import           GHC.Word
import           Language.Haskell.TH
import           System.IO.Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Map.Strict as M

import           FlatParse.Internal
import           FlatParse.Internal.UnboxedNumerics

import qualified FlatParse.Basic as Basic

--------------------------------------------------------------------------------

-- | Primitive result of a parser. Possible results are given by `OK#`, `Err#` and `Fail#`
--   pattern synonyms.
type Res# s e a =
  (#
    (# a, Addr#, s #)
  | (# #)
  | (# e #)
  #)

-- | Contains return value, pointer to the rest of the input buffer and the nex `Int`
--   state.
pattern OK# :: a -> Addr# -> s -> Res# s e a
pattern OK# a s n = (# (# a, s, n #) | | #)

-- | Constructor for errors which are by default non-recoverable.
pattern Err# :: e -> Res# s e a
pattern Err# e = (# | | (# e #) #)

-- | Constructor for recoverable failure.
pattern Fail# :: Res# s e a
pattern Fail# = (# | (# #) | #)
{-# complete OK#, Err#, Fail# #-}

-- | @Parser r s e a@ has a reader environment @r@, error type @e@ and a return type @a@.
newtype Parser r s e a = Parser {runParser# :: ForeignPtrContents -> r -> Addr# -> Addr# -> s -> Res# s e a}

instance Functor (Parser r s e) where
  fmap f (Parser g) = Parser \fp !r eob s n -> case g fp r eob s n of
    OK# a s n -> let !b = f a in OK# b s n
    x         -> unsafeCoerce# x
  {-# inline fmap #-}

  (<$) a' (Parser g) = Parser \fp !r eob s n -> case g fp r eob s n of
    OK# a s n -> OK# a' s n
    x         -> unsafeCoerce# x
  {-# inline (<$) #-}

instance Applicative (Parser r s e) where
  pure a = Parser \fp !r eob s n -> OK# a s n
  {-# inline pure #-}
  Parser ff <*> Parser fa = Parser \fp !r eob s n -> case ff fp r eob s n of
    OK# f s n -> case fa fp r eob s n of
      OK# a s n -> let !b = f a in OK# b s n
      x         -> unsafeCoerce# x
    x -> unsafeCoerce# x
  {-# inline (<*>) #-}
  Parser fa <* Parser fb = Parser \fp !r eob s n -> case fa fp r eob s n of
    OK# a s n   -> case fb fp r eob s n of
      OK# b s n -> OK# a s n
      x         -> unsafeCoerce# x
    x -> unsafeCoerce# x
  {-# inline (<*) #-}
  Parser fa *> Parser fb = Parser \fp !r eob s n -> case fa fp r eob s n of
    OK# a s n -> fb fp r eob s n
    x         -> unsafeCoerce# x
  {-# inline (*>) #-}

instance Monad (Parser r s e) where
  return = pure
  {-# inline return #-}
  Parser fa >>= f = Parser \fp !r eob s n -> case fa fp r eob s n of
    OK# a s n -> runParser# (f a) fp r eob s n
    x         -> unsafeCoerce# x
  {-# inline (>>=) #-}
  (>>) = (*>)
  {-# inline (>>) #-}

-- | Higher-level boxed data type for parsing results.
data Result s e a =
    OK a s !(B.ByteString)    -- ^ Contains return value, last state, unconsumed input.
  | Fail                      -- ^ Recoverable-by-default failure.
  | Err !e                    -- ^ Unrecoverble-by-default error.
  deriving Show

instance Functor (Result s e) where
  fmap f (OK a s n) = let !b = f a in OK b s n
  fmap f r          = unsafeCoerce# r
  {-# inline fmap #-}
  (<$) a (OK _ s n) = OK a s n
  (<$) _ r          = unsafeCoerce# r
  {-# inline (<$) #-}

--------------------------------------------------------------------------------

-- | Run a parser. The `s` argument is the initial state.
runParser :: Parser r s e a  -- ^ Parser to run
          -> r -- ^ Reader enivronment
          -> s -- ^ Initial state
          -> B.ByteString -- ^ Input to the parser
          -> Result s e a
runParser (Parser f) !r n b@(B.PS (ForeignPtr _ fp) _ (I# len)) = unsafeDupablePerformIO do
  B.unsafeUseAsCString b \(Ptr buf) -> do
    let end = plusAddr# buf len
    case f fp r end buf n of
      Err# e ->
        pure (Err e)
      OK# a s n -> do
        let offset = minusAddr# s buf
        pure (OK a n (B.drop (I# offset) b))
      Fail# ->
        pure Fail
{-# inlinable runParser #-}

-- | Run a parser on a `String` input. Reminder: @OverloadedStrings@ for `B.ByteString` does not
--   yield a valid UTF-8 encoding! For non-ASCII `B.ByteString` literal input, use `runParserS` or
--   `packUTF8` for testing.
runParserS :: Parser r s e a -> r -> s -> String -> Result s e a
runParserS pa r !n s = runParser pa r n (packUTF8 s)

--------------------------------------------------------------------------------

-- | Query the state.
get :: Parser r s e s
get = Parser \fp !r eob s n -> OK# n s n
{-# inline get #-}

-- | Write the state.
put :: s -> Parser r s e ()
put n = Parser \fp !r eob s _ -> OK# () s n
{-# inline put #-}

-- | Modify the state.
modify :: (s -> s) -> Parser r s e ()
modify f = Parser \fp !r eob s n -> OK# () s (f $! n)
{-# inline modify #-}

-- | Query the environment.
ask :: Parser r s e r
ask = Parser \fp !r eob s n -> OK# r s n
{-# inline ask #-}

-- | Run a parser in a modified environment.
local :: (r -> r) -> Parser r s e a -> Parser r s e a
local f (Parser g) = Parser \fp !r eob s n -> let !r' = f r in g fp r' eob s n
{-# inline local #-}

--------------------------------------------------------------------------------

-- | The failing parser. By default, parser choice `(<|>)` arbitrarily backtracks
--   on parser failure.
empty :: Parser r s e a
empty = Parser \fp !r eob s n -> Fail#
{-# inline empty #-}

-- | Throw a parsing error. By default, parser choice `(<|>)` can't backtrack
--   on parser error. Use `try` to convert an error to a recoverable failure.
err :: e -> Parser r s e a
err e = Parser \fp !r eob s n -> Err# e
{-# inline err #-}

-- | Save the parsing state, then run a parser, then restore the state.
lookahead :: Parser r s e a -> Parser r s e a
lookahead (Parser f) = Parser \fp !r eob s n ->
  case f fp r eob s n of
    OK# a _ _ -> OK# a s n
    x         -> x
{-# inline lookahead #-}

-- | Convert a parsing failure to a success.
fails :: Parser r s e a -> Parser r s e ()
fails (Parser f) = Parser \fp !r eob s n ->
  case f fp r eob s n of
    OK# _ _ _ -> Fail#
    Fail#     -> OK# () s n
    Err# e    -> Err# e
{-# inline fails #-}

-- | Convert a parsing error into failure.
try :: Parser r s e a -> Parser r s e a
try (Parser f) = Parser \fp !r eob s n -> case f fp r eob s n of
  Err# _ -> Fail#
  x      -> x
{-# inline try #-}

-- | Convert a parsing failure to a `Maybe`. If possible, use `withOption` instead.
optional :: Parser r s e a -> Parser r s e (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
{-# inline optional #-}

-- | Convert a parsing failure to a `()`.
optional_ :: Parser r s e a -> Parser r s e ()
optional_ p = (() <$ p) <|> pure ()
{-# inline optional_ #-}

-- | CPS'd version of `optional`. This is usually more efficient, since it gets rid of the
--   extra `Maybe` allocation.
withOption :: Parser r s e a -> (a -> Parser r s e b) -> Parser r s e b -> Parser r s e b
withOption (Parser f) just (Parser nothing) = Parser \fp !r eob s n -> case f fp r eob s n of
  OK# a s n -> runParser# (just a) fp r eob s n
  Fail#     -> nothing fp r eob s n
  Err# e    -> Err# e
{-# inline withOption #-}

-- | Convert a parsing failure to an error.
cut :: Parser r s e a -> e -> Parser r s e a
cut (Parser f) e = Parser \fp !r eob s n -> case f fp r eob s n of
  Fail# -> Err# e
  x     -> x
{-# inline cut #-}

-- | Run the parser, if we get a failure, throw the given error, but if we get an error, merge the
--   inner and the newly given errors using the @e -> e -> e@ function. This can be useful for
--   implementing parsing errors which may propagate hints or accummulate contextual information.
cutting :: Parser r s e a -> e -> (e -> e -> e) -> Parser r s e a
cutting (Parser f) e merge = Parser \fp !r eob s n -> case f fp r eob s n of
  Fail#   -> Err# e
  Err# e' -> let !e'' = merge e' e in Err# e''
  x       -> x
{-# inline cutting #-}

--------------------------------------------------------------------------------


-- | Succeed if the input is empty.
eof :: Parser r s e ()
eof = Parser \fp !r eob s n -> case eqAddr# eob s of
  1# -> OK# () s n
  _  -> Fail#
{-# inline eof #-}

-- | Read the given number of bytes as a 'ByteString'.
--
-- Throws a runtime error if given a negative integer.
takeBs :: Int -> Parser r s e B.ByteString
takeBs (I# n#) = Parser \fp !r eob s n -> case n# <=# minusAddr# eob s of
  1# -> -- have to runtime check for negative values, because they cause a hang
    case n# >=# 0# of
      1# -> OK# (B.PS (ForeignPtr s fp) 0 (I# n#)) (plusAddr# s n#) n
      _  -> error "FlatParse.Basic.take: negative integer"
  _  -> Fail#
{-# inline takeBs #-}

-- | Consume the rest of the input. May return the empty bytestring.
takeRestBs :: Parser r s e B.ByteString
takeRestBs = Parser \fp !r eob s n ->
  let n# = minusAddr# eob s
  in  OK# (B.PS (ForeignPtr s fp) 0 (I# n#)) eob n
{-# inline takeRestBs #-}

-- | Parse a UTF-8 character literal. This is a template function, you can use it as
--   @$(char \'x\')@, for example, and the splice in this case has type @Parser r s e ()@.
char :: Char -> Q Exp
char c = string [c]

-- | Read a byte.
byte :: Word8 -> Parser r s e ()
byte w = ensureBytes# 1 >> scan8# w
{-# inline byte #-}

-- | Read a sequence of bytes. This is a template function, you can use it as @$(bytes [3, 4, 5])@,
--   for example, and the splice has type @Parser r s e ()@.
bytes :: [Word] -> Q Exp
bytes bytes = do
  let !len = length bytes
  [| ensureBytes# len >> $(scanBytes# bytes) |]

-- | Parse a UTF-8 string literal. This is a template function, you can use it as @$(string "foo")@,
--   for example, and the splice has type @Parser r s e ()@.
string :: String -> Q Exp
string str = bytes (strToBytes str)

{-|
This is a template function which makes it possible to branch on a collection of string literals in
an efficient way. By using `switch`, such branching is compiled to a trie of primitive parsing
operations, which has optimized control flow, vectorized reads and grouped checking for needed input
bytes.

The syntax is slightly magical, it overloads the usual @case@ expression. An example:

@
    $(switch [| case _ of
        "foo" -> pure True
        "bar" -> pure False |])
@

The underscore is mandatory in @case _ of@. Each branch must be a string literal, but optionally
we may have a default case, like in

@
    $(switch [| case _ of
        "foo" -> pure 10
        "bar" -> pure 20
        _     -> pure 30 |])
@

All case right hand sides must be parsers with the same type. That type is also the type
of the whole `switch` expression.

A `switch` has longest match semantics, and the order of cases does not matter, except for
the default case, which may only appear as the last case.

If a `switch` does not have a default case, and no case matches the input, then it returns with
failure, \without\ having consumed any input. A fallthrough to the default case also does not
consume any input.
-}
switch :: Q Exp -> Q Exp
switch = switchWithPost Nothing

{-|
Switch expression with an optional first argument for performing a post-processing action after
every successful branch matching. For example, if we have @ws :: Parser r s e ()@ for a
whitespace parser, we might want to consume whitespace after matching on any of the switch
cases. For that case, we can define a "lexeme" version of `switch` as follows.

@
  switch' :: Q Exp -> Q Exp
  switch' = switchWithPost (Just [| ws |])
@

Note that this @switch'@ function cannot be used in the same module it's defined in, because of the
stage restriction of Template Haskell.
-}
switchWithPost :: Maybe (Q Exp) -> Q Exp -> Q Exp
switchWithPost postAction exp = do
  !postAction <- sequence postAction
  (!cases, !fallback) <- parseSwitch exp
  genTrie $! genSwitchTrie' postAction cases fallback

-- | Version of `switchWithPost` without syntactic sugar. The second argument is the
--   list of cases, the third is the default case.
rawSwitchWithPost :: Maybe (Q Exp) -> [(String, Q Exp)] -> Maybe (Q Exp) -> Q Exp
rawSwitchWithPost postAction cases fallback = do
  !postAction <- sequence postAction
  !cases <- forM cases \(str, rhs) -> (str,) <$> rhs
  !fallback <- sequence fallback
  genTrie $! genSwitchTrie' postAction cases fallback

-- | Parse a UTF-8 `Char` for which a predicate holds.
satisfy :: (Char -> Bool) -> Parser r s e Char
satisfy f = Parser \fp !r eob s n -> case runParser# anyChar fp r eob s n of
  OK# c s n | f c -> OK# c s n
  _               -> Fail#
{-#  inline satisfy #-}

-- | Skip a UTF-8 `Char` for which a predicate holds.
satisfy_ :: (Char -> Bool) -> Parser r s e ()
satisfy_ f = Parser \fp !r eob s n -> case runParser# anyChar fp r eob s n of
  OK# c s n | f c -> OK# () s n
  _               -> Fail#
{-#  inline satisfy_ #-}

-- | Parse an ASCII `Char` for which a predicate holds. Assumption: the predicate must only return
--   `True` for ASCII-range characters. Otherwise this function might read a 128-255 range byte,
--   thereby breaking UTF-8 decoding.
satisfyASCII :: (Char -> Bool) -> Parser r s e Char
satisfyASCII f = Parser \fp !r eob s n -> case eqAddr# eob s of
  1# -> Fail#
  _  -> case derefChar8# s of
    c1 | f (C# c1) -> OK# (C# c1) (plusAddr# s 1#) n
       | otherwise -> Fail#
{-#  inline satisfyASCII #-}

-- | Skip an ASCII `Char` for which a predicate holds.  Assumption: the
--   predicate must only return `True` for ASCII-range characters.
satisfyASCII_ :: (Char -> Bool) -> Parser r s e ()
satisfyASCII_ f = () <$ satisfyASCII f
{-# inline satisfyASCII_ #-}

-- | This is a variant of `satisfy` which allows more optimization. We can pick four testing
--   functions for the four cases for the possible number of bytes in the UTF-8 character. So in
--   @fusedSatisfy f1 f2 f3 f4@, if we read a one-byte character, the result is scrutinized with
--   @f1@, for two-bytes, with @f2@, and so on. This can result in dramatic lexing speedups.
--
--   For example, if we want to accept any letter, the naive solution would be to use
--   `Data.Char.isLetter`, but this accesses a large lookup table of Unicode character classes. We
--   can do better with @fusedSatisfy isLatinLetter isLetter isLetter isLetter@, since here the
--   `isLatinLetter` is inlined into the UTF-8 decoding, and it probably handles a great majority of
--   all cases without accessing the character table.
fusedSatisfy :: (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> Parser r s e Char
fusedSatisfy f1 f2 f3 f4 = Parser \fp !r eob buf n -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# | f1 (C# c1) -> OK# (C# c1) (plusAddr# buf 1#) n
         | otherwise  -> Fail#
      _  -> case eqAddr# eob (plusAddr# buf 1#) of
        1# -> Fail#
        _ -> case indexCharOffAddr# buf 1# of
          c2 -> case c1 `leChar#` '\xDF'# of
            1# ->
              let resc = C# (chr# (((ord# c1 -# 0xC0#) `uncheckedIShiftL#` 6#) `orI#`
                                   (ord# c2 -# 0x80#)))
              in case f2 resc of
                   True -> OK# resc (plusAddr# buf 2#) n
                   _    -> Fail#
            _ -> case eqAddr# eob (plusAddr# buf 2#) of
              1# -> Fail#
              _  -> case indexCharOffAddr# buf 2# of
                c3 -> case c1 `leChar#` '\xEF'# of
                  1# ->
                    let resc = C# (chr# (((ord# c1 -# 0xE0#) `uncheckedIShiftL#` 12#) `orI#`
                                         ((ord# c2 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                         (ord# c3 -# 0x80#)))
                    in case f3 resc of
                         True -> OK# resc (plusAddr# buf 3#) n
                         _    -> Fail#
                  _ -> case eqAddr# eob (plusAddr# buf 3#) of
                    1# -> Fail#
                    _  -> case indexCharOffAddr# buf 3# of
                      c4 ->
                        let resc = C# (chr# (((ord# c1 -# 0xF0#) `uncheckedIShiftL#` 18#) `orI#`
                                             ((ord# c2 -# 0x80#) `uncheckedIShiftL#` 12#) `orI#`
                                             ((ord# c3 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                              (ord# c4 -# 0x80#)))
                        in case f4 resc of
                             True -> OK# resc (plusAddr# buf 4#) n
                             _    -> Fail#
{-# inline fusedSatisfy #-}

-- | Skipping variant of `fusedSatisfy`.
fusedSatisfy_ :: (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> Parser r s e ()
fusedSatisfy_ f1 f2 f3 f4 = () <$ fusedSatisfy f1 f2 f3 f4
{-# inline fusedSatisfy_ #-}

-- | Parse any UTF-8-encoded `Char`.
anyChar :: Parser r s e Char
anyChar = Parser \fp !r eob buf n -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK# (C# c1) (plusAddr# buf 1#) n
      _  -> case eqAddr# eob (plusAddr# buf 1#) of
        1# -> Fail#
        _ -> case indexCharOffAddr# buf 1# of
          c2 -> case c1 `leChar#` '\xDF'# of
            1# ->
              let resc = ((ord# c1 -# 0xC0#) `uncheckedIShiftL#` 6#) `orI#`
                          (ord# c2 -# 0x80#)
              in OK# (C# (chr# resc)) (plusAddr# buf 2#) n
            _ -> case eqAddr# eob (plusAddr# buf 2#) of
              1# -> Fail#
              _  -> case indexCharOffAddr# buf 2# of
                c3 -> case c1 `leChar#` '\xEF'# of
                  1# ->
                    let resc = ((ord# c1 -# 0xE0#) `uncheckedIShiftL#` 12#) `orI#`
                               ((ord# c2 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                (ord# c3 -# 0x80#)
                    in OK# (C# (chr# resc)) (plusAddr# buf 3#) n
                  _ -> case eqAddr# eob (plusAddr# buf 3#) of
                    1# -> Fail#
                    _  -> case indexCharOffAddr# buf 3# of
                      c4 ->
                        let resc = ((ord# c1 -# 0xF0#) `uncheckedIShiftL#` 18#) `orI#`
                                   ((ord# c2 -# 0x80#) `uncheckedIShiftL#` 12#) `orI#`
                                   ((ord# c3 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                    (ord# c4 -# 0x80#)
                        in OK# (C# (chr# resc)) (plusAddr# buf 4#) n
{-# inline anyChar #-}

-- | Skip any UTF-8-encoded `Char`.
anyChar_ :: Parser r s e ()
anyChar_ = Parser \fp !r eob buf n -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK# () (plusAddr# buf 1#) n
      _  ->
        let buf' =
              case c1 `leChar#` '\xDF'# of
                1# -> plusAddr# buf 2#
                _  -> case c1 `leChar#` '\xEF'# of
                    1# -> plusAddr# buf 3#
                    _  ->  plusAddr# buf 4#
        in case leAddr# buf' eob of
             1# -> OK# () buf' n
             _  -> Fail#
{-# inline anyChar_ #-}


-- | Parse any `Char` in the ASCII range, fail if the next input character is not in the range.
--   This is more efficient than `anyChar` if we are only working with ASCII.
anyCharASCII :: Parser r s e Char
anyCharASCII = Parser \fp !r eob buf n -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK# (C# c1) (plusAddr# buf 1#) n
      _  -> Fail#
{-# inline anyCharASCII #-}

-- | Skip any `Char` in the ASCII range. More efficient than `anyChar_` if we're working only with
--   ASCII.
anyCharASCII_ :: Parser r s e ()
anyCharASCII_ = () <$ anyCharASCII
{-# inline anyCharASCII_ #-}

-- | Read an `Int` from the input, as a non-empty digit sequence. The `Int` may
--   overflow in the result.
readInt :: Parser r s e Int
readInt = Parser \fp r eob s n -> case FlatParse.Internal.readInt eob s of
  (# (##) | #)        -> Fail#
  (# | (# i, s' #) #) -> OK# (I# i) s' n
{-# inline readInt #-}

-- | Read an `Integer` from the input, as a non-empty digit sequence.
readInteger :: Parser r s e Integer
readInteger = Parser \fp r eob s n -> case FlatParse.Internal.readInteger fp eob s of
  (# (##) | #)        -> Fail#
  (# | (# i, s' #) #) -> OK# i s' n
{-# inline readInteger #-}


--------------------------------------------------------------------------------

-- | Choose between two parsers. If the first parser fails, try the second one, but if the first one
--   throws an error, propagate the error.
infixr 6 <|>
(<|>) :: Parser r s e a -> Parser r s e a -> Parser r s e a
(<|>) (Parser f) (Parser g) = Parser \fp !r eob s n ->
  case f fp r eob s n of
    Fail# -> g fp r eob s n
    x     -> x
{-# inline (<|>) #-}

-- | Branch on a parser: if the first argument succeeds, continue with the second, else with the third.
--   This can produce slightly more efficient code than `(<|>)`. Moreover, `ḃranch` does not
--   backtrack from the true/false cases.
branch :: Parser r s e a -> Parser r s e b -> Parser r s e b -> Parser r s e b
branch pa pt pf = Parser \fp !r eob s n -> case runParser# pa fp r eob s n of
  OK# _ s n -> runParser# pt fp r eob s n
  Fail#     -> runParser# pf fp r eob s n
  Err# e    -> Err# e
{-# inline branch #-}

-- | An analogue of the list `foldl` function: first parse a @b@, then parse zero or more @a@-s,
--   and combine the results in a left-nested way by the @b -> a -> b@ function. Note: this is not
--   the usual `chainl` function from the parsec libraries!
chainl :: (b -> a -> b) -> Parser r s e b -> Parser r s e a -> Parser r s e b
chainl f start elem = start >>= go where
  go b = do {!a <- elem; go $! f b a} <|> pure b
{-# inline chainl #-}

-- | An analogue of the list `foldr` function: parse zero or more @a@-s, terminated by a @b@, and
--   combine the results in a right-nested way using the @a -> b -> b@ function. Note: this is not
--   the usual `chainr` function from the parsec libraries!
chainr :: (a -> b -> b) -> Parser r s e a -> Parser r s e b -> Parser r s e b
chainr f (Parser elem) (Parser end) = go where
  go = Parser \fp !r eob s n -> case elem fp r eob s n of
    OK# a s n -> case runParser# go fp r eob s n of
      OK# b s n -> let !b' = f a b in OK# b' s n
      x         -> x
    Fail# -> end fp r eob s n
    Err# e -> Err# e
{-# inline chainr #-}

-- | Run a parser zero or more times, collect the results in a list. Note: for optimal performance,
--   try to avoid this. Often it is possible to get rid of the intermediate list by using a
--   combinator or a custom parser.
many :: Parser r s e a -> Parser r s e [a]
many (Parser f) = go where
  go = Parser \fp !r eob s n -> case f fp r eob s n of
    OK# a s n -> case runParser# go fp r eob s n of
                   OK# as s n -> OK# (a:as) s n
                   x          -> x
    Fail#  -> OK# [] s n
    Err# e -> Err# e
{-# inline many #-}

-- | Skip a parser zero or more times.
many_ :: Parser r s e a -> Parser r s e ()
many_ (Parser f) = go where
  go = Parser \fp !r eob s n -> case f fp r eob s n of
    OK# a s n -> runParser# go fp r eob s n
    Fail#     -> OK# () s n
    Err# e    -> Err# e
{-# inline many_ #-}

-- | Run a parser one or more times, collect the results in a list. Note: for optimal performance,
--   try to avoid this. Often it is possible to get rid of the intermediate list by using a
--   combinator or a custom parser.
some :: Parser r s e a -> Parser r s e [a]
some p = (:) <$> p <*> many p
{-# inline some #-}

-- | Skip a parser one or more times.
some_ :: Parser r s e a -> Parser r s e ()
some_ pa = pa >> many_ pa
{-# inline some_ #-}

-- | Succeed if the first parser succeeds and the second one fails. The parsing
--   state is restored to the point of the first argument's success.
notFollowedBy :: Parser r s e a -> Parser r s e b -> Parser r s e a
notFollowedBy p1 p2 = p1 <* lookahead (fails p2)
{-# inline notFollowedBy #-}

-- | @isolate n p@ runs the parser @p@ isolated to the next @n@ bytes. All
--   isolated bytes must be consumed.
--
-- Throws a runtime error if given a negative integer.
isolate :: Int -> Parser r s e a -> Parser r s e a
isolate (I# n#) p = Parser \fp !r eob s n ->
  let s' = plusAddr# s n#
  in  case n# <=# minusAddr# eob s of
        1# -> case n# >=# 0# of
          1# -> case runParser# p fp r s' s n of
            OK# a s'' n' -> case eqAddr# s' s'' of
              1# -> OK# a s'' n'
              _  -> Fail# -- isolated segment wasn't fully consumed
            Fail#     -> Fail#
            Err# e    -> Err# e
          _  -> error "FlatParse.Basic.isolate: negative integer"
        _  -> Fail# -- you tried to isolate more than we have left
{-# inline isolate #-}

--------------------------------------------------------------------------------

-- | Get the current position in the input.
getPos :: Parser r s e Pos
getPos = Parser \fp !r eob s n -> OK# (addrToPos# eob s) s n
{-# inline getPos #-}

-- | Set the input position. Warning: this can result in crashes if the position points outside the
--   current buffer. It is always safe to `setPos` values which came from `getPos` with the current
--   input.
setPos :: Pos -> Parser r s e ()
setPos s = Parser \fp !r eob _ n -> OK# () (posToAddr# eob s) n
{-# inline setPos #-}

-- | The end of the input.
endPos :: Pos
endPos = Pos 0
{-# inline endPos #-}


-- | Return the consumed span of a parser. Use `withSpan` if possible for better efficiency.
spanOf :: Parser r s e a -> Parser r s e Span
spanOf (Parser f) = Parser \fp !r eob s n -> case f fp r eob s n of
  OK# a s' n -> OK# (Span (addrToPos# eob s) (addrToPos# eob s')) s' n
  x          -> unsafeCoerce# x
{-# inline spanOf #-}

-- | Bind the result together with the span of the result. CPS'd version of `spanOf`
--   for better unboxing.
withSpan :: Parser r s e a -> (a -> Span -> Parser r s e b) -> Parser r s e b
withSpan (Parser f) g = Parser \fp !r eob s n -> case f fp r eob s n of
  OK# a s' n -> runParser# (g a (Span (addrToPos# eob s) (addrToPos# eob s'))) fp r eob s' n
  x          -> unsafeCoerce# x
{-# inline withSpan #-}

-- | Return the `B.ByteString` consumed by a parser. Note: it's more efficient to use `spanOf` and
--   `withSpan` instead.
byteStringOf :: Parser r s e a -> Parser r s e B.ByteString
byteStringOf (Parser f) = Parser \fp !r eob s n -> case f fp r eob s n of
  OK# a s' n -> OK# (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s))) s' n
  x          -> unsafeCoerce# x
{-# inline byteStringOf #-}

-- | CPS'd version of `byteStringOf`. Can be more efficient, because the result is more eagerly unboxed
--   by GHC. It's more efficient to use `spanOf` or `withSpan` instead.
withByteString :: Parser r s e a -> (a -> B.ByteString -> Parser r s e b) -> Parser r s e b
withByteString (Parser f) g = Parser \fp !r eob s n -> case f fp r eob s n of
  OK# a s' n -> runParser# (g a (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s)))) fp r eob s' n
  x          -> unsafeCoerce# x
{-# inline withByteString #-}

-- | Create a `B.ByteString` from a `Span`. The result is invalid is the `Span` points
--   outside the current buffer, or if the `Span` start is greater than the end position.
unsafeSpanToByteString :: Span -> Parser r s e B.ByteString
unsafeSpanToByteString (Span l r) =
  lookahead (setPos l >> byteStringOf (setPos r))
{-# inline unsafeSpanToByteString #-}


-- | Run a parser in a given input span. The input position and the `Int` state is restored after
--   the parser is finished, so `inSpan` does not consume input and has no side effect.  Warning:
--   this operation may crash if the given span points outside the current parsing buffer. It's
--   always safe to use `inSpan` if the span comes from a previous `withSpan` or `spanOf` call on
--   the current input.
inSpan :: Span -> Parser r s e a -> Parser r s e a
inSpan (Span s eob) (Parser f) = Parser \fp !r eob' s' n' ->
  case f fp r (posToAddr# eob' eob) (posToAddr# eob' s) n' of
    OK# a _ _ -> OK# a s' n'
    x         -> unsafeCoerce# x
{-# inline inSpan #-}


--------------------------------------------------------------------------------

-- | Parse the rest of the current line as a `String`. Assumes UTF-8 encoding,
--   throws an error if the encoding is invalid.
takeLine :: Parser r s e String
takeLine = branch eof (pure "") do
  c <- anyChar
  case c of
    '\n' -> pure ""
    _    -> (c:) <$> takeLine

-- | Parse the rest of the current line as a `String`, but restore the parsing state.
--   Assumes UTF-8 encoding. This can be used for debugging.
traceLine :: Parser r s e String
traceLine = lookahead takeLine

-- | Take the rest of the input as a `String`. Assumes UTF-8 encoding.
takeRest :: Parser r s e String
takeRest = branch eof (pure "") do
  c <- anyChar
  cs <- takeRest
  pure (c:cs)

-- | Get the rest of the input as a `String`, but restore the parsing state. Assumes UTF-8 encoding.
--   This can be used for debugging.
traceRest :: Parser r s e String
traceRest = lookahead takeRest

--------------------------------------------------------------------------------

-- | Check that the input has at least the given number of bytes.
ensureBytes# :: Int -> Parser r s e ()
ensureBytes# (I# len) = Parser \fp !r eob s n ->
  case len  <=# minusAddr# eob s of
    1# -> OK# () s n
    _  -> Fail#
{-# inline ensureBytes# #-}

-- | Unsafely read a concrete byte from the input. It's not checked that the input has
--   enough bytes.
scan8# :: Word8 -> Parser r s e ()
scan8# (W8# c) = Parser \fp !r eob s n ->
  case indexWord8OffAddr# s 0# of
    c' -> case eqWord8'# c c' of
      1# -> OK# () (plusAddr# s 1#) n
      _  -> Fail#
{-# inline scan8# #-}

-- | Unsafely read two concrete bytes from the input. It's not checked that the input has
--   enough bytes.
scan16# :: Word16 -> Parser r s e ()
scan16# (W16# c) = Parser \fp !r eob s n ->
  case indexWord16OffAddr# s 0# of
    c' -> case eqWord16'# c c' of
      1# -> OK# () (plusAddr# s 2#) n
      _  -> Fail#
{-# inline scan16# #-}

-- | Unsafely read four concrete bytes from the input. It's not checked that the input has
--   enough bytes.
scan32# :: Word32 -> Parser r s e ()
scan32# (W32# c) = Parser \fp !r eob s n ->
  case indexWord32OffAddr# s 0# of
    c' -> case eqWord32'# c c' of
      1# -> OK# () (plusAddr# s 4#) n
      _  -> Fail#
{-# inline scan32# #-}

-- | Unsafely read eight concrete bytes from the input. It's not checked that the input has
--   enough bytes.
scan64# :: Word -> Parser r s e ()
scan64# (W# c) = Parser \fp !r eob s n ->
  case indexWord64OffAddr# s 0# of
    c' -> case eqWord# c c' of
      1# -> OK# () (plusAddr# s 8#) n
      _  -> Fail#
{-# inline scan64# #-}

-- | Unsafely read and return a byte from the input. It's not checked that the input is non-empty.
scanAny8# :: Parser r s e Word8
scanAny8# = Parser \fp !r eob s n -> OK# (W8# (indexWord8OffAddr# s 0#)) (plusAddr# s 1#) n
{-# inline scanAny8# #-}

scanPartial64# :: Int -> Word -> Parser r s e ()
scanPartial64# (I# len) (W# w) = Parser \fp !r eob s n ->
  case indexWordOffAddr# s 0# of
    w' -> case uncheckedIShiftL# (8# -# len) 3# of
      sh -> case uncheckedShiftL# w' sh of
        w' -> case uncheckedShiftRL# w' sh of
          w' -> case eqWord# w w' of
            1# -> OK# () (plusAddr# s len) n
            _  -> Fail#
{-# inline scanPartial64# #-}

-- | Decrease the current input position by the given number of bytes.
setBack# :: Int -> Parser r s e ()
setBack# (I# i) = Parser \fp !r eob s n ->
  OK# () (plusAddr# s (negateInt# i)) n
{-# inline setBack# #-}

-- | Template function, creates a @Parser r s e ()@ which unsafely scans a given
--   sequence of bytes.
scanBytes# :: [Word] -> Q Exp
scanBytes# bytes = do
  let !(leading, w8s) = splitBytes bytes
      !scanw8s        = go w8s where
                         go (w8:[] ) = [| scan64# w8 |]
                         go (w8:w8s) = [| scan64# w8 >> $(go w8s) |]
                         go []       = [| pure () |]
  case w8s of
    [] -> go leading
          where
            go (a:b:c:d:[]) = let !w = packBytes [a, b, c, d] in [| scan32# w |]
            go (a:b:c:d:ws) = let !w = packBytes [a, b, c, d] in [| scan32# w >> $(go ws) |]
            go (a:b:[])     = let !w = packBytes [a, b]       in [| scan16# w |]
            go (a:b:ws)     = let !w = packBytes [a, b]       in [| scan16# w >> $(go ws) |]
            go (a:[])       = [| scan8# a |]
            go []           = [| pure () |]
    _  -> case leading of

      []              -> scanw8s
      [a]             -> [| scan8# a >> $scanw8s |]
      ws@[a, b]       -> let !w = packBytes ws in [| scan16# w >> $scanw8s |]
      ws@[a, b, c, d] -> let !w = packBytes ws in [| scan32# w >> $scanw8s |]
      ws              -> let !w = packBytes ws
                             !l = length ws
                         in [| scanPartial64# l w >> $scanw8s |]


-- Switching code generation
--------------------------------------------------------------------------------

#if MIN_VERSION_base(4,15,0)
mkDoE = DoE Nothing
{-# inline mkDoE #-}
#else
mkDoE = DoE
{-# inline mkDoE #-}
#endif

genTrie :: (Map (Maybe Int) Exp, Trie' (Rule, Int, Maybe Int)) -> Q Exp
genTrie (rules, t) = do
  branches <- traverse (\e -> (,) <$> (newName "rule") <*> pure e) rules

  let ix m k = case M.lookup k m of
        Nothing -> error ("key not in map: " ++ show k)
        Just a  -> a

  let ensure :: Maybe Int -> Maybe (Q Exp)
      ensure = fmap (\n -> [| ensureBytes# n |])

      fallback :: Rule -> Int ->  Q Exp
      fallback rule 0 = pure $ VarE $ fst $ ix branches rule
      fallback rule n = [| setBack# n >> $(pure $ VarE $ fst $ ix branches rule) |]

  let go :: Trie' (Rule, Int, Maybe Int) -> Q Exp
      go = \case
        Branch' (r, n, alloc) ts
          | M.null ts -> pure $ VarE $ fst $ branches M.! r
          | otherwise -> do
              !next         <- (traverse . traverse) go (M.toList ts)
              !defaultCase  <- fallback r (n + 1)

              let cases = mkDoE $
                    [BindS (VarP (mkName "c")) (VarE 'scanAny8#),
                      NoBindS (CaseE (VarE (mkName "c"))
                         (map (\(w, t) ->
                                 Match (LitP (IntegerL (fromIntegral w)))
                                       (NormalB t)
                                       [])
                              next
                          ++ [Match WildP (NormalB defaultCase) []]))]

              case ensure alloc of
                Nothing    -> pure cases
                Just alloc -> [| branch $alloc $(pure cases) $(fallback r n) |]

        Path (r, n, alloc) ws t ->
          case ensure alloc of
            Nothing    -> [| branch $(scanBytes# ws) $(go t) $(fallback r n)|]
            Just alloc -> [| branch ($alloc >> $(scanBytes# ws)) $(go t) $(fallback r n) |]

  letE
    (map (\(x, rhs) -> valD (varP x) (normalB (pure rhs)) []) (Data.Foldable.toList branches))
    (go t)

parseSwitch :: Q Exp -> Q ([(String, Exp)], Maybe Exp)
parseSwitch exp = exp >>= \case
  CaseE (UnboundVarE _) []    -> error "switch: empty clause list"
  CaseE (UnboundVarE _) cases -> do
    (!cases, !last) <- pure (init cases, last cases)
    !cases <- forM cases \case
      Match (LitP (StringL str)) (NormalB rhs) [] -> pure (str, rhs)
      _ -> error "switch: expected a match clause on a string literal"
    (!cases, !last) <- case last of
      Match (LitP (StringL str)) (NormalB rhs) [] -> pure (cases ++ [(str, rhs)], Nothing)
      Match WildP                (NormalB rhs) [] -> pure (cases, Just rhs)
      _ -> error "switch: expected a match clause on a string literal or a wildcard"
    pure (cases, last)
  _ -> error "switch: expected a \"case _ of\" expression"

genSwitchTrie' :: Maybe Exp -> [(String, Exp)] -> Maybe Exp
              -> (Map (Maybe Int) Exp, Trie' (Rule, Int, Maybe Int))
genSwitchTrie' postAction cases fallback =

  let (!branches, !strings) = unzip do
        (!i, (!str, !rhs)) <- zip [0..] cases
        case postAction of
          Nothing    -> pure ((Just i, rhs), (i, str))
          Just !post -> pure ((Just i, (VarE '(>>)) `AppE` post `AppE` rhs), (i, str))

      !m    =  M.fromList ((Nothing, maybe (VarE 'empty) id fallback) : branches)
      !trie = compileTrie strings
  in (m , trie)

--------------------------------------------------------------------------------

withAnyWord8# :: (Word8'# -> Parser r s e a) -> Parser r s e a
withAnyWord8# p = Parser \fp !r eob buf n -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case indexWord8OffAddr# buf 0# of
    w# -> runParser# (p w#) fp r eob (plusAddr# buf 1#) n
{-# inline withAnyWord8# #-}

withAnyWord16# :: (Word16'# -> Parser r s e a) -> Parser r s e a
withAnyWord16# p = Parser \fp !r eob buf n -> case 2# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> case indexWord16OffAddr# buf 0# of
    w# -> runParser# (p w#) fp r eob (plusAddr# buf 2#) n
{-# inline withAnyWord16# #-}

withAnyWord32# :: (Word32'# -> Parser r s e a) -> Parser r s e a
withAnyWord32# p = Parser \fp !r eob buf n -> case 4# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> case indexWord32OffAddr# buf 0# of
    w# -> runParser# (p w#) fp r eob (plusAddr# buf 4#) n
{-# inline withAnyWord32# #-}

withAnyWord64# :: (Word# -> Parser r s e a) -> Parser r s e a
withAnyWord64# p = Parser \fp !r eob buf n -> case 8# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> case indexWordOffAddr# buf 0# of
    w# -> runParser# (p w#) fp r eob (plusAddr# buf 8#) n
{-# inline withAnyWord64# #-}

withAnyInt8# :: (Int8'# -> Parser r s e a) -> Parser r s e a
withAnyInt8# p = Parser \fp !r eob buf n -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case indexInt8OffAddr# buf 0# of
    i# -> runParser# (p i#) fp r eob (plusAddr# buf 1#) n
{-# inline withAnyInt8# #-}

withAnyInt16# :: (Int16'# -> Parser r s e a) -> Parser r s e a
withAnyInt16# p = Parser \fp !r eob buf n -> case 2# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> case indexInt16OffAddr# buf 0# of
    i# -> runParser# (p i#) fp r eob (plusAddr# buf 2#) n
{-# inline withAnyInt16# #-}

withAnyInt32# :: (Int32'# -> Parser r s e a) -> Parser r s e a
withAnyInt32# p = Parser \fp !r eob buf n -> case 4# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> case indexInt32OffAddr# buf 0# of
    i# -> runParser# (p i#) fp r eob (plusAddr# buf 4#) n
{-# inline withAnyInt32# #-}

withAnyInt64# :: (Int# -> Parser r s e a) -> Parser r s e a
withAnyInt64# p = Parser \fp !r eob buf n -> case 8# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> case indexInt64OffAddr# buf 0# of
    i# -> runParser# (p i#) fp r eob (plusAddr# buf 8#) n
{-# inline withAnyInt64# #-}

--------------------------------------------------------------------------------

-- | Parse any 'Word8' (byte).
anyWord8 :: Parser r s e Word8
anyWord8 = withAnyWord8# (\w# -> pure (W8# w#))
{-# inline anyWord8 #-}

-- | Skip any 'Word8' (byte).
anyWord8_ :: Parser r s e ()
anyWord8_ = () <$ anyWord8
{-# inline anyWord8_ #-}

-- | Parse any 'Word16'.
anyWord16 :: Parser r s e Word16
anyWord16 = withAnyWord16# (\w# -> pure (W16# w#))
{-# inline anyWord16 #-}

-- | Skip any 'Word16'.
anyWord16_ :: Parser r s e ()
anyWord16_ = () <$ anyWord16
{-# inline anyWord16_ #-}

-- | Parse any 'Word32'.
anyWord32 :: Parser r s e Word32
anyWord32 = withAnyWord32# (\w# -> pure (W32# w#))
{-# inline anyWord32 #-}

-- | Skip any 'Word32'.
anyWord32_ :: Parser r s e ()
anyWord32_ = () <$ anyWord32
{-# inline anyWord32_ #-}

-- | Parse any 'Word64'.
anyWord64 :: Parser r s e Word64
anyWord64 = withAnyWord64# (\w# -> pure (W64# w#))
{-# inline anyWord64 #-}

-- | Skip any 'Word64'.
anyWord64_ :: Parser r s e ()
anyWord64_ = () <$ anyWord64
{-# inline anyWord64_ #-}

-- | Parse any 'Word'.
anyWord :: Parser r s e Word
anyWord = withAnyWord64# (\w# -> pure (W# w#))
{-# inline anyWord #-}

-- | Skip any 'Word'.
anyWord_ :: Parser r s e ()
anyWord_ = () <$ anyWord
{-# inline anyWord_ #-}

--------------------------------------------------------------------------------

-- | Parse any 'Int8'.
anyInt8 :: Parser r s e Int8
anyInt8 = withAnyInt8# (\i# -> pure (I8# i#))
{-# inline anyInt8 #-}

-- | Parse any 'Int16'.
anyInt16 :: Parser r s e Int16
anyInt16 = withAnyInt16# (\i# -> pure (I16# i#))
{-# inline anyInt16 #-}

-- | Parse any 'Int32'.
anyInt32 :: Parser r s e Int32
anyInt32 = withAnyInt32# (\i# -> pure (I32# i#))
{-# inline anyInt32 #-}

-- | Parse any 'Int64'.
anyInt64 :: Parser r s e Int64
anyInt64 = withAnyInt64# (\i# -> pure (I64# i#))
{-# inline anyInt64 #-}

-- | Parse any 'Int'.
anyInt :: Parser r s e Int
anyInt = withAnyInt64# (\i# -> pure (I# i#))
{-# inline anyInt #-}

--------------------------------------------------------------------------------

-- | Parse any 'Word16' (little-endian).
anyWord16le :: Parser r s e Word16
anyWord16le = anyWord16
{-# inline anyWord16le #-}

-- | Parse any 'Word16' (big-endian).
anyWord16be :: Parser r s e Word16
anyWord16be = withAnyWord16# (\w# -> pure (W16# (byteSwap16'# w#)))
{-# inline anyWord16be #-}

-- | Parse any 'Word32' (little-endian).
anyWord32le :: Parser r s e Word32
anyWord32le = anyWord32
{-# inline anyWord32le #-}

-- | Parse any 'Word32' (big-endian).
anyWord32be :: Parser r s e Word32
anyWord32be = withAnyWord32# (\w# -> pure (W32# (byteSwap32'# w#)))
{-# inline anyWord32be #-}

-- | Parse any 'Word64' (little-endian).
anyWord64le :: Parser r s e Word64
anyWord64le = anyWord64
{-# inline anyWord64le #-}

-- | Parse any 'Word64' (big-endian).
anyWord64be :: Parser r s e Word64
anyWord64be = withAnyWord64# (\w# -> pure (W64# (byteSwap# w#)))
{-# inline anyWord64be #-}

--------------------------------------------------------------------------------

-- | Parse any 'Int16' (little-endian).
anyInt16le :: Parser r s e Int16
anyInt16le = anyInt16
{-# inline anyInt16le #-}

-- | Parse any 'Int16' (big-endian).
anyInt16be :: Parser r s e Int16
anyInt16be = withAnyWord16# (\w# -> pure (I16# (word16ToInt16# (byteSwap16'# w#))))
{-# inline anyInt16be #-}

-- | Parse any 'Int32' (little-endian).
anyInt32le :: Parser r s e Int32
anyInt32le = anyInt32
{-# inline anyInt32le #-}

-- | Parse any 'Int32' (big-endian).
anyInt32be :: Parser r s e Int32
anyInt32be = withAnyWord32# (\w# -> pure (I32# (word32ToInt32# (byteSwap32'# w#))))
{-# inline anyInt32be #-}

-- | Parse any 'Int64' (little-endian).
anyInt64le :: Parser r s e Int64
anyInt64le = anyInt64
{-# inline anyInt64le #-}

-- | Parse any 'Int64' (big-endian).
anyInt64be :: Parser r s e Int64
anyInt64be = withAnyWord64# (\w# -> pure (I64# (word2Int# (byteSwap# w#))))
{-# inline anyInt64be #-}
