{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE RecordWildCards    #-}

-- | You can use this module to convert back and forth between a `ByteString`
--   and its corresponding tokens using an existing encoding like @cl100k_base@
--   or @o200k_base@
--
--   Example usage:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- import "Tiktoken" (`o200k_base`, toTokens, toTokenIDs)
--
-- main :: `IO` ()
-- main = do
--     -- `Just` [\"El\",\" per\",\"ro\",\" come\",\" las\",\" man\",\"zana\",\"s\"]
--     `print` (`toTokens` `o200k_base` \"El perro come las manzanas\")
--
--     -- `Just` [4422,96439,3063,1996,873,90333,82]
--     `print` (`toTokenIDs` `o200k_base` \"El perro come las manzanas\")
-- @
module Tiktoken
    ( -- * Encoding
      Encoding
    , tiktokenToEncoding
    , addSpecialTokens

      -- * Stock Encodings
    , r50k_base
    , p50k_base
    , p50k_edit
    , cl100k_base
    , o200k_base
      -- ** by model name


      -- * Tokenization
    , toTokens
    , toTokenIDs

      -- * Detokenization
    , fromTokens
    , fromTokenIDs
    ) where

import Control.Applicative ((<|>))
import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Text (Text)
import Data.Trie (Trie)
import Data.Vector (MVector, Vector, (!?))
import Data.Void (Void)
import Prelude hiding (id)
import System.FilePath ((</>))
import Text.Megaparsec (ParseErrorBundle, ParsecT)

import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64.Encoding
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.IO as Text.IO
import qualified Data.Trie as Trie
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as Vector.Mutable
import qualified Paths_tiktoken as Paths
import qualified System.IO.Unsafe as Unsafe
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec.Char

{-| This is an efficient internal representation of an encoding like
    @cl100k_base@, @p50k_edit@, or @o200k_base@
-}
data Encoding = Encoding
    { encode :: Trie Int
    , decode :: Vector ByteString
    , specialTokens :: Map ByteString Int
    }

parseToken :: ParsecT Void Text m ByteString
parseToken = do
    base64Text <- Megaparsec.takeWhileP (Just "Base64 character") (/= ' ')

    let base64Bytes = Text.Encoding.encodeUtf8 base64Text

    token <- case Base64.Encoding.decodeBase64Untyped base64Bytes of
        Left text -> fail (Text.unpack text)
        Right token -> return token

    -- We don't bother parsing the token ID because the tokens are always stored
    -- in sequential order by token ID.  We could *not* assume this but this
    -- would not only make the parsing slower but it would also require using
    -- a `HashMap` instead of a `Vector` to handle potential gaps in the token
    -- ID sequence.  It's much more efficient to make this simplifying
    -- assumption.

    _ <- Megaparsec.takeWhileP (Just "Base64 character") (/= '\n')

    _ <- Megaparsec.Char.char '\n'

    return token

parseDecode :: ParsecT Void Text (ST s) (MVector s ByteString)
parseDecode = do
    -- 100,000 is the size of the largest commonly-used encoding at the time of
    -- this writing (`cl100k_base`) and it's not that expensive to pre-allocate
    -- a `Vector` that big, so let's go wild and start with a large allocation.
    let initialSize = 100_000

    initialVector <- lift (Vector.Mutable.new initialSize)

    let loop index vector
            | index < size = do
                let success = do
                        token <- parseToken

                        lift (Vector.Mutable.write vector index token)

                        loop (index + 1) vector

                let failure = do
                        return (Vector.Mutable.take index vector)

                success <|> failure
                
            | otherwise = do
                largerVector <- lift (Vector.Mutable.grow vector size)

                loop index largerVector
          where
            size = Vector.Mutable.length vector

    loop 0 initialVector

-- | Create an `Encoding` from an ordered set of tokens
tokensToEncoding :: Vector ByteString -> Encoding
tokensToEncoding decode = Encoding{..}
  where
    encode = Trie.fromList (Vector.toList (Vector.imap adapt decode))
      where
        adapt index token = (token, index)

    specialTokens = mempty

-- | Parse an encoding from the @.tiktoken@ file format
tiktokenToEncoding :: Text -> Either (ParseErrorBundle Text Void) Encoding
tiktokenToEncoding text =
    fmap tokensToEncoding
        (Vector.createT (Megaparsec.runParserT parseDecode "" text))

-- | Add special tokens to a base `Encoding`
addSpecialTokens :: Map ByteString Int -> Encoding -> Encoding
addSpecialTokens tokens Encoding{ specialTokens = oldSpecialTokens, .. } =
    Encoding{ specialTokens = Map.union tokens oldSpecialTokens, .. }

_ENDOFTEXT :: ByteString
_ENDOFTEXT = "<|endoftext|>"

_FIM_PREFIX :: ByteString
_FIM_PREFIX = "<|fim_prefix|>"

_FIM_MIDDLE :: ByteString
_FIM_MIDDLE = "<|fim_middle|>"

_FIM_SUFFIX :: ByteString
_FIM_SUFFIX = "<|fim_suffix|>"

_ENDOFPROMPT :: ByteString
_ENDOFPROMPT = "<|endofprompt|>"

loadEncoding :: FilePath -> Map ByteString Int -> IO Encoding
loadEncoding file specialTokens = do
    dataDirectory <- Paths.getDataDir

    text <- Text.IO.readFile (dataDirectory </> file)

    encoding <- case tiktokenToEncoding text of
        Left exception -> Exception.throwIO exception
        Right encoding -> return encoding

    return (addSpecialTokens specialTokens encoding)

-- | @r50k_base@ `Encoding`
r50k_base :: Encoding
r50k_base =
    Unsafe.unsafePerformIO
        (loadEncoding "r50k_base.tiktoken" [ (_ENDOFTEXT, 50256) ])
{-# NOINLINE r50k_base #-}

-- | @p50k_base@ `Encoding`
p50k_base :: Encoding
p50k_base =
    Unsafe.unsafePerformIO
        (loadEncoding "p50k_base.tiktoken" [ (_ENDOFTEXT, 50256) ])
{-# NOINLINE p50k_base #-}

-- | @p50k_edit@ `Encoding`
p50k_edit :: Encoding
p50k_edit =
    Unsafe.unsafePerformIO
        (loadEncoding "p50k_base.tiktoken"
            [ (_ENDOFTEXT , 50256)
            , (_FIM_PREFIX, 50281)
            , (_FIM_MIDDLE, 50282)
            , (_FIM_SUFFIX, 50283)
            ] 
        )
{-# NOINLINE p50k_edit #-}

-- | @cl100k_base@ `Encoding`
cl100k_base :: Encoding
cl100k_base =
    Unsafe.unsafePerformIO
        (loadEncoding "cl100k_base.tiktoken"
            [ (_ENDOFTEXT  , 100257)
            , (_FIM_PREFIX , 100258)
            , (_FIM_MIDDLE , 100259)
            , (_FIM_SUFFIX , 100260)
            , (_ENDOFPROMPT, 100276)
            ]
        )
{-# NOINLINE cl100k_base #-}

-- | @o200k_base@ `Encoding`
o200k_base :: Encoding
o200k_base =
    Unsafe.unsafePerformIO
        (loadEncoding "o200k_base.tiktoken"
            [ (_ENDOFTEXT  , 199999)
            , (_ENDOFPROMPT, 200018)
            ]
        )
{-# NOINLINE o200k_base #-}

splitOn :: ByteString -> ByteString -> NonEmpty ByteString
splitOn separator initialBytes = initialPrefix :| loop initialSuffix
  where
    split = ByteString.breakSubstring separator

    (initialPrefix, initialSuffix) = split initialBytes

    loop bytes
        | ByteString.null bytes = []
        | otherwise = prefix : loop suffix
      where
        rest = ByteString.drop (ByteString.length separator) bytes

        (prefix, suffix) = split rest

-- | Tokenizer that ignores special tokens
tokenizeWith
    :: (ByteString -> Int -> a) -> Encoding -> ByteString -> Maybe (Vector a)
tokenizeWith fromTokenAndID Encoding{..} initialBytes =
    Vector.createT do
        vector <- Vector.Mutable.new (ByteString.length initialBytes)

        -- Carefully note that we never check that the index is in bounds
        -- because we pre-allocate a vector with on element per input byte and
        -- the number of tokens can never exceed the number of input bytes. This
        -- also means that we never need to grow the vector.
        let loop bytes index
                | ByteString.null bytes =
                    return (Just (Vector.Mutable.take index vector))

                | Just (token, id, rest) <- Trie.match encode bytes = do
                    Vector.Mutable.write vector index (fromTokenAndID token id)

                    loop rest (index + 1)

                | otherwise =
                    return Nothing

        loop initialBytes 0

-- | Tokenizer that is special-token-aware
tokenizeWithSpecial
    :: (ByteString -> Int -> a) -> Encoding -> ByteString -> Maybe (Vector a)
tokenizeWithSpecial fromTokenAndID encoding@Encoding{..} initialBytes =
    foldr cons nil (Map.toList specialTokens) initialBytes
  where
    cons (token, id) tokenize bytes = do
        fmap joinSegments (traverse tokenize (splitOn token bytes))
      where
        joinSegments =
              Vector.concat
            . NonEmpty.toList
            . NonEmpty.intersperse [ fromTokenAndID token id ]

    nil bytes = tokenizeWith fromTokenAndID encoding bytes

{-| Use an `Encoding` to tokenize a `ByteString` into smaller `ByteString`s

    This will fail if you provide a `Encoding` that does not handle all
    possible `ByteString`s.
-}
toTokens :: Encoding -> ByteString -> Maybe (Vector ByteString)
toTokens = tokenizeWithSpecial (\bytes _ -> bytes)

{-| Use an `Encoding` to tokenize a `ByteString` into token IDs

    This will fail if you provide a `Encoding` that does not handle all
    possible `ByteString`s.
-}
toTokenIDs :: Encoding -> ByteString -> Maybe (Vector Int)
toTokenIDs = tokenizeWithSpecial (\_ id -> id)

{-| Combine a sequence of `ByteString` tokens back into a `ByteString`

    This is just a glorified @"Data.ByteString".`ByteString.concat`@ (no
    `Encoding` necessary), provided solely for consistency/convenience.
-}
fromTokens :: Vector ByteString -> ByteString
fromTokens vector = ByteString.concat (Vector.toList vector)

{-| Convert a sequence of token IDs back into a `ByteString`

    This will fail if you supply any token IDs which are not recognized by the
    `Encoding`.
-}
fromTokenIDs :: Encoding -> Vector Int -> Maybe ByteString
fromTokenIDs Encoding{..} vector = fmap fromTokens (traverse (decode !?) vector)
