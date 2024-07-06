{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}

-- | You can use this module to convert back and forth between a `ByteString`
--   and its corresponding tokens using an existing encoding like @cl100k_base@.
--
--   Example usage:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- import qualified "Control.Exception" as Exception
-- import qualified "Data.Text.IO" as Text.IO
-- import qualified "Tiktoken"
--
-- main :: `IO` ()
-- main = do
--     text <- Text.IO.`Data.Text.IO.readFile` \"cl100k\_base.tiktoken\"
--
--     case `textToEncoding` text of
--         `Left` errorBundle ->
--             Exception.`Exception.throwIO` errorBundle
--         `Right` encoding ->
--             let input = \"El perro come las manzanas\"
--
--             -- Just [\"El\",\" per\",\"ro\",\" come\",\" las\",\" man\",\"zan\",\"as\"]
--             print (Tiktoken.`Tiktoken.toTokens` encoding input)
--
--             -- Just [6719,824,299,2586,5252,893,50226,300]
--             print (Tiktoken.`Tiktoken.toTokenIDs` encoding input)
-- @
module Tiktoken
    ( -- * Types
      Encoding
    , textToEncoding
    , addSpecialTokens

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
import Data.Text (Text)
import Data.Trie (Trie)
import Data.Vector (MVector, Vector, (!?))
import Data.Void (Void)
import Prelude hiding (id)
import Text.Megaparsec (ParseErrorBundle, ParsecT)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64.Encoding
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Trie as Trie
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as Vector.Mutable
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec.Char

{-| This is an efficient internal representation of an encoding like
    @cl100k_base@ or @p50k_base@.
-}
data Encoding = Encoding{ encode :: Trie Int, decode :: Vector ByteString }

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

-- | Parse an encoding from the `.tiktoken` file format
textToEncoding :: Text -> Either (ParseErrorBundle Text Void) Encoding
textToEncoding text = do
    decode <- Vector.createT (Megaparsec.runParserT parseDecode "" text)

    let encode = Trie.fromList (Vector.toList (Vector.imap adapt decode))
          where
            adapt index token = (token, index)

    return Encoding{..}

-- | Add special tokens to a base `Encoding`
addSpecialTokens :: Vector ByteString -> Encoding -> Encoding
addSpecialTokens tokens Encoding{ encode = oldEncode, decode = oldDecode } =
        Encoding{..}
  where
    encode = Trie.unionR oldEncode newTrie
      where
        newTrie = Trie.fromList (Vector.toList (Vector.imap adapt tokens))
          where
            adapt index token = (token, index + Vector.length oldDecode)

    decode = oldDecode <> tokens

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

{-| Use an `Encoding` to tokenize a `ByteString` into smaller `ByteString`s

    This will fail if you provide a `Encoding` that does not handle all
    possible `ByteString`s.
-}
toTokens :: Encoding -> ByteString -> Maybe (Vector ByteString)
toTokens = tokenizeWith (\bytes _ -> bytes)

{-| Use an `Encoding` to tokenize a `ByteString` into token IDs

    This will fail if you provide a `Encoding` that does not handle all
    possible `ByteString`s.
-}
toTokenIDs :: Encoding -> ByteString -> Maybe (Vector Int)
toTokenIDs = tokenizeWith (\_ id -> id)

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
