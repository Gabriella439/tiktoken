{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}

-- | You can use this module to convert back and forth between a `ByteString`
--   and its corresponding tokens using an existing encoding like `cl100k_base`
--   or `o200k_base`
--
--   Example usage:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- import "Tiktoken" (`o200k_base`, toTokens, toRanks)
--
-- main :: `IO` ()
-- main = do
--     -- `Just` [\"El\",\" perro\",\" come\",\" las\",\" man\",\"z\",\"anas\"]
--     `print` (`toTokens` `o200k_base` \"El perro come las manzanas\")
--
--     -- `Just` [4422,96439,3063,1996,873,89,14457]
--     `print` (`toRanks` `o200k_base` \"El perro come las manzanas\")
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

      -- * Tokenization
    , toTokens
    , toRanks
    , toTokensAndRanks

      -- * Detokenization
    , fromTokens
    , fromRanks
    ) where

import Control.Applicative (many)
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.IntMap (IntMap, Key)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Text (Text)
import Data.Void (Void)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Prelude hiding (lookup)
import System.FilePath ((</>))
import Text.Megaparsec (ParseErrorBundle, ParsecT)
import Text.RawString.QQ (r)

import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64.Encoding
import qualified Data.ByteString.Char8 as Char8
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.IO as Text.IO
import qualified Paths_tiktoken as Paths
import qualified System.IO.Unsafe as Unsafe
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Megaparsec.Lexer
import qualified Text.Regex.PCRE.Light as Regex

{-| This is an efficient internal representation of an encoding like
    @cl100k_base@, @p50k_edit@, or @o200k_base@
-}
data Encoding = Encoding
    { encode :: HashMap ByteString Int
    , decode :: IntMap ByteString
    , specialTokens :: Map ByteString Int
    , regex :: ByteString
    } deriving stock (Generic)
      deriving anyclass (NFData)

parseToken :: ParsecT Void Text m (Int, ByteString)
parseToken = do
    base64Text <- Megaparsec.takeWhileP (Just "Base64 character") (/= ' ')

    let base64Bytes = Text.Encoding.encodeUtf8 base64Text

    token <- case Base64.Encoding.decodeBase64Untyped base64Bytes of
        Left text -> fail (Text.unpack text)
        Right token -> return token

    _ <- Megaparsec.Char.char ' '

    rank <- Megaparsec.Lexer.decimal

    _ <- Megaparsec.Char.char '\n'

    return (rank, token)

-- | Create an `Encoding` from regular expression and an ordered set of tokens
tokensToEncoding
    :: ByteString
    -- ^ Regular expression used for coarse-grained splitting of the input
    -> IntMap ByteString
    -- ^ The tokens in sequential order of their token IDs
    -> Encoding
tokensToEncoding regex decode = Encoding{..}
  where
    encode = HashMap.fromList (map swap (IntMap.toList decode))
      where
        swap (rank, token) = (token, rank)

    specialTokens = mempty

-- | Parse an encoding from the @.tiktoken@ file format
tiktokenToEncoding
    :: ByteString
    -- ^ Regular expression used for coarse-grained splitting of the input
    -> Text
    -- ^ The contents of the @.tiktoken@ file
    -> Either (ParseErrorBundle Text Void) Encoding
tiktokenToEncoding regex text = Megaparsec.runParser parser "" text
  where
    parser = do
        keyValues <- many parseToken
        pure (tokensToEncoding regex (IntMap.fromList keyValues))

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

loadEncoding :: FilePath -> ByteString -> Map ByteString Int -> IO Encoding
loadEncoding file regex specialTokens = do
    dataDirectory <- Paths.getDataDir

    text <- Text.IO.readFile (dataDirectory </> file)

    encoding <- case tiktokenToEncoding regex text of
        Left exception -> Exception.throwIO exception
        Right encoding -> return encoding

    return (addSpecialTokens specialTokens encoding)

-- | @r50k_base@ `Encoding`
r50k_base :: Encoding
r50k_base =
    Unsafe.unsafePerformIO
        (loadEncoding "r50k_base.tiktoken" regex [ (_ENDOFTEXT, 50256) ])
  where
    regex =
        [r|'(?:[sdmt]|ll|ve|re)| ?\p{L}+| ?\p{N}+| ?[^\s\p{L}\p{N}]+|\s+(?!\S)|\s+|]
{-# NOINLINE r50k_base #-}

-- | @p50k_base@ `Encoding`
p50k_base :: Encoding
p50k_base =
    Unsafe.unsafePerformIO
        (loadEncoding "p50k_base.tiktoken" regex [ (_ENDOFTEXT, 50256) ])
  where
    regex =
        [r|'(?:[sdmt]|ll|ve|re)| ?\p{L}+| ?\p{N}+| ?[^\s\p{L}\p{N}]+|\s+(?!\S)|\s+|]
{-# NOINLINE p50k_base #-}

-- | @p50k_edit@ `Encoding`
p50k_edit :: Encoding
p50k_edit =
    Unsafe.unsafePerformIO
        (loadEncoding "p50k_base.tiktoken"
            regex
            [ (_ENDOFTEXT , 50256)
            , (_FIM_PREFIX, 50281)
            , (_FIM_MIDDLE, 50282)
            , (_FIM_SUFFIX, 50283)
            ] 
        )
  where
    regex =
        [r|'(?:[sdmt]|ll|ve|re)| ?\p{L}+| ?\p{N}+| ?[^\s\p{L}\p{N}]+|\s+(?!\S)|\s+|]
{-# NOINLINE p50k_edit #-}

-- | @cl100k_base@ `Encoding`
cl100k_base :: Encoding
cl100k_base =
    Unsafe.unsafePerformIO
        (loadEncoding "cl100k_base.tiktoken"
            regex
            [ (_ENDOFTEXT  , 100257)
            , (_FIM_PREFIX , 100258)
            , (_FIM_MIDDLE , 100259)
            , (_FIM_SUFFIX , 100260)
            , (_ENDOFPROMPT, 100276)
            ]
        )
  where
    regex =
        [r|'(?i:[sdmt]|ll|ve|re)|[^\r\n\p{L}\p{N}]?+\p{L}+|\p{N}{1,3}| ?[^\s\p{L}\p{N}]++[\r\n]*|\s*[\r\n]|\s+(?!\S)|\s+|]
{-# NOINLINE cl100k_base #-}

-- | @o200k_base@ `Encoding`
o200k_base :: Encoding
o200k_base =
    Unsafe.unsafePerformIO
        (loadEncoding "o200k_base.tiktoken"
            regex
            [ (_ENDOFTEXT  , 199999)
            , (_ENDOFPROMPT, 200018)
            ]
        )
  where
    -- Note: This is not the same as the upstream pattern:
    --
    -- https://github.com/openai/tiktoken/blob/c0ba74c238d18b4824c25f3c27fc8698055b9a76/tiktoken_ext/openai_public.py#L101-L111
    --
    -- The reason why is that the `pcre-light` package doesn't treat an
    -- ideographic space (U+3000) as whitespace whereas Python's `regex` package
    -- does, so all of the rules below have to basically replace all occurrences
    -- of `\s` with `\s\x{3000}`.  There might be other discrepancies, but
    -- that's the only one I've found so far.
    regex =
        Char8.intercalate "|"
            [ [r|[^\r\n\p{L}\p{N}]?[\p{Lu}\p{Lt}\p{Lm}\p{Lo}\p{M}]*[\p{Ll}\p{Lm}\p{Lo}\p{M}]+(?i:'s|'t|'re|'ve|'m|'ll|'d)?|]
            , [r|[^\r\n\p{L}\p{N}]?[\p{Lu}\p{Lt}\p{Lm}\p{Lo}\p{M}]+[\p{Ll}\p{Lm}\p{Lo}\p{M}]*(?i:'s|'t|'re|'ve|'m|'ll|'d)?|]
            , [r|\p{N}{1,3}|]
            , [r| ?[^\s\x{3000}\p{L}\p{N}]+[\r\n/]*|]
            , [r|[\s\x{3000}]*[\r\n]+|]
            , [r|[\s\x{3000}]+(?![^\s\x{3000}])|]
            , [r|[\s\x{3000}]+|]
            ]
{-# NOINLINE o200k_base #-}

minimumBy :: (a -> a -> Ordering) -> IntMap a -> Maybe (Int, a)
minimumBy comparison intMap
    | IntMap.null intMap =
        Nothing
    | otherwise =
        Just (List.minimumBy (comparison `on` snd) (IntMap.toList intMap))

drop1 :: [a] -> [a]
drop1 (_ : xs) = xs
drop1      []  = []

{-| This is basically the same thing as `Maybe Int` except with an `Ord`
    instance that treats `Ranked` values as less than `Unranked` values
-}
data Ranked = Ranked Int | Unranked
    deriving (Eq, Ord)

data Chunk = Chunk
    { rank  :: Int
      -- ^ Rank of this chunk
    , rank2 :: Ranked
      -- ^ Rank of this chunk combined with the next chunk
    }

{-| This corresponds to the @_byte_pair_merge@ function in the upstream @tiktoken@
    package:

    https://github.com/openai/tiktoken/blob/c0ba74c238d18b4824c25f3c27fc8698055b9a76/src/lib.rs#L18-L74

    The intermediate data structure is an `IntMap` instead of a
    `Data.Vector.Vector` but other than that the algorithm is essentially
    identical.
-}
bytePairEncode
    :: HashMap ByteString Int -> ByteString -> Maybe [(Int, ByteString)]
bytePairEncode hashMap bytes
    | Just rank <- HashMap.lookup bytes hashMap =
        Just [ (rank, bytes) ]
    | ByteString.null bytes =
        pure []
    | otherwise = do
        -- In practice this should always return a `Just` because all of
        -- OpenAI's encodings are defined for all bytes, but in theory the user
        -- could create an `Encoding` that doesn't satisfy that invariant, so
        -- we still need to handle that case.
        let lookupByte :: Word8 -> Maybe Int
            lookupByte word8 = HashMap.lookup (ByteString.singleton word8) hashMap

        let toChunk w0 w1 = do
                rank <- lookupByte w0

                let rank2 = lookupSlice (ByteString.pack [ w0, w1 ])

                pure Chunk{ rank, rank2 }

        initChunks <- do
            sequence (ByteString.zipWith toChunk bytes (ByteString.tail bytes))

        lastChunk <- do
            rank <- lookupByte (ByteString.last bytes)

            pure Chunk{ rank, rank2 = Unranked }

        {- Unlike the upstream `tiktoken` we do not use `Vector (Key, Ranked)`
           as our intermediate datastructure, but rather something more like
           `IntMap Ranked` (technically `IntMap Chunk`, which is just a tiny
           optimization).

           This makes it cheaper to delete keys without having to rebuild a
           `Vector` each time, but at the expense of neighbor lookups (e.g.
           `lookupLT` / `lookupGT`) being more expensive.
        -}
        let initialMap :: IntMap Chunk
            initialMap =
                IntMap.fromList (zip [0 ..] (initChunks <> [ lastChunk ]))

        let keyValues = IntMap.toAscList (loop initialMap)

        pure do
            let adapt (index, Chunk{ rank }) nextIndex =
                    (rank, slice index nextIndex)

            zipWith adapt keyValues (drop1 (map fst keyValues) <> [ size ])
  where
    size :: Int
    size = ByteString.length bytes

    lookupSlice :: ByteString -> Ranked
    lookupSlice b = case HashMap.lookup b hashMap of
        Nothing  -> Unranked
        Just int -> Ranked int

    slice :: Int -> Int -> ByteString
    slice begin end = ByteString.take (end - begin) (ByteString.drop begin bytes)

    loop :: IntMap Chunk -> IntMap Chunk
    loop chunks0 = case minimumBy (Ord.comparing rank2) chunks0 of
        Just (index, Chunk{ rank2 = Ranked rank }) -> loop chunks3
          where
            chunks1 = rerank index rank chunks0

            chunks2 = case IntMap.lookupLT index chunks1 of
                Just (prevIndex, Chunk{ rank = prevRank }) ->
                    rerank prevIndex prevRank chunks1
                _ ->
                    chunks1

            chunks3 = case IntMap.lookupGT index chunks2 of
                -- In theory we should never hit the `Nothing` case here because
                -- the `rank2` field can only be `Ranked` if there is a `Chunk`
                -- following this one.
                Nothing ->
                    error "Tiktoken.bytePairEncode: Internal error - a ranked byte pair is missing the second byte in the pair"
                Just (nextIndex, _) ->
                    IntMap.delete nextIndex chunks2

        _ ->
            chunks0

    rerank :: Key -> Int -> IntMap Chunk -> IntMap Chunk
    rerank index0 rank chunks = IntMap.insert index0 newChunk chunks
      where
        maybeIndex3 = do
            (index1, _) <- IntMap.lookupGT index0 chunks

            (index2, _) <- IntMap.lookupGT index1 chunks

            pure case IntMap.lookupGT index2 chunks of
                Just (index3, _) -> index3
                Nothing          -> size

        rank2 = case maybeIndex3 of
            Nothing     -> Unranked
            Just index3 -> lookupSlice (slice index0 index3)

        newChunk = Chunk{ rank, rank2 }

{-| Split a `ByteString` into smaller `ByteString`s, each of which are
    successive longest possible matches to the provided regular expression
-}
splitUsingRegex
    :: ByteString
    -- ^ Regex to match
    -> ByteString
    -- ^ Bytes to split into chunks
    -> Maybe [ByteString]
splitUsingRegex pattern = loop Prelude.id
  where
    loop diff bytes
        | ByteString.null bytes =
            Just (diff [])
        | otherwise =
            case Regex.match regex bytes [ Regex.exec_no_utf8_check ] of
                Just (prefix : _) ->
                    let suffix = ByteString.drop (ByteString.length prefix) bytes
                    in  loop (diff . (prefix :)) suffix
                _ -> Nothing

    regex = Regex.compile pattern [ Regex.utf8, Regex.anchored ]

{-| Divide up the input into coarse-grained chunks based on the provided splitting
    regular expression before doing the final byte pair encoding
-}
bytePairEncodeWithSplitting :: Encoding -> ByteString -> Maybe [(Int, ByteString)]
bytePairEncodeWithSplitting Encoding{..} bytes = do
    chunks <- splitUsingRegex regex bytes

    tokenss <- traverse (bytePairEncode encode) chunks

    pure (concat tokenss)

{-| Split a `ByteString` into smaller `ByteString`s separated by the given
    separator
-}
splitOnSeparator
    :: ByteString
    -- ^ Separator
    -> ByteString
    -- ^ `ByteString` to separate
    -> NonEmpty ByteString
splitOnSeparator separator initialBytes = initialPrefix :| loop initialSuffix
  where
    split = ByteString.breakSubstring separator

    (initialPrefix, initialSuffix) = split initialBytes

    loop bytes
        | ByteString.null bytes = []
        | otherwise             = prefix : loop suffix
      where
        rest = ByteString.drop (ByteString.length separator) bytes

        (prefix, suffix) = split rest

{-| Use an `Encoding` to tokenize a `ByteString` into smaller `ByteString`s and
    their associated ranks

    This only fails if you provide an `Encoding` that cannot rank all possible
    1-byte sequences
-}
toTokensAndRanks :: Encoding -> ByteString -> Maybe [(Int, ByteString)]
toTokensAndRanks encoding@Encoding{..} initialBytes =
    foldr cons nil (Map.toList specialTokens) initialBytes
  where
    cons (token, rank) tokenizer bytes = do
        fmap joinSegments (traverse tokenizer (splitOnSeparator token bytes))
      where
        joinSegments =
              concat
            . NonEmpty.toList
            . NonEmpty.intersperse [ (rank, token) ]

    nil bytes = bytePairEncodeWithSplitting encoding bytes

{-| Use an `Encoding` to tokenize a `ByteString` into smaller `ByteString`s

    This only fails if you provide an `Encoding` that cannot rank all possible
    1-byte sequences
-}
toTokens :: Encoding -> ByteString -> Maybe [ByteString]
toTokens = fmap (fmap (fmap (fmap snd))) toTokensAndRanks

{-| Use an `Encoding` to tokenize a `ByteString` into ranks

    This only fails if you provide an `Encoding` that cannot rank all possible
    1-byte sequences
-}
toRanks :: Encoding -> ByteString -> Maybe [Int]
toRanks = fmap (fmap (fmap (fmap fst))) toTokensAndRanks

{-| Combine a sequence of `ByteString` tokens back into a `ByteString`

    This is just a synonym for @"Data.ByteString".`ByteString.concat`@ (no
    `Encoding` necessary), provided solely for consistency/convenience.
-}
fromTokens :: [ByteString] -> ByteString
fromTokens = ByteString.concat

{-| Convert a sequence of ranks back into a `ByteString`

    This will fail if you supply any ranks which are not recognized by the
    `Encoding`.
-}
fromRanks :: Encoding -> [Int] -> Maybe ByteString
fromRanks Encoding{..} vector = fmap fromTokens (traverse lookup vector)
  where
    lookup rank = IntMap.lookup rank decode
