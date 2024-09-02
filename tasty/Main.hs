{-# LANGUAGE BlockArguments #-}

module Main where

import Data.Text (Text)
import Test.QuickCheck.Instances ()
import Test.Tasty.QuickCheck (Property, (===))
import Tiktoken (Encoding)

import qualified Data.ByteString as ByteString
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.QuickCheck as Tasty.QuickCheck
import qualified Test.Tasty.Silver as Silver
import qualified Tiktoken

roundTrip :: Encoding -> Text -> Property
roundTrip encoding text = Just bytes === result
  where
    bytes = Text.Encoding.encodeUtf8 (Text.take 100 text)

    result = do
        ranks <- Tiktoken.toRanks encoding bytes

        Tiktoken.fromRanks encoding ranks

main :: IO ()
main = do
    Tasty.defaultMain do
        let actual encode input encoding = do
                bytes <- ByteString.readFile input
                case encode encoding bytes of
                    Nothing     -> fail "Encoding failed"
                    Just tokens -> return tokens

        let normalRanks  = actual Tiktoken.toRanks "tasty/sample/input.txt"
        let specialRanks = actual Tiktoken.toRanks "tasty/special/input.txt"

        let normalTokens =
                actual Tiktoken.toTokens "tasty/tokenization/input.txt"

        let render :: Show a => [a] -> Text
            render = Text.unlines . map (Text.pack . show) . Foldable.toList

        Tasty.testGroup "tests"
            [ Tasty.testGroup "golden"
                [ Silver.goldenVsAction "r50k_base" "tasty/sample/r50k_base.golden" (normalRanks Tiktoken.r50k_base) render
                , Silver.goldenVsAction "p50k_base" "tasty/sample/p50k_base.golden" (normalRanks Tiktoken.p50k_base) render
                , Silver.goldenVsAction "cl100k_base" "tasty/sample/cl100k_base.golden" (normalRanks Tiktoken.cl100k_base) render
                , Silver.goldenVsAction "o200k_base" "tasty/sample/o200k_base.golden" (normalRanks Tiktoken.o200k_base) render
                ]
            , Tasty.testGroup "special tokens"
                [ Silver.goldenVsAction "r50k_base" "tasty/special/r50k_base.golden" (specialRanks Tiktoken.r50k_base) render
                , Silver.goldenVsAction "p50k_base" "tasty/special/p50k_base.golden" (specialRanks Tiktoken.p50k_base) render
                , Silver.goldenVsAction "p50k_edit" "tasty/special/p50k_edit.golden" (specialRanks Tiktoken.p50k_edit) render
                , Silver.goldenVsAction "cl100k_base" "tasty/special/cl100k_base.golden" (specialRanks Tiktoken.cl100k_base) render
                , Silver.goldenVsAction "o200k_base" "tasty/special/o200k_base.golden" (specialRanks Tiktoken.o200k_base) render
                ]
            , Tasty.testGroup "tokenization"
                [ Silver.goldenVsAction "r50k_base" "tasty/tokenization/r50k_base.golden" (normalTokens Tiktoken.r50k_base) render
                , Silver.goldenVsAction "p50k_base" "tasty/tokenization/p50k_base.golden" (normalTokens Tiktoken.p50k_base) render
                , Silver.goldenVsAction "cl100k_base" "tasty/tokenization/cl100k_base.golden" (normalTokens Tiktoken.cl100k_base) render
                , Silver.goldenVsAction "o200k_base" "tasty/tokenization/o200k_base.golden" (normalTokens Tiktoken.o200k_base) render
                ]
            , Tasty.testGroup "roundtrip"
                [ Tasty.QuickCheck.testProperty "r50k_base" (roundTrip Tiktoken.r50k_base)
                , Tasty.QuickCheck.testProperty "p50k_base" (roundTrip Tiktoken.p50k_base)
                , Tasty.QuickCheck.testProperty "p50k_edit" (roundTrip Tiktoken.p50k_edit)
                , Tasty.QuickCheck.testProperty "cl100k_base" (roundTrip Tiktoken.cl100k_base)
                , Tasty.QuickCheck.testProperty "o200k_base" (roundTrip Tiktoken.cl100k_base)
                ]

            ]
