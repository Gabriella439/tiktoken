{-# LANGUAGE BlockArguments #-}

module Main where

import qualified Data.ByteString as ByteString
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Silver as Silver
import qualified Tiktoken

main :: IO ()
main = do
    Tasty.defaultMain do
        let actualTokens input encoding = do
                bytes <- ByteString.readFile input
                case Tiktoken.toRanks encoding bytes of
                    Nothing     -> fail "Encoding failed"
                    Just tokens -> return tokens

        let normalTokens  = actualTokens "tasty/sample/input.txt"
        let specialTokens = actualTokens "tasty/special/input.txt"

        let render = Text.unlines . map (Text.pack . show) . Foldable.toList
                
        Tasty.testGroup "tests"
            [ Tasty.testGroup "golden"
                [ Silver.goldenVsAction "r50k_base" "tasty/sample/r50k_base.golden" (normalTokens Tiktoken.r50k_base) render
                , Silver.goldenVsAction "p50k_base" "tasty/sample/p50k_base.golden" (normalTokens Tiktoken.p50k_base) render
                , Silver.goldenVsAction "cl100k_base" "tasty/sample/cl100k_base.golden" (normalTokens Tiktoken.cl100k_base) render
                , Silver.goldenVsAction "o200k_base" "tasty/sample/o200k_base.golden" (normalTokens Tiktoken.o200k_base) render
                ]
            , Tasty.testGroup "special tokens"
                [ Silver.goldenVsAction "r50k_base" "tasty/special/r50k_base.golden" (specialTokens Tiktoken.r50k_base) render
                , Silver.goldenVsAction "p50k_base" "tasty/special/p50k_base.golden" (specialTokens Tiktoken.p50k_base) render
                , Silver.goldenVsAction "p50k_edit" "tasty/special/p50k_edit.golden" (specialTokens Tiktoken.p50k_edit) render
                , Silver.goldenVsAction "cl100k_base" "tasty/special/cl100k_base.golden" (specialTokens Tiktoken.cl100k_base) render
                , Silver.goldenVsAction "o200k_base" "tasty/special/o200k_base.golden" (specialTokens Tiktoken.o200k_base) render
                ]
            ]
