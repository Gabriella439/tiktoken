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
        let actualTokens encoding = do
                bytes <- ByteString.readFile "tasty/sample.txt"
                case Tiktoken.toRanks encoding bytes of
                    Nothing     -> fail "Encoding failed"
                    Just tokens -> return tokens

        let render = Text.unlines . map (Text.pack . show) . Foldable.toList
                
        Tasty.testGroup "golden"
            [ Silver.goldenVsAction "r50k" "tasty/r50k_base.golden" (actualTokens Tiktoken.r50k_base) render
            , Silver.goldenVsAction "p50k" "tasty/p50k_base.golden" (actualTokens Tiktoken.p50k_base) render
            , Silver.goldenVsAction "cl100k" "tasty/cl100k_base.golden" (actualTokens Tiktoken.cl100k_base) render
            , Silver.goldenVsAction "o200k" "tasty/o200k_base.golden" (actualTokens Tiktoken.o200k_base) render
            ]
