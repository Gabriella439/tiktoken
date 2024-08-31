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
        let actualTokens = do
                bytes <- ByteString.readFile "tasty/sample.txt"
                case Tiktoken.toTokenIDs Tiktoken.cl100k_base bytes of
                    Nothing     -> fail "Encoding failed"
                    Just tokens -> return tokens

        let render = Text.unlines . map (Text.pack . show) . Foldable.toList
                
        Silver.goldenVsAction "golden" "tasty/tokens.golden" actualTokens render
