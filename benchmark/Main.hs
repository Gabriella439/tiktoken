{-# LANGUAGE BlockArguments #-}

module Main where

import System.FilePath ((</>))
import Test.Tasty.Bench (Benchmark)
import Tiktoken (Encoding)

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Paths_tiktoken as Paths
import qualified Test.Tasty.Bench as Bench
import qualified Tiktoken

main :: IO ()
main = do
    dataDirectory <- Paths.getDataDir

    let file = dataDirectory </> "wikipedia_10mb.txt"

    Bench.defaultMain
        [ Bench.env (ByteString.readFile file) \text -> do
            let benchmark name encoding = do
                    let loadEncoding = Exception.evaluate (DeepSeq.force encoding)

                    Bench.env loadEncoding \encoding -> do
                        Bench.bench name (Bench.nf (Tiktoken.toTokens encoding) text)

            Bench.bgroup "Encode 10 MB of Wikipedia"
                [ benchmark "r50k_base" Tiktoken.r50k_base
                , benchmark "p50k_base" Tiktoken.p50k_base
                , benchmark "p50k_edit" Tiktoken.p50k_edit
                , benchmark "cl100k_base" Tiktoken.cl100k_base
                , benchmark "o200k_base" Tiktoken.o200k_base
                ]
        ]
