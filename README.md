# `tiktoken`

This is a Haskell implementation of
[`tiktoken`](https://github.com/openai/tiktoken), but just the tokenization
logic.  In other words, given an existing encoding (like `cl100k_base`) you
can tokenize a string (into smaller strings or token IDs).

This means that you can't (yet) use this package to create your own new
encodings, but you can use it to consume encodings.  In particular, this comes
in handy for prompt engineering where you want to use as much of the available
prompt tokens as possible (which requires accurately counting tokens).
