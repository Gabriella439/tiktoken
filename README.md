# `tiktoken`

This is a Haskell implementation of
[`tiktoken`](https://github.com/openai/tiktoken), but just the tokenization
logic.  In other words, given an existing encoding (like `cl100k_base`) you
can tokenize a string (into smaller strings or token IDs).

This means that you can't (yet) use this package to create your own new
encodings, but you can use it to consume encodings.  In particular, this comes
in handy for prompt engineering where you want to use as much of the available
prompt tokens as possible (which requires accurately counting tokens).

Encoding speed is ≈7.5-9 MB/s on an M1 MacBook Pro (using only one core since
this package does not yet support parallel tokenization):

```
All
  Encode 10 MB of Wikipedia
    r50k_base:   OK (3.33s)
      1.102 s ±  43 ms
    p50k_base:   OK (3.35s)
      1.107 s ±  62 ms
    p50k_edit:   OK (3.56s)
      1.178 s ±  33 ms
    cl100k_base: OK (3.96s)
      1.308 s ±  29 ms
    o200k_base:  OK (3.97s)
      1.298 s ±  49 ms
```
