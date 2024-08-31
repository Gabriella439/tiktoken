# `tiktoken`

This is a Haskell implementation of
[`tiktoken`](https://github.com/openai/tiktoken), but just the tokenization
logic.  In other words, given an existing encoding (like `cl100k_base`) you
can tokenize a string (into smaller strings or token ranks).

This means that you can't (yet) use this package to create your own new
encodings, but you can use it to consume encodings.  In particular, this comes
in handy for prompt engineering where you want to use as much of the available
prompt tokens as possible (which requires accurately counting tokens).

Encoding speed is ≈2.6-3.1 MB/s on an M1 MacBook Pro (using only one core since
this package does not yet support parallel tokenization):

```
All
  Encode 10 MB of Wikipedia
    r50k_base:   OK (23.88s)
      3.356 s ± 151 ms
    p50k_base:   OK (10.39s)
      3.445 s ±  31 ms
    p50k_edit:   OK (11.13s)
      3.693 s ± 240 ms
    cl100k_base: OK (11.16s)
      3.685 s ± 143 ms
    o200k_base:  OK (11.01s)
      3.648 s ± 134 ms
```
