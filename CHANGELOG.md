1.0.2

- [Correctly handle gaps in ranks](https://github.com/Gabriella439/tiktoken/commit/cc82cb192b1f22a2185257b3a1045e2aaf25a4e8)

  The old implementation assumed that encoding don't have gaps in their ranks,
  but some do (especially near the end, typically reserved for special tokens).
  This change fixes the internal implementation to correctly handle those gaps.

- [Fix `o200k_base` regex to match upstream](https://github.com/Gabriella439/tiktoken/commit/fa70dddb431af5a71b243325cf9ef72061721e99)

  The upstream `tiktoken` package uses a flavor of regex that subtly differs
  from the Haskell `pcre-light` package.  Specifically, they differ in whether
  they treat ideographic space (U+3000) as whitespace (which this change fixes).

  There may be other differences yet to be uncovered, but this is the only one
  that has arisen so far when comparing to upstream on a large corpus of text.

1.0.1

- [Small fixes to documentation](https://github.com/Gabriella439/tiktoken/commit/bb0eaf724a8120e7cd2ada2ad09835369bac02bc)

1.0.0

- Initial release
