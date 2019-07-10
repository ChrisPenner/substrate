# substrate

Preprocessor for markdown-ish files which can run GHCi sessions amongst other things

NOTE: This edits your files in-place, use with caution. It's not my fault if it ruins your day :'(

Usage:

```
stack build && stack exec substrate-exe -- file1.md file2.md
```

This will go through both files looking for blocks like this:

    ```repl
    >>> import Data.Char
    >>> x = "hello :)"
    >>> fmap toUpper x
    "HELLO :)"
    ```

Upon seeing one it will run it through ghci and update the output clauses in-place.
