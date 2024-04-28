# mypandoc

Document converter implemented from scratch in Haskell. Aims to be a robust
subset of [`pandoc`](https://pandoc.org).

# usage

> [!WARNING]\
> This tool only supports `markdown`, `json` and `xml`

```sh
./mypandoc -i ifile -f ofmt [-o ofile] [-e ifmt]

    -i ifile  : input file
    -f ofmt   : output format
    -o ofile  : output file (default: stdout)
    -e ifmt   : input format (default: auto)
```

# setup

Clone the repository

```sh
git clone git@github.com:Jabolol/pandoc.git .
```

Build the project

```sh
gmake
```

Run the converter

```sh
./mypandoc -i examples/syntaxe.md -f json -o output/mine.json
```

# overview

This project uses in-house parsers and printers to convert between formats. Full
support between `markdown`, `json` and `xml` is provided. This means that you
can convert to and from any of these formats.

## parser

The main parser type is defined as follows:

```hs
newtype Parser i o = Parser {parse :: i -> Maybe (i, o)}
```

This type is a wrapper around a function that takes an input and returns a
`Maybe` tuple of the remaining input and the output. This allows for the
composition of parsers.

### functor

The `Functor` typeclass is implemented to allow for the mapping of a function
over the output of a parser. This is done by applying the function to the output
of the parser.

```hs
instance Functor (Parser i) where
  fmap :: (a -> b) -> Parser i a -> Parser i b
  fmap f parser = Parser $ fmap (fmap f) . parse parser
```

### applicative

The `Applicative` typeclass is implemented to allow for the chaining of parsers.
This is done by applying the output of one parser to the input of another.

```hs
instance Applicative (Parser i) where
  pure :: a -> Parser i a
  pure x = Parser $ pure . (,x)

  (<*>) :: Parser i (a -> b) -> Parser i a -> Parser i b
  x <*> y = Parser $ \i -> case parse x i of
    Just (i', f) -> fmap f <$> parse y i'
    Nothing -> Nothing
```

### alternative

The `Alternative` typeclass is implemented to allow for the chaining of parsers
with a fallback. This is done by trying the first parser and if it fails, trying
the second parser and so on.

```hs
instance Alternative (Parser i) where
  empty :: Parser i a
  empty = Parser $ const empty

  (<|>) :: Parser i a -> Parser i a -> Parser i a
  x <|> y = Parser $ \i -> parse x i <|> parse y i
```

### monad

The `Monad` typeclass is implemented to allow for the chaining of parsers with
the ability to change the parser based on the output of the previous parser.
This is done by applying a function to the output of a parser and then parsing
the result.

```hs
instance Monad (Parser i) where
  (>>=) :: Parser i a -> (a -> Parser i b) -> Parser i b
  x >>= f = Parser $ \i -> case parse x i of
    Just (i', a) -> parse (f a) i'
    Nothing -> Nothing
```

## result

Take for example the [`xml` parser](./src/XML.hs):

```hs
xTag :: S.Parser String XValue
xTag = do
  _ <- S.char '<'
  n <- A.some (S.matches C.isAlphaNum)
  a <- A.many $ do
    k <- S.spaces *> A.some (S.matches C.isAlphaNum)
    _ <- S.spaces *> S.char '='
    _ <- S.spaces *> S.char '"'
    v <- A.many (S.matches (/= '"'))
    _ <- S.char '"' <* S.spaces
    pure (k, v)
  _ <- S.char '>'
  c <- S.tabs *> A.many (xTag A.<|> xText) <* S.tabs
  _ <- S.string "</" *> S.string n <* S.char '>' <* S.tabs
  pure $ XTag n a c
```

This parser is constructed from smaller parsers that parse individual parts of
the `xml` format. It uses the `Applicative` and `Monad` typeclasses to chain
parsers together to form a larger parser. It also uses the `Alternative`
typeclass to provide fallbacks in case a parser fails, effectively providing
error handling.
