Hello and welcome to this simple example of using the Medea library,
a library for decoding XML documents to Haskell data types.

> {-# LANGUAGE OverloadedStrings #-}

> module Main where

The XML documents are from the `Text.XML` module from the
`xml-conduit` package. (we will also need the Text library)

> import Data.Text
> import Text.XML

Let's also import the `Medea` library

> import Medea

Good, so in order to be able to decode to Haskell types we need
some types to decode to. The first example will be a simple record.

> data Rec = Rec
>   { foo :: Text
>   , bar :: Text
>   , baz :: Bool
>   } deriving Show

Which is encoded in xml as follows (For Rec "F" "B" True)

<rec>
  <foo>F</foo>
  <bar>B</bar>
  <baz>true</baz>
</rec>

Which we in Haskell could write like this

> example :: Document
> example = parseLBS_ def
>   "<rec><foo>F</foo><bar>B</bar><baz>true</baz></rec>"

So now time to write a decoder! The `recDecoder` below is
a decoder for the `Rec` type. So a `Decoder a` is basically
a function from `Element` to `a`, that can fail. There are
some combinators to be able to build up the decoder in a
somewhat composeable way.

> name :: Text -> Name
> name n = Name n Nothing Nothing

> recDecoder :: Decoder Rec
> recDecoder = isTag (name "rec") $
>   Rec <$> child fooDecoder
>       <*> child barDecoder
>       <*> child bazDecoder

So the `isTag` combinator checks if the current element is a
tag of the given name. In the example here we use the `name`
function from above to create the name of the tag.

Currently the `isTag` combinator takes a "continuation" decoder
I'm not sure I want that in the future or if I want to do this
is another way. Anyway, `Decoder` is an `Applicative` functor
so we can easily build up the `Rec` type. We use the decoders
defined below but we use the `child` combinator for the children
of the `Rec`. So the `child` combinator checks if the decoder works
for one (and only one) of the children of the current `Element`.
So because it tries all children, the decoder should hopefully try
to fail fast (like use a `isTag`) I don't know how to make this
more obvious, maybe have a special `ChildDecoder` type that make
this more obvious??

> fooDecoder :: Decoder Text
> fooDecoder = isTag (name "foo") $ uniqueText

> barDecoder :: Decoder Text
> barDecoder = isTag (name "bar") $ uniqueText

Both of the `foo` and `bar` field are just tags and then one single
text node. So the `uniqueText` combinator just checks that there
is one (and only one) text node as a child to the current `Element`.

> bazDecoder :: Decoder Bool
> bazDecoder = isTag (name "baz") $
>   uniqueText `validate` parseBool
>   where
>     parseBool :: Text -> Validated String Bool
>     parseBool "true" = pure True
>     parseBool "false" = pure False
>     parseBool x = invalid $ "Can't parse bool: " ++ unpack x

The `baz` field is a bit more involved since we need to parse a `Bool`
Here we are using `uniqueText` to get a decoder for the `Text` which
we than use `validate` to parse the `Text` to a `Bool`. But since this
can fail we return a `Validated String Bool`.

`Validated` is similar to `Either` except that it has a different
`Applicative` instance (which can not be turned into a `Monad`). And
yes, this type can't be turned into a `Monad` even though there is a
function:

```haskell
andThen :: Validated e a -> (a -> Validated e b) -> Validated e b
```

But don't you dare make the type an instance of `Monad` because then
the Haskell police will come and arrest you for breaking the `Monad`
law.

> main :: IO ()
> main = print $ decode example recDecoder

So the `decode` function takes a `Document` and a `Decoder a`
and will give back a `Validated String a`. Which luckily for us have
a `Show` instance so we can display it here.

In the future I'm hoping to get better error messages (like having
XPath expressions showing locations etc.) But that will happen in the
`Future`...


