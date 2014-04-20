% A wreq tutorial

# Installation

To use the `wreq` package, simply use `cabal`, the standard Haskell
package management command.

~~~~
cabal update
cabal install -j --disable-tests wreq
~~~~

Depending on how many prerequisites you already have installed, and
what your Cabal configuration looks like, the build may take a few
minutes: a few seconds for `wreq`, and the rest for its dependencies.


# Interactive usage

We'll run our examples interactively via the `ghci` shell.

~~~~
$ ghci
~~~~

To start using `wreq`, we import the
[`Network.Wreq`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html)
module.

~~~~ {.haskell}
ghci> import Network.Wreq
ghci> r <- get "http://httpbin.org/get"
ghci> :type r
r :: Response ByteString
~~~~

The variable `r` above is the
[`Response`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#t:Response)
from the server.


## Working with string-like types

Complex Haskell libraries and applications have to deal fluently with
Haskell's three main string types: `String` ("legacy"), `Text`, and
`ByteString` (mostly used for binary data, sometimes ASCII).

To write string literals without having to always provide a conversion
function, we use the `OverloadedStrings` language extension.

Throughout the rest of this tutorial, we'll assume that you have
enabled `OverloadedStrings` in `ghci`:

~~~~ {.haskell}
ghci> :set -XOverloadedStrings
~~~~

If you're using `wreq` from a Haskell source file, put a pragma at the
top of your file:

~~~~ {.haskell}
{-# LANGUAGE OverloadedStrings #-}
~~~~


# A quick lens backgrounder

The `wreq` package makes heavy use of Edward Kmett's
[`lens`](https://lens.github.io/) package to provide a clean,
consistent API.

~~~~ {.haskell}
ghci> import Control.Lens
~~~~

While `lens` has a vast surface area, the portion that you must
understand in order to productively use `wreq` is tiny.

A lens provides a way to focus on a portion of a Haskell value. For
example, the `Response` type has a
[`responseStatus`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:responseStatus)
lens, which focuses on the status information returned by the server.

~~~~ {.haskell}
ghci> r ^. responseStatus
Status {statusCode = 200, statusMessage = "OK"}
~~~~

The
[`^.`](http://hackage.haskell.org/package/lens/docs/Control-Lens-Getter.html#v:-94-.)
operator takes a value as its first argument, a lens as its second,
and returns the portion of the value focused on by the lens.

We compose lenses using function composition, which allows us to
easily focus on part of a deeply nested structure.

~~~~ {.haskell}
ghci> r ^. responseStatus . statusCode
200
~~~~

We'll have more to say about lenses as this tutorial proceeds.


# Changing default behaviours

While `get` is convenient and easy to use, there's a lot more power
available to us.

For example, if we want to add parameters to the query string of a
URL, we will use the
[`getWith`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:getWith)
function.  The `*With` family of functions all accept an
[`Options`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#t:Options)
parameter that allow changes from the library's default behaviours.

~~~~ {.haskell}
ghci> import Data.Aeson.Lens (_String, key)
ghci> let opts = defaults & param "foo" .~ ["bar", "quux"]
ghci> r <- getWith opts "http://httpbin.org"
ghci> r ^. responseBody . key "url" . _String
"http://httpbin.org/get?foo=bar&foo=quux"
~~~~

(We'll talk more about `key` and `_String` below.)

The default parameters for all queries is represented by the variable
[`defaults`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:defaults).
(In fact, `get` is defined simply as `getWith defaults`.)

Here's where we get to learn a little more about lenses.

In addition to *getting* a value from a nested structure, we can also
*set* (edit) a value within a nested structure, which makes an
identical copy of the structure except for the portion we want to
modify.

The `&` operator is just function application with its operands
reversed, so the function is on the right and its parameter is on the
left.

~~~~ {.haskell}
parameter & functionToApply
~~~~

The
[`.~`](http://hackage.haskell.org/package/lens/docs/Control-Lens-Setter.html#v:.-126-)
 operator turns a lens into a setter function, with the lens
on the left and the new value on the right.

~~~~ {.haskell}
param "foo" .~ ["bar", "quux"]
~~~~

The
[`param`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:param)
lens focuses on the values associated with the given key in the query
string.

~~~~ {.haskell}
param :: Text -> Lens' Options [Text]
~~~~

The reason we allow for a list of values instead of just a single
value is simply that this is completely legitimate. For instance, in
our example above we generate the query string `foo=bar&foo=quux`.

If you use non-ASCII characters in a `param` key or value, they will
be encoded as UTF-8 before being URL-encoded, so that they can be
safely transmitted over the wire.


# Accessing the body of a response

The
[`responseBody`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:responseBody)
lens gives us access to the body of a response.

~~~~ {.haskell}
ghci> r <- get "http://httpbin.org/get"
ghci> r ^. responseBody
"{\n  \"headers\": {\n    \"Accept-Encoding\": \"gzip"{-...-}
~~~~

The response body is a raw lazy
[`ByteString`](http://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Lazy.html#t:ByteString).


## JSON responses

We can use the
[`asJSON`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:asJSON)
function to convert a response body to a Haskell value that implements
the
[`FromJSON`](http://hackage.haskell.org/package/aeson/docs/Data-Aeson-Types.html#t:FromJSON)
class.

~~~~ {.haskell}
ghci> import Data.Map as Map
ghci> import Data.Aeson (Value)
ghci> type Resp = Response (Map String Value)
ghci> r <- asJSON =<< get "http://httpbin.org/get" :: IO Resp
ghci> Map.size (r ^. responseBody)
4
~~~~

(Notice that we have to tell `ghci` exactly what target type we are
expecting. In a real Haskell program, the correct return type will
usually be inferred automatically, making an explicit type signature
unnecessary in most cases.)

If the response is not `application/json`, or we try to convert to an
incompatible Haskell type, a
[`JSONError`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#t:JSONError)
exception will be thrown.

~~~~ {.haskell}
ghci> type Resp = Response [Int]
ghci> r <- asJSON =<< get "http://httpbin.org/get" :: IO Resp
*** Exception: JSONError "when expecting a [a], encountered Object instead"
~~~~


## Convenient JSON traversal

The `lens` package provides some extremely useful functions for
traversing JSON structures without having to either build a
corresponding Haskell type or traverse a `Value` by hand.

The first of these is
[`key`](http://hackage.haskell.org/package/lens/docs/Data-Aeson-Lens.html#v:key),
which traverses to the named key in a JSON object.

~~~~ {.haskell}
ghci> import Data.Aeson.Lens (key)
ghci> r <- get "http://httpbin.org/get"
ghci> r ^? responseBody . key "url"
Just (String "http://httpbin.org/get")
~~~~

Notice our use of the
[`^?`](http://hackage.haskell.org/package/lens-4.1.2/docs/Control-Lens-Fold.html#v:-94--63-)
operator here. This is like `^.`, but it allows for the possibility
that an access might fail---and of course there may not be a key named
`"url"` in our object.

That said, our result above has the type `Maybe Value`, so it's quite
annoying to work with. This is where the `_String` lens comes in.

~~~~ {.haskell}
ghci> import Data.Aeson.Lens (_String, key)
ghci> r <- get "http://httpbin.org/get"
ghci> r ^. responseBody . key "url" . _String
"http://httpbin.org/get"
~~~~

If the key exists, and is a `Value` with a `String` constructor,
`_String` gives us back a regular `Text` value with all the wrappers
removed; otherwise it gives an empty value. Notice what happens as we
switch between `^?` and `^.` in these examples.

~~~~ {.haskell}
ghci> r ^. responseBody . key "fnord" . _String
""
ghci> r ^? responseBody . key "fnord" . _String
Nothing
ghci> r ^? responseBody . key "url" . _String
Just "http://httpbin.org/get"
~~~~