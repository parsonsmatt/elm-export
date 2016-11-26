# PureScript Export

[![Build Status](https://travis-ci.org/parsonsmatt/purescript-export.svg)](https://travis-ci.org/parsonsmatt/purescript-export)

Create PureScript classes and JSON decoders from Haskell DataTypes.

Inspired by (and mostly copied from) Kris Jenkins' excellent `elm-export` library.

## Installation

PureScript Export is [available on Hackage](http://hackage.haskell.org/package/purescript-export).

## Usage

To use this library, you must first make the types you want to export
implement `PureScriptType`. This is easy. Just derive `Generic`, and then
we can automatically generate the `PureScriptType` instance for you. Here's
an example with a `Person` type:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Db where

import GHC.Generics
import PureScript

data Person =
  Person {id   :: Int
         ,name :: Maybe String}
  deriving (Show,Eq,Generic,PureScriptType)
```

That's it for the type. Now you'll want to write a main that generates
the PureScript source code:

```haskell
module Main where

import Db
import PureScript
import Data.Proxy

spec :: Spec
spec = Spec ["Db", "Types"]
            ["import Json.Decode exposing (..)"
            ,"import Json.Decode.Extra exposing (apply,date)"
            ,toPureScriptTypeSource (Proxy :: Proxy Person)
            ,toPureScriptDecoderSource (Proxy :: Proxy Person)]

main :: IO ()
main = specsToDir [spec] "some/where/output"
```

Run this and the directory `some/where/output` will be created, and
under that the PureScript source file `Db/Types.purescript` will be found.

All the hard work here is done by `toPureScriptTypeSource` and
`toPureScriptDecoderSource`. The `Spec` code is just wrapping to make it easy
to create a complete PureScript file from the meat that `PureScriptType` gives
you.

## Development

You will need [Stack](https://github.com/commercialhaskell/stack).

### Building

```sh
stack build
```

### Testing

```sh
stack test --file-watch
```

## Change Log

### V0.3.0.0
* Renamed `ToPureScriptType` to `PureScriptType`, for brevity.

### V0.2.0.0
* Added Encoders (thanks to [Matthew Bray](https://github.com/mattjbray))

### V0.1.0.0
* Initial release.

## Status

Alpha. The author is using it in production, but it is not yet
expected to work for every reasonable case.

There are some Haskell datatypes that cannot be represented in
PureScript. Obviously we will not support those. But there are some which are
legal Haskell and legal PureScript, but we do not yet generate. Please send
examples, PRs and code-suggestions!

## Contributors

* [Matthew Bray](https://github.com/mattjbray)

## License

Copyright © 2015-2016 Kris Jenkins

Distributed under the Eclipse Public License.

## See Also

[PureScript Bridge](https://hackage.haskell.org/package/purescript-bridge) is a
different implementation of the same goal. That project uses Template
Haskell, this one uses GHC Generics.
