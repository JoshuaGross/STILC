Simply Typed Incremental Lambda Calculus
============================

Simply typed incremental lambda calculus.

It's a lambda calculus with three built-in types: int, bool, arrow. It also has a built-in function, `derive`, that will derive a change function for a given lambda term.

This is a first attempt to implement an incremental lambda-calculus based on the [paper and project by Paolo G. Giarrusso](http://inc-lc.github.io/). It's not very useful yet, it's just a toy. This was also based on Stephen Diehl's simply-typed lambda calculus implementation from ["Write You a Haskell"](http://dev.stephendiehl.com/fun/).

There are many theoretical problems with this implementation. For example, we're not using any canonical name-capturing scheme (like de Bruijn Indexes), so automatic renaming conflicts are likely.

To compile and run:

```shell
$ stack build && stack install
```

Usage:

```haskell
$ stilc

STILC> (\x : Int . \y : Int . y) 1 2
2

STILC> (\x : (Int -> Int). x) (\x : Int . 1) 2
1

STILC> (\x : Int . x) False
Couldn't match expected type 'Int' with actual type: 'Bool'

STILC> (\x : Int . (\y : Int . x))
<<closure (\x . \(y : Int) -> x)>>

STILC> (\x : Int . \y : Int . + x y)
<<closure (\x . \ (y : Int) -> (+ x y))>>

STILC> derive (\x : Int . \y : Int . + x y)
<<closure (\x . \ (dx : Int) -> (\ (y : Int) -> (\ (dy : Int) -> (+ (+ x y) (+ dx dy)))))>>

STILC> (derive (\x : Int . \y : Int . + x y)) 10 0 10 0
<<closure (\x . \ (dx : Int) -> (\ (y : Int) -> (\ (dy : Int) -> (+ (+ x y) (+ dx dy)))))>>
```


License
=======

Released under MIT license.
