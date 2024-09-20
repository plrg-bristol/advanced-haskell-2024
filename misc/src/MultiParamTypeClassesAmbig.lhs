> {-# language MultiParamTypeClasses #-}
> {-# language InstanceSigs #-}
> {-# language FunctionalDependencies #-}
> module MultiParamTypeClassesAmbig where

Q: What is the 'dilemma' of multi-parameter type classes?
A: Ambiguity

Let's get a better idea of the problem by looking at an example.
Say we want to make an `Add` type class which can automatically convert
between different types and do the sensible thing. We could write it like this:

> class Add a b c where
>   add :: a -> b -> c

If we add an `Int` and a `Float`, we want to automatically convert the `Int` to
a `Float` and return a `Float`. We could write the instance like this:

> instance Add Int Float Float where
>   add :: Int -> Float -> Float
>   add x y = fromIntegral x + y

Great! So now if we write `add 1 1.5` we should get `2.5`. Very sensible.
Let's try it:

< eg = add 1 1.5

> -- Error!
> -- Ambiguous type variable ‘b0’ arising from the literal ‘1.5’
> --  prevents the constraint ‘(Fractional b0)’ from being solved.

Hm, that's annoying, but that's just because the literals `1` and `1.5` are
overloaded. If we ensure they're `Int` and `Float`, it'll be fine:

< eg2 = add (1 :: Int) (1.5 :: Float)

> -- Ambiguous type variable ‘c0’ arising from a use of ‘add’
> --   prevents the constraint ‘(Add Int Float c0)’ from being solved.
> --   Probable fix: use a type annotation to specify what ‘c0’ should be.
> --   Potentially matching instance:
> --     instance Add Int Float Float

Oh. That's annoying. Will it work if we specify the output type as well?

> eg3 :: Float
> eg3 = add (1 :: Int) (1.5 :: Float)
> -- >>> eg3
> -- 2.5

It does! This seems like more hassle than it's worth, but at least we can
use it now:

< eg4 :: Float
< eg4 = add (2 :: Int) (add (1 :: Int) (1.5 :: Float))

> -- Ambiguous type variable ‘b0’ arising from a use of ‘add’
> --   prevents the constraint ‘(Add Int b0 Float)’ from being solved.
> --   Probable fix: use a type annotation to specify what ‘b0’ should be.
> --   Potentially matching instance:
> --     instance Add Int Float Float
> -- In the expression: add (2 :: Int) (add (1 :: Int) (1.5 :: Float))
>
> -- Ambiguous type variable ‘b0’ arising from a use of ‘add’
> -- prevents the constraint ‘(Add Int Float b0)’ from being solved.
> -- Probable fix: use a type annotation to specify what ‘b0’ should be.
> -- Potentially matching instance:
> --   instance Add Int Float Float
> -- In the second argument of ‘add’, namely
> --   ‘(add (1 :: Int) (1.5 :: Float))’

Ugh! We specified the output of the overall expression, but now
the sub-expression `(add (1 :: Int) (1.5 :: Float))` is ambiguous.
If we look at just the types we specified for `eg3` vs `eg4`
they look like this:

`eg3 = add Int Float = Float`
`eg4 = add Int (add Int Float = ?) = Float`

Any time there is a disconnect between the input types and the output type
like this, it causes big problems for type inference.

But why can't GHC see that `add Int Float` must be a `Float`? It even lists the
potentially matching instances and there's only one: `instance Add Int Float Float`

Here's the rub: Haskell's type classes are *open world*

Open world, as opposed to closed world, means there's nothing stopping new
instances being added later. For example, it's perfectly valid to add the
following instance:

> instance Add Int Float Int where
>   add :: Int -> Float -> Int
>   add x y = x + round y

Even if you as a library author don't add it, there's nothing stopping users
of your library from adding it later. That's the 'open world' model.

If we look at the error for `eg2` again with this extra instance available,
it's clear that the output is genuinely ambiguous:

< eg2 = add (1 :: Int) (1.5 :: Float)

> -- Ambiguous type variable ‘c0’ arising from a use of ‘add’
> --   prevents the constraint ‘(Add Int Float c0)’ from being solved.
> --   Probable fix: use a type annotation to specify what ‘c0’ should be.
> --   Potentially matching instances:
> --     instance Add Int Float Float
> --     instance Add Int Float Int

There are a couple of ways around this, but a common and effective solution
is using the `FunctionalDependencies` language extension to modify our
type class:

> class Add' a b c | a b -> c where
>   add' :: a -> b -> c

The `|` indicates the start of a functional dependency, and `a b -> c` can
be read as '`a` and `b` uniquely determine `c``, i.e. for any particular
combination of `a` and `b`, there is only one `c`. This doesn't change how
we write our instance:

> instance Add' Int Float Float where
>   add' :: Int -> Float -> Float
>   add' x y = fromIntegral x + y

But it does prevent us (and any future users) from writing a conflicting instance:

< instance Add' Int Float Int where
<   add' :: Int -> Float -> Int
<   add' x y = x + round y

> -- Functional dependencies conflict between instance declarations:
> --  instance Add' Int Float Float
> --  instance Add' Int Float Int

Functional dependencies therefore let us 'close' the world a little bit and,
in this case, bridge the gap between inputs and outputs:

> eg5 = add' (2 :: Int) (add' (1 :: Int) (1.5 :: Float))

The paper 'A Solution to Haskell's Multi-Parameter Type Class Dilemma' has a
different approach to closing the world, which seems to be 'if GHC would normally
report the type as ambiguous (like with eg2), but there's only one possible
instance, just use that instance'. That does indeed result in less ambiguity
errors, but it also sounds like a brittle solution that could be a bit of a
footgun. GHC maintainers prefer to avoid these kinds of brittle changes to
type class resolution, which is why OverlappingInstances and IncoherentInstances
have been deprecated.
