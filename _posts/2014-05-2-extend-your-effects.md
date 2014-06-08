---
layout: post
title: "Extend Your Effects"
categories: [type systems, functional programming]
published: false
---

Monads are used pervasively throughout Haskell for many things, most notably to
model arbitrary side effects. I would like to propose a significantly better way
to handle effects in a purely functional language such as Haskell.

One existing alternative solution to providing side effects in a pure language
is to use linear types. Haskell does not have linear types - or does it? Indeed,
it is possible to [embed linear types in Haskell][tagless]. However, there is
a problem that comes with any possible embedding: ordinary Haskell functions
cannot be lifted into the embedding. Since ordinary Haskell values are
non-linear, an embedding has no way of ensuring that an arbitrary function does
not copy values. If we wish to make a useful program using linear types, _every_
function must be written in the embedded language. This is impractical, even if
we ignore performance issues.

Monads have the opposite problem. While it's impossible to lift an arbitrary
function into an embedding of linear types, it's also impossible to convert an
arbitrary monadic value into a regular value. That is, there is no general
function of type `Monad m => m a -> a`. While we can't execute side effects in
pure code, we can do the opposite and lift pure functions into impure code.

It is precisely the "lifting pure functions into impure code" part that I
disagree with. Side effects, whether we like it or not, are an integral part of
any useful application. Why do we need `liftM` and `do` syntax and `Applicative`
and god knows what else just to combine pure and effectful code in a meaningful
way? Why can't I just write `putStrLn (getLine ())`?

Does this mean we should allow arbitrary side effects in pure code? Hell no! We
should absolutely separate pure and impure code, but the current monadic
solution suffers from the same fundamental problem that the embedded linear
types solution suffers from: it's an _embedding_. What we really need is proper
integration of effects within the type system.

This has been done plenty of times before: Koka; Disciple; to an extent, Idris'
effect library; FX-87. Effect systems go back even further than Haskell. With a
proper type and effect system, I believe that we can not only make purely
functional programming easier, but also make it significantly more approachable
to the common coder. In particular, the following makes become non-issues:

1. **No `Monad` hurdle**

    Monads tend to be the main stopping point in a new Haskeller's journey.
    Effects are significantly easier to explain. Using `putStrLn`? Make sure
    `Console` shows up in the list of effects in the type signature (if you're
    not already leaving it to be inferred).

2. **No need for lifting**

    No more `liftM`, `ap`, `<*>`, `<$>`, `fmap`, `liftA`, `lift`, `hoist`, or
    what-have-you. Just pass your arguments to functions as you normally would,
    and let the type system infer the rest.

3. **Better performance**

    I'm a little iffy on this one, not being a compiler writer (yet!), but it
    seems like it would be easier to optimize regular procedural code that is
    annotated with effects, rather than traverse the nest of closures that come
    with using `>>=` all over the place.

[tagless]: https://www.fpcomplete.com/user/mutjida/typed-tagless-final-linear-lambda-calculus
