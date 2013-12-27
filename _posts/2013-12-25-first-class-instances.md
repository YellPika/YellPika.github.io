---
layout: post
title: "First Class Instances"
categories: [haskell, programming, typeclasses]
---

In this post I'm going to implement first class instances for typeclasses. Yes, it's [already been done][hsfci], but I find that particular approach problematic for two reasons:

1. Every alternative instance has to deal with the `Proxy` type. This isn't so much of a problem for classes with simple functions (like `show`), but the extra casts get annoying pretty quickly when you have to deal with more complicated functions, such as `>>=`.

2. The solution doesn't generalize that well. You have to create a new `ProxyN` newtype for every type kind (i.e. `Proxy` for kind `*`, `Proxy1` for kind `* -> *`). The `PolyKinds` extension doesn't seem to be enough to alleviate the issue.

I'm going to propose a different solution based on implicit parameters.

Named Typeclass Instances
-------------------------

First of all, I'm going to be making use of these imports and extensions:

{% highlight haskell %}
{-# LANGUAGE
    ConstraintKinds,
    FlexibleContexts,
    FlexibleInstances,
    NoImplicitPrelude,
    PolyKinds,
    RankNTypes,
    RebindableSyntax,
    TypeFamilies,
    UndecidableInstances
  #-}

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Prelude (IO, flip, id, undefined)
import Unsafe.Coerce (unsafeCoerce)

import qualified Control.Monad
import qualified Prelude
{% endhighlight %}

No `ImplicitParams`? I want to be able to specify a "default" type class instance. Unfortunately, the existing implicit parameters extension can't do that, so I'm going to rip something out of the guts of the (awesome) [`implicit-params`][ip] package.

(We don't need the named implicit parameters, so I've renamed and streamlined a few things.)

{% highlight haskell %}
class Class a where
    inst :: a

with :: c -> (Class c => a) -> a
with = flip unsafeCoerce
{% endhighlight %}

We can now sanely make use of the technique described in [Scrap Your Type Classes][sctc]. First, we'll define the records:

{% highlight haskell %}
-- Type class instances are represented with records.
newtype Pointed' t = Pointed' { _return :: forall a. a -> t a }
newtype Functor' t = Functor' { _map :: forall a b. (a -> b) -> t a -> t b }
{% endhighlight %}

With `ConstraintKinds`, we can create some synonyms for bringing instances into scope:

{% highlight haskell %}
type Pointed t = Class (Pointed' t)
type Functor t = Class (Functor' t)
{% endhighlight %}

In particular, we can use constraint synonyms for expressing hierarchies:

{% highlight haskell %}
newtype Applicative' t = Applicative' { _apply :: forall a b. t (a -> b) -> t a -> t b }

type Applicative t = (Pointed t, Functor t, Class (Applicative' t))
{% endhighlight %}

Default instances can be specified with `Class` instances:

{% highlight haskell %}
instance Class (Pointed' IO) where
    inst = Pointed' {
        _return = Prelude.return
    }

instance Class (Functor' IO) where
    inst = Functor' {
        _map = Prelude.fmap
    }

instance Class (Applicative' IO) where
    inst = Applicative' {
        _apply = Control.Monad.ap
    }
{% endhighlight %}

Alternate instances can be specified with plain old values:

{% highlight haskell %}
-- An (idiotic) alternate instance.
dumbPointed :: Pointed' IO
dumbPointed = Pointed' {
    _return = \x -> undefined
}
{% endhighlight %}

Finally, we define the actual class functions:

{% highlight haskell %}
return :: Pointed t => a -> t a
return = _return inst

map :: Functor t => (a -> b) -> t a -> t b
map = _map inst

(<*>) :: Applicative t => t (a -> b) -> t a -> t b
(<*>) = _apply inst
{% endhighlight %}

Usage is straightforward:

{% highlight haskell %}
(<$>) :: Functor t => (a -> b) -> t a -> t b
(<$>) = map

sequence :: Applicative t => [t a] -> t [a]
sequence [] = return []
sequence (x:xs) = (:) <$> x <*> sequence xs

dumbReturn :: a -> IO a
dumbReturn x = with dumbPointed (return x)
{% endhighlight %}

{% highlight haskell %}
ghci> return 3
3
ghci> dumbReturn 3
*** Exception: Prelude.undefined
{% endhighlight %}

Default Implementations
-----------------------

I've demonstrated how to mimic the basics, but we can go further.

Let's look at monads. Monads have two possible definitions: `join` and `bind`. Ideally, the `Monad` typeclass would include definitions for both:

{% highlight haskell %}
class Applicative t => Monad t where
    bind :: (a -> t b) -> t a -> t b
    bind f x = join (map f x)

    join :: t (t a)
    join = bind id
{% endhighlight %}

I've provided default definitions for both functions, so the client code only has to implement one function. However, there's nothing stopping someone from implementing _neither_ of them.

{% highlight haskell %}
instance Monad FooBar
{% endhighlight %}

Obviously, we would get an infinite loop if we tried to use this instance. With our new approach, we can avoid this:

(As a side note, GHC 7.7+ has a MINIMAL pragma, which solves this problem. But that's currently unreleased...)

{% highlight haskell %}
type Monad t = (Applicative t, Class (Monad' t))

data Monad' t = Monad' {
    _bind :: forall a b. (a -> t b) -> t a -> t b,
    _join :: forall a. t (t a) -> t a
}

monadFromBind :: Applicative t => (forall a b. (a -> t b) -> t a -> t b) -> Monad' t
monadFromBind bind' = Monad' {
    _bind = bind',
    _join = bind' id
}

monadFromJoin :: Applicative t => (forall a. t (t a) -> t a) -> Monad' t
monadFromJoin join' = Monad' {
    _bind = \f x -> join' (map f x),
    _join = join'
}

bind :: Monad t => (a -> t b) -> t a -> t b
bind = _bind inst

join :: Monad t => t (t a) -> t a
join = _join inst
{% endhighlight %}

By turning typeclass instances into simple values, we've given ourselves more control over how instances are constructed. Perhaps this can be taken further to ensure that instances satisfy certain laws.

Do Notation
-----------

While we're at it, let's reclaim `do` notation (with `RebindableSyntax`, of course):

{% highlight haskell %}
(>>=) :: Monad t => t a -> (a -> t b) -> t b
(>>=) = flip bind
{% endhighlight %}

Multiparameter Typeclasses, Functional Dependencies, and Associated Types
-------------------------------------------------------------------------

Multiparameter typeclasses come free - just add another parameter to your instance record. On the other hand, functional dependencies are impossible to express directly. We can approximate them using `TypeFamilies`:

{% highlight haskell %}
type MonadRef m = (Monad m, Class (MonadRef' m))

type family Ref m :: * -> *

data MonadRef' m = MonadRef' {
    _newRef :: a -> m (Ref m a),
    _readRef :: Ref m a -> m a,
    _writeRef :: Ref m a -> a -> m ()
}

type instance Ref IO = IORef

instance Class (MonadRef IO) where
    inst = MonadRef' {
        _newRef = newIORef,
        _readRef = readIORef,
        _writeRef = writeIORef
    }

newRef :: MonadRef m => a -> m (Ref m a)
newRef = _newRef inst

readRef :: MonadRef m => Ref m a -> m a
readRef = _readRef inst

writeRef :: MonadRef m => Ref m a -> a -> m ()
writeRef = _writeRef inst
{% endhighlight %}

Quantified Contexts
-------------------

With our new method of declaring type classes, we now have the ability to express [quantified contexts][qc]! First, we'll define a new type `Q` (for quantify):

{% highlight haskell %}
-- c is the class we're implementing
-- t is the type being quantified
newtype Q c t = Q { unQ :: forall a. c (t a) }
{% endhighlight %}

`Q` allows us to declare things like this:

{% highlight haskell %}
type Empty' t = Empty' { _empty :: t }

instance Class (Q Empty' []) where
    inst = Q (Empty' { _empty = [] })

empty :: Class (Semigroup' t) => t
empty = _empty inst

empty2 :: Class (Q Semigroup' t) => (t a, t b)
empty2 = (_empty (unQ inst), _empty (unQ inst))
{% endhighlight %}

Think of `c` like a continuation. By stacking `Q`s, we can achieve an arbitrary level of quantification:

{% highlight haskell %}
higherEmpty :: Class (Q (Q Semigroup') t) => (t a b, t b a)
higherEmpty = (_empty (unQ (unQ inst)), _empty (unQ (unQ inst)))
{% endhighlight %}

Typing `unQ inst` everywhere is a chore. We can alleviate this problem by declaring another instance for `Class`:

{% highlight haskell %}
instance Class (Q c t) => Class (c (t a)) where
    inst = unQ inst
{% endhighlight %}

Now it's possible to use `empty` directly, instead of nested calls to `unQ`:

{% highlight haskell %}
empty2 :: Class (Q Semigroup' t) => (t a, t b)
empty2 = (empty, empty)

higherEmpty :: Class (Q (Q Semigroup') t) => (t a b, t b a)
higherEmpty = (empty, empty)
{% endhighlight haskell %}

Just watch out for overlapping instances.

Conclusion
----------

I think there's more to explore. In particular, I'm concerned about the safety of this method. Am I breaking the the system in a bad way? Another thing to consider is whether or not all the boilerplate can be automated with metaprogramming. Guess I finally have a reason to learn Template Haskell :)

[hsfci]: http://joyoftypes.blogspot.ca/2012/02/haskell-supports-first-class-instances.html
[sctc]: http://www.haskellforall.com/2012/05/scrap-your-type-classes.html
[ip]: http://hackage.haskell.org/package/implicit-params
[qc]: http://www.haskell.org/haskellwiki/Quantified_contexts
