---
layout: post
title: "Improving Monadic Regions"
categories: [haskell, programming, regions]
---

[Monadic regions][reg] are a great idea, but the current implementation suffers
from what I think is a major flaw: you can't pass resources to a new thread. I'm
going to rectify that.

Let's rework the semantics from scratch. What we want is a `Monad` that ensures
that resources can't escape their scope. To achieve this, we utilize
`RankNTypes`, and a phantom parameter:

{% highlight haskell %}
-- s is the scope variable
-- m is the inner monad (this is a monad transformer)
-- a is the return type.
data RegionT s m a = ...

-- The universal quantifier ensures that any types
-- parameterized by s can't appear outside of the region.
runRegionT :: (forall s. RegionT s m a) -> m a
runRegionT region = ...

data Handle s = ...

-- action is executed when the handle's reference count hits zero.
newHandle :: m () -> RegionT s m (Handle s)
newHandle action = ...

useHandle :: Handle s -> RegionT s m ()
useHandle res = if refCount res < 0 then error "..." else return ()
{% endhighlight %}

So far so good. How about child scopes?

{% highlight haskell %}
-- Any resources allocated in the parameter region will be released when the
-- region exits.
scope :: (forall s. RegionT s m a) -> RegionT s' m a
scope region = ...
{% endhighlight %}

What happens if we want to make use of a resource from the surrounding scope? We
need some way to import resources:

{% highlight haskell %}
capture :: Handle s' -> RegionT s m (Handle s)
capture resource = ...
{% endhighlight %}

Conceptually, `capture` will increment the reference count for `resource`, and
then decrement it when the surrounding scope terminates. We can define a
similar function for escaping scopes:

{% highlight haskell %}
escape :: Handle s -> RegionT s m (Handle s')
escape resource = ...
{% endhighlight %}

`escape` also increments the reference count for `resource`, but it will be
decremented when the parent scope exits instead. There's a small problem with
this: what if we have multiple nested scopes? Which scope does it escape to?
How do we ensure that the reference count is decremented when the correct scope
exits?

Allowing resources to escape through arbitrarily nested scopes would require
type level lists, and therefore `UndecidableInstances` to search them. Let's
restrict `escape` to only allow escaping up one level of scope.

We redefine everything as follows:

{% highlight haskell %}
-- p is the parent scope
-- c is the current (child) scope
data RegionT p c m a = ...

runRegionT :: (forall p c. RegionT p c m a) -> m a
scope :: (forall c. RegionT p c m a) -> RegionT p' p m a

data Handle p c = ...

newHandle :: m () -> RegionT p c m (Handle p c)

-- Handles can be used whenever the current context
-- is the same. We allow the parent context to differ
-- for reasons which will be revealed soon...
useHandle :: Handle p' c -> RegionT p c m ()

capture :: Handle p' p -> RegionT p c m (Handle p c)
release :: Handle p c -> RegionT p c m (Handle p' p)
{% endhighlight %}

This formulation is already quite different from the existing implementation.
Incidentally, it's perfect for what I'm about to do.

I define a function named `reset`:

{% highlight haskell %}
reset :: (m a -> m b) -> (forall p. RegionT p c m a) -> RegionT p' c m b
reset transform region = ...
{% endhighlight %}

`reset` takes a region computation, unwraps it and then passes it to
`transform`. It retains the context of the parent scope. This implies that
resources from the parent scope are _automatically acquired_. Reference counts
are incremented _before_ the computation is passed to `transform`, so things
like this become perfectly safe:

{% highlight haskell %}
runRegionT $ do
    handle <- newHandle ...
    reset forkIO $ do
        threadDelay aReallyLongTime
        useHandle handle
{% endhighlight %}

And that's it! The actual implementation of this scheme can get a little hairy
at times, so I'll detail that in a later post.

The Whole API
-------------

{% highlight haskell %}
data RegionT p c m a = ...

runRegionT :: (forall p c. RegionT p c m a) -> m a
scope :: (forall c. RegionT p c m a) -> RegionT p' p m a
reset :: (forall p. RegionT p c m a) -> RegionT p' c m a

data Handle p c = ...

newHandle :: m () -> RegionT p c m (Handle p c)
useHandle :: Handle p' c -> RegionT p c m ()

capture :: Handle p' p -> RegionT p c m (Handle p c)
release :: Handle p c -> RegionT p c m (Handle p' p)
{% endhighlight %}

[reg]: http://hackage.haskell.org/package/regions
