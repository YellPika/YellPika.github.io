---
layout: post
title: "Improving Monadic Regions"
categories: [haskell, programming, regions]
published: true
---

[Monadic regions][reg] are a great idea, but the current implementation suffers
from what I think is a major flaw: you can't pass resources to a new thread. I'm
going to rectify that.

Let's rework the API from scratch. What we want is a `Monad` that ensures that
resources can't escape their scope. To achieve this, we utilize `RankNTypes`,
and a phantom parameter:

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
useHandle res = if refCount res <= 0 then error "..." else return ()
{% endhighlight %}

So far so good. How about child scopes?

{% highlight haskell %}
-- Any resources allocated in the region parameter will
-- automatically be released when the region exits.
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
decremented when the parent scope exits instead. But what if we have multiple
nested scopes? Which scope does it escape to? How do we ensure that the
reference count is decremented when the correct scope exits?

Allowing resources to escape through arbitrarily nested scopes would require
type level lists, and therefore `UndecidableInstances` to search them. Let's
restrict `capture` and `escape` to only work over one level of scope. To do
this, we need to add another phantom type parameter to indicate the parent
scope.

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
escape :: Handle p c -> RegionT p c m (Handle p' p)
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
resources from the parent scope are _automatically captured_. If we increment
the reference counts of the captured resources _before_ the computation is
passed to `transform`, things like this become perfectly safe:

{% highlight haskell %}
runRegionT $ do
    handle <- newHandle ...
    reset forkIO $ do
        threadDelay aReallyLongTime
        useHandle handle

    useHandle handle

    -- Root region exits before reset region does, but handle
    -- isn't released until the forked region exits.
{% endhighlight %}

And that's it! The actual implementation of this scheme can get a little hairy
at times (think nested generators), so I'll describe that in a later post.

The Whole API
-------------

{% highlight haskell %}
data RegionT p c m a = ...

runRegionT :: (forall p c. RegionT p c m a) -> m a
scope :: (forall c. RegionT p c m a) -> RegionT p' p m a
reset :: (m a -> m b) -> (forall p. RegionT p c m a) -> RegionT p' c m b

data Handle p c = ...

newHandle :: m () -> RegionT p c m (Handle p c)
useHandle :: Handle p' c -> RegionT p c m ()

capture :: Handle p' p -> RegionT p c m (Handle p c)
escape :: Handle p c -> RegionT p c m (Handle p' p)
{% endhighlight %}

[reg]: http://hackage.haskell.org/package/regions
