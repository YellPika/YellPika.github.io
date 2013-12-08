---
layout: post
title: "The Perfect Language"
published: false
---

Yes, I went there.

I've been programming for about 8 years. I've used all sorts of languages,
ranging from C# to Prolog to Haskell, and I'm well aware of each language's
respective strengths and weaknesses. At the very least, I'm aware of what I _do_
and _don't_ like.

Perhaps it's about time I got around to designing my own?

Let me be clear that when I say "The Perfect Language", I mean *my* perfect
language, according to my tastes and experiences. Nonetheless, I hope the ideas
I'm about to splurge all over these pages will appeal to someone other than me.

I will update this post periodically. I expect that it will eventually grow into
a full rationale for whatever language comes out of this.

## Goals

What makes a good piece of software? To me, a good software is:

* Reliable
* Maintainable
* Fast

I have ordered these items deliberately. Software is useless if it doesn't work
correctly. I consider maintainability a prerequisite for ensuring correctness
over time. I'm not saying that performance isn't important, but optimizing a
program is much easier (and more gratifying) than debugging one.

I think the design of a programming language should reflect these goals.

## Paradigms

A language's paradigm defines the way ideas are expressed. Object oriented
programmers think in terms of communicating entities, functional programmers
think in terms of transformations, and logic programmers think in terms of
descriptions.

I want something fundamental, something that can express ideas from other
paradigms with ease. The first thing that might come to mind is LISP. I dislike
its syntax and its preference for prefix operators.

I don't consider logic programming fundamental. Logic programming can
essentially be dissolved into the concepts of _backtracking_ and _unification_,
and I've also seen logic programming successfully embedded in Haskell before.

I don't think I need to make a case for C-like procedural programming. It's too
inflexible.

This leaves the object-oriented and functional paradigms. I noticed a while back
that constructs from either can be expressed in the other. For example, consider
a predicate function, `a -> Bool`. We can express this with an interface:

{% highlight csharp linenos %}
public interface IPredicate<T> {
    public bool Invoke(T arg);
}

public struct GreaterThan : IPredicate<int> {
    private int bound;
    
    public GreaterThan(int bound) {
        this.bound = bound;
    }
    
    public bool Invoke(int arg) {
        return arg > bound;
    }
}
{% endhighlight %}

This is unnecessarily verbose. In contrast, an interface can be easily expressed
with records.

{% highlight haskell linenos %}
data Connection = Connector {
    send :: Data -> IO (),
    recv :: IO Data
}

newTCPConnection :: Address -> IO Connection
newTCPConnection address = do
    socket <- newSocket ...
    return Connector {
        send = \data -> ... socket ...
        recv = \data -> ... socket ...
    }
{% endhighlight %}

In the first example, notice how we are attempting to express a smaller concept
(closures) with a larger concept (classes). In the second, we are expressing a
larger concept (classes) with smaller ones (records, closures). Thus, records
and closures are the more fundamental constructs.

While the second example demonstrates polymorphism, it doesn't quite capture the
object-oriented notion of inheritance. Once again, classes are a _monolithic_
concept. Inheritance can be expressed in a functional language by other means,
but we don't need to implement a concept if we don't need it.

## Type System

If correctness is of utmost importance, then the type system is a programmer's
second most powerful tool for achieving that goal (the first being his own
brain). In practice, a type system serves as documentation as well: so it makes
a program more maintainable. For these reasons, I prefer a language with strong,
static types.

Some people complain that type annotations are unnecessarily verbose, and that
type systems are too restrictive. The first problem is easily solved by type
inference. The second is mostly solved by polymorphic types. I would consider
that C++'s templates solve the problem entirely.

I would like to avoid C++ style templates. Constraints on template parameters
are not documented in the type signature, which goes against the idea that
types are a form of documentation.

So how are we to think about types? Haskell attempts to support type-level
programming with constraints and functions (as well as relations, but functional
dependencies are being eschewed in favour of type families). In the interest
of simplicity I would like to stick to one.

I think that constraints are a natural way to think about types. Rather than say
that a value _has_ type _T_, let's say that it's type is _constrained_ to
support whatever _functionality_ is specified by _T_.

\- Anthony Vandikas (YellPika)
