---
layout: post
title: "Nominal Types are Structural Types"
categories: [type systems, functional programming]
published: false
---

I've had a bit of a revelation. I first encountered this idea in a comment somewhere on [Lambda the Ultimate][ltu]. I can't locate the exact URL, so I'm documenting it here while it's still fresh in my head:

*Nominal Types = Structural Types + Uniquely Generated Types*

Assume we have the following built into a language (excuse the nonstandard notation):

* Sum types: `T1 + T2`
* Product types: `T1 * T2`
* Recursive types: `Rec[a]. T`
* Unit type: `1`
* Empty type: `0`

So let's say we want to define the natural numbers. We have:

{% highlight haskell %}
type Nat = Rec[nat]. 1 + nat
{% endhighlight haskell %}

This means that `Nat` is a recursive type that is either a single value or another `Nat`. `1` represents the value zero (yeah, I know), and the successor is represented by the recursion.

Some possible values:
{% highlight haskell %}
let zero = InL 1 -- InL means "In the left side of T1 + T2"
let one = InR (InL 1) -- InR means "In the right side of T1 + T2"
let two = InR (InR (InL 1))
let three = ... -- and so on
{% endhighlight %}

But wait. What if we want (for some contrived reason) to redefine the naturals such that the two types are distinct?
{% highlight haskell %}
type Nat2 = Rec[nat]. 1 + nat

let zero2 = InL 1
...
{% endhighlight %}

If we're using a structural type system, then zero2 and zero are equivalent: they produce values that are both `Nat` and `Nat2`. Indeed, given a `Nat` or `Nat2`, you can easily pass one to a function that expects another. For encapsulation, we do not want this.

Let's introduce a symbol type, denoted `` `T ``, which contains one value, also denoted `` `T ``. We can now differentiate the two types of natural numbers:

{% highlight haskell %}
type Nat = (Rec[nat]. `Nat * (1 + nat))
type Nat2 = (Rec[nat]. `Nat2 * (1 + nat))

zero = (`Nat, InL 1)
one = (`Nat, InR (`Nat, 1))

zero2 = (`Nat2, InL 1)
one2 = (`Nat2, InR (`Nat2, 1))
{% endhighlight %}

Excellent! Except that it's overly verbose, and `InL 1` is a terrible identifier for zero. We can easily amend this with some form of pattern synonyms. Also, we can make the type definitions easier on the eyes with some syntax sugar, perhaps in the style Haskell's `data` declarations.

So why would we want to do this anyways? For one, I think it may simplify a language's underlying type theory. More importantly, it makes it easier to do datatype generic programming. In Haskell, we can define all sorts of datatypes, but we have to bend over backwards with `deriving Generic` and the multitude of types in `GHC.Generics` in order to write operations that work over general types.

[ltu]: http://lambda-the-ultimate.org/ "Lambda the Ultimate"
