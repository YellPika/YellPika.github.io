---
layout: post
title: "The Perfect Language: Priorities"
categories: ["language design"]
published: true
---

This post is part of what will be a series of posts where I discuss language
design. Each post can be considered to be a work in progress, and will be
updated from time to time.

I once thought C# was a great language. I was so immersed in it that I came to
disregard all other languages. Python isn't statically typed? C++ doesn't have
automatic memory management? Bah.

A few years back, I went about designing a mathematics library for games. I
implemented the typical structures: vectors, matrices, quaternions... (I still
don't fully understand how the last one works). However, there were two very
annoying problems: how to parametrize the structures by their element types, and
how to parametrize the structures by the number of elements. The first I could
solve with some sneaky intermediate language hacks. The second proved much more
difficult.

For the first time, my head hit the ceiling; I discovered that C# had limits to
what it could express. This spurred my development as a programmer. Since then
I've learned nearly a dozen new languages, including Python, C++, Prolog, and
Haskell.

There are flaws in every language. The .NET and JVM languages suffer from
inexpressive type systems. Haskell's core libraries are showing signs of age
(tends to happen in an area of heavy research). Python suffers from a global
interpreter lock (is this being fixed?), significant whitespace is not
optional (Haskell you genius), and there's nothing in the language quite as
powerful as call/cc. LISP's braces still bug me, and Prolog is... still slow.

As you can see, the subjectivity of my criticisms increases as I go on. No
language seems to offer exactly what I want. How exactly am I supposed to
satisfy my tastes?

Easy. Make my own language. Of course, the "easy" part is identifying the
solution. _Implementing_ the solution is much harder. I'm sure you're tired of
me rambling on, and perhaps even have objections to my rather half-assed
criticism of your favourite language above. How about we let bygones be bygones
and move on to the meat of this post?

Priorities
==========

What makes a programming language _good_? To answer that, we first need to
identify what a programming language is _for_. We all know what it is:
creating software. At least, that's what _I_ plan on using it for. Those of you
that are into theorem proving should probably look elsewhere.

So what makes a piece of software good? To me, good software is,

* Reliable
* Maintainable
* Fast

The order here is not arbitrary. Software is useless if it's not reliable. If
a piece of software is not maintainable, it becomes difficult to ensure it's
correctness. Only once the first two requirements are satisfied should we worry
about performance. After all, it's usually much easier to optimize software than
it is to debug it.

The obvious exception to this order is when performance is so bad that the
program is no longer usable. "Fast" becomes higher priority than "Maintainable".

Given these requirements, it follows that a programming language should be
designed to aid the programmer in achieving these requirements.

A strong, expressive type system is a boon in all three aspects. Unlike unit
tests, type systems don't reduce certain types of errors: they _eliminate_ them.
They also serve as documentation, and allow the compiler to make various
optimizations.

Generally, the less code implies less bugs. A language should have terse syntax,
although not so terse that it becomes unreadable. This means:

* No redundant brackets, braces, or parentheses
* No Unicode operators

Functional languages (that are not LISP or APL) tend to fit these descriptions
the best.

A language should allow the programmer to drop down to a lower level in order to
write more efficient code. However, such blocks should be clearly marked. The
designers of C# were in the right when they added `unsafe` code to C#.

A somewhat under-appreciated aspect of language design is the core libraries.
Haskell and C++ suffer greatly from this. C++'s standard template library is
embarrassingly small. Haskell's are a little better, but they suffer from
annoying inconsistencies (`fmap` vs `map`), and poor design due to historical
accident (the `Functor`/`Applicative`/`Monad` issue). A language that does not
have polished, standardized libraries will always lose to one that does. JVM
and .NET languages are superior in that regard.

A language without proper libraries will cause software to suffer in all
respects. Consider a language that does not come with a sorting function.
Developers will be forced, after they get over their disbelief, to write their
own, likely suboptimal, replacement. They might write it over and over again,
since they have no standard place to put such basic utilities.

Much of this should be old news, although some hardcore performance enthusiasts
never seem to get the point (we're not all writing real-time applications!). I
don't consider high level languages as a crutch for incompetent programmers to
lean on. I consider them _support_. They are foundations on which competent
programmers can attain even loftier goals.

What I've done here is set some ground rules. I expect every design decision I
make to relate to what I've written here, in some way or another. The next
few posts will discuss type systems, syntax, and libraries.
