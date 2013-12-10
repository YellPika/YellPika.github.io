---
layout: post
title: "The Perfect Language: Paradigms"
categories: [language design, programming]
published: true
---

This post is part of what will be a series of posts where I discuss language
design. Each post can be considered to be a work in progress, and will be
updated from time to time.

A paradigm is a way of thinking. Object-oriented programmers tend to think in
terms of communicating entities, functional programmers tend to think in terms
of sequences of transformations, and logic programmers tend to think in terms of
definitions and descriptions. I say "tend to", because it's perfectly possible
to emulate other styles of programming in each paradigm. I recently embedded
[typed logical programming][tlogic] in Haskell. An abstract interface can be
emulated with a record of functions. The [command pattern][command] is
essentially a bloated closure. This is my primary gripe with (mainstream) object
oriented languages. Objects are a swiss army chainsaw, a monolithic concept. You
can implement anything with them, but the result will often be overly verbose.

How about the reverse? Let's try to emulate objects in a functional language.
Consider the following, somewhat contrived, C# code:

{% highlight csharp linenos %}
public interface IConnection {
    int Read(byte[] bytes, int offset, int count);
    void Write(byte[] bytes, int offset, int count);
}

public class TcpConnection : IConnection {
    public TcpConnection(string address) { ... }
    public virtual int Read(byte[] bytes, int offset, int count) { ... }
    public virtual void Write(byte[] bytes, int offset, int count) { ... }
}

public class SslConnection : TcpConnection {
    public SslConnection(string address)
        : base(address) {
        ...
    }

    public int Read(byte[] bytes, int offset, int count) { ... base ... }
    public void Write(byte[] bytes, int offset, int count) { ... base ... }
}
{% endhighlight %}

This example is a rather stupid one. `SslConnection` would be better implemented
with composition, but I wanted to throw in inheritance for the sake of the
argument. Now for Haskell:

{% highlight haskell linenos %}
data Connection = Connection {
    read :: String -> Int -> Int -> IO Int
    write :: String -> Int -> Int -> IO ()
}

newTcpConnection :: String -> IO Connection
newTcpConnection address = do
    ...
    return $ Connection {
        read = ...
        write = ...
    }

newSslConnection :: String -> IO Connection
newSslConnection address = do
    base <- newTcpConnection address
    return $ Connection {
        read = ... base ...
        write = ... base ...
    }
{% endhighlight %}

This example has a small problem: once we've created a connection, we have no
way to identify what kind of connection it is. I think this indicates an
advantage. While it's entirely possible to implement the missing parts, this
particular implementation is simple, concise, and doesn't implement unnecessary
features. What if you don't _want_ to be able to identify the type of the
connection? No problem.

Objects (a la C++ and Java) are not a general concept; they are a unification of
multiple smaller concepts. Is this actually a bad thing? There are languages
like Scala, which throw in everything and the kitchen sink. Then there are
languages like Scheme, which consist of an extremely limited set of fundamental
primitives, and build everything on top of them. Which approach leads to better
software?

There's a reason Scheme is used for teaching: it's simple as hell. Larger
structures can be built on top of smaller ones. However, this approach has its
own kinds of problems. How should programmers recognize these larger structures?
Surely something like Scala would be better, as every language feature is
documented, and has clear uses?

Actually, that's a terrible argument. In the end, it boils down to learning
idioms. The singleton is a design pattern - an idiom unique to object oriented
programming. Baking it into the language doesn't make the concept easier or
harder to learn. So what are the differences between idioms that are baked into
the language, and idioms that are created within a language? I see no meaningful
difference, so both approaches are valid... in the hands of experts. There _is_
a difference, it that difference becomes apparent when teaching languages to
newcomers. I highly doubt that any school will decide to teach Scala to their
first year students.

If two programs are written with equal skill, then the more maintainable one is
the program written in the language with more users. Therefore, a language
should be simple enough for beginners to pick up, but powerful enough for an
expert to exercise his full abilities. These languages are invariably
functional. Or Python.

[tlogic]: https://github.com/YellPika/tlogic
[command]: http://en.wikipedia.org/wiki/Command_pattern
[aa]: http://en.wikipedia.org/wiki/Associative_array
