---
layout: post
title: "Haskell on the JVM via GHCJS and Nashorn"
date: 2016-02-27 19:56
comments: true
categories: 
  - Haskell
  - JVM
  - Nashorn
  - GHCJS
---
Currently there are 2 ways we can write Haskell on the JVM:

1. [Frege](https://github.com/Frege/frege), a language that follows Haskell 2010 standard and compiles to Java.
1. Haskell itself by compiling it to JavaScript via GHCJS.

Frege is basically a Haskell for the JVM and is infact conforms to Haskell 2010 with few inbuilt GHC extensions. Even with good Java interop, it doesn't sacrifice its type guarantees and currently is the only pure language on the JVM.

In this post, I am going to explore another interesting option: Haskell itself on the JVM. Haskell can be compiled to JavaScript using GHCJS and Java has an inbuilt JavaScript engine, called Nashorn so it is actually possible to compile Haskell to JavaScript and run the resulting JavaScript on the JVM.

Here is a simple Haskell code that can be run on the JVM:
{% codeblock lang:haskell %} 
module Main where

-- Nashorn doesn't provide default console object. Haskell's putStrLn logs to the console.
foreign import javascript unsafe "console={ \
    \ log: function(s) { java.lang.System.out.print(s); },\
    \ info: function(s) { java.lang.System.out.print(s); },\
    \ warn: function(s) { java.lang.System.out.print(s); },\
    \ debug: function(s) { java.lang.System.out.print(s); },\
    \ error: function(s) { java.lang.System.err.print(s); }\
    \ }"
  setupConsole :: IO ()

foreign import javascript unsafe "java.lang.System.exit($1)"
  sysexit :: Int -> IO ()

main = do
  setupConsole
  putStrLn "Hello from Haskell!"
  sysexit 0

{% endcodeblock %}

Nashorn doesn't have an inbuilt `console` object and Haskell's `putStrLn` prints to the console so we have to provide an implementation of console. The implementation, as can be seen from the code above, is actually backed by Java's `System.out.print`. That is our first example of calling Java from Haskell. `sysexit` is another function calling Java. `sysexit` is needed here as otherwise the program just keeps on running which I think is because of JavaScript event loop or something similar that prevents the JVM from shutting down.

### Compiling Haskell with GHCJS and running on JVM
```
$ ghcjs -o HelloJava HelloJava.hs
[1 of 1] Compiling Main             ( HelloJava.hs, HelloJava.js_o )
Linking HelloJava.jsexe (Main)

$ jjs HelloJava.jsexe/all.js 
Hello from Haskell!
```
`jjs` is a JVM laucher for JavaScript code similar to Node. It is also possible to run this as a regular Java program along with other Java classes without `jjs`. `jjs` is just a convenient way  to run just JavaScript code on the JVM. Above GHCJS compiles the Haskell code to JavaScript in one file `all.js` and the JVM runs the JavaScript code from `all.js`.

### Example 2

Now let's look at another example that shows how to convert between Haskell and Java lists:

{% include_code Converstion between Haskell and Java Lists lang:haskell HaskellJavaListsConversion.hs %}

In the code above, two Java types are used: `java.util.ArrayList` and `java.util.Iterator`. 

#### Importing a Java class
A Java class can be imported with `Java.type(className)` Nashorn JavaScript code. Line 80 defines the corresponding Haskell FFI function:

```haskell
-- Imports a Java class
foreign import javascript unsafe "Java.type($1)"
  jimport :: JSVal -> JType
```
#### Creating an instance of a Java class
An instance can be created by invoking the constructor on the Java class with `new`. Here is the corresponding FFI:
```haskell
-- ArrayList Constructor
foreign import javascript unsafe "new $1()"
  arrayList_new :: JType -> ST s (STArrayList s)
```
It takes the `ArrayList` class and invokes the dafault `ArrayList` constructor to return an instance of it. In the same way, we can create FFI functions for `ArrayList.add` and `ArrayList.iterator` to return an Java `Iterator` instance.

The function `listToArrayList` takes a Haskell list and return an instance of Java `ArrayList`. As the java list is mutable, it is returned as `STArrayList s` inside `ST`. This function first creates an instance of `ArrayList` by invoking the Java constructor and then calls `ArrayList.add` to add items from Haskell list to the `ArrayList`. 

In the similar way, the function `iteratorToList` takes a Java `iterator` and returns Haskell list by extracting items from the iterator by invoking corresponding FFI functions for `Iterator.hasNext` and `Iterator.next`.

### Building with `Stack`
It is easy to [setup a GHCJS project with `Stack`](http://docs.haskellstack.org/en/stable/ghcjs/) so that we can add other dependencies easily and build it for GHCJS. With the above code in a stack project "haskell-jvm-hello", we can build it with `stack build` and run it with `jjs`:

```
$ stack build
haskell-jvm-hello-0.1.0.0: unregistering (local file changes: app/Main.hs)
haskell-jvm-hello-0.1.0.0: build
Preprocessing library haskell-jvm-hello-0.1.0.0...
In-place registering haskell-jvm-hello-0.1.0.0...
Preprocessing executable 'haskell-jvm-hello-exe' for
haskell-jvm-hello-0.1.0.0...
[1 of 1] Compiling Main             ( app/Main.hs, .stack-work/dist/x86_64-linux/Cabal-1.22.4.0_ghcjs/build/haskell-jvm-hello-exe/haskell-jvm-hello-exe-tmp/Main.js_o )
Linking .stack-work/dist/x86_64-linux/Cabal-1.22.4.0_ghcjs/build/haskell-jvm-hello-exe/haskell-jvm-hello-exe.jsexe (Main)
haskell-jvm-hello-0.1.0.0: copy/register
Installing library in
/workspace/haskell-jvm-hello/.stack-work/install/x86_64-linux/lts-3.12/ghcjs-0.2.0_ghc-7.10.3/lib/x86_64-linux-ghcjs-0.2.0-ghc7_10_3/haskell-jvm-hello-0.1.0.0-7MA0h74rERuEwiJY2TRuHx
Installing executable(s) in
/workspace/haskell-jvm-hello/.stack-work/install/x86_64-linux/lts-3.12/ghcjs-0.2.0_ghc-7.10.3/bin
Warning: the following files would be used as linker inputs, but linking is not being done: .stack-work/dist/x86_64-linux/Cabal-1.22.4.0_ghcjs/build/haskell-jvm-hello-exe/haskell-jvm-hello-exe
Registering haskell-jvm-hello-0.1.0.0...

$ jjs .stack-work/dist/x86_64-linux/Cabal-1.22.4.0_ghcjs/build/haskell-jvm-hello-exe/haskell-jvm-hello-exe.jsexe/all.js
1
2
3
4
5
```

Java's Nashorn JavaScript engine opens up few more ways for the JVM to be polyglot and it is so good to have one of the best languages, Haskell, on the JVM. Actually it should also be possible to run PureScript as well in this way on the JVM but that is for another day. Happy Haskelling!
