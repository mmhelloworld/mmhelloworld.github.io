---
layout: post
title: "Idris 2 Initial Release 0.2.1 for the JVM"
date: 2021-07-24 10:32:32 -0400
comments: true
tags: [Idris, JVM]
---

There is now an initial version for Idris 2 - 0.2.1 for the JVM. This is compiled from Idris 2 JVM bootstrap version.

## Changes:
* Compile initial Idris 2 compiler (0.2.1) with Idris 2 JVM bootstrap version targeting Java 8 bytecode
* Add JVM backend
* Support primitives such as system, IO, network, clock, buffer etc. for JVM backend
* Eliminate tail recursion
* Add debug information such as variable name, file name and line number in bytecode from Idris source
* Compile Idris modules to Java classes at the bytecode level, Idris top level functions to static Java methods and Idris lambdas into Java lambdas with `invokedynamic` implementing Java `Function` interface.
* Generate bytecode concurrently for modules
* Generate classes with `toString` and property getters for Idris constructors
* Infer types to avoid boxing and casting
* Compile Idris list into a Java list
* JVM foreign descriptors supporting static, instance, interface methods and constructor calls

## To try it out:
* Unzip `idris2-jvm-0.2.1-SNAPSHOT-20210720.zip` from here https://github.com/mmhelloworld/Idris2/releases/tag/v0.2.1-SNAPSHOT-20210720
* Add `bin` directory to `PATH` variable
* Copy `.idris2` directory containing Idris 0.2.1 packages into your home directory.

## To compile and run:
```haskell
module Main

data Tree a = Leaf
            | Node (Tree a) a (Tree a)

inorder : Tree a -> List a
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

tree : Tree String
tree = Node
        (Node
          (Node Leaf "3" Leaf)
          "+"
          (Node Leaf "7" Leaf))
        "/"
        (Node Leaf "2" Leaf)

main : IO ()
main = printLn $ inorder tree
```

```bash
$ idris2 Main.idr -o main

$ java -jar build/exec/main_app/main.jar
["3", "+", "7", "/", "2"]
```
