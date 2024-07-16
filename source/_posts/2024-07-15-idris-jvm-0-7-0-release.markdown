---
layout: post
title: Idris JVM 0.7.0 Release
date: 2024-07-15 20:51:10-04:00
comments: true
tags: [Idris, JVM, Java]
---

Happy to announce [Idris JVM 0.7.0 release](https://github.com/mmhelloworld/idris-jvm/releases/tag/latest).
In addition to enabling Idris 0.7.0 features for the JVM backend, this release also includes:

* Exporting Idris functions to Java static methods, instance methods and constructors.
* Exporting Java classes from Idris that can extend other classes and implement Java interfaces.
* Exporting functions and classes with Java annotations including function parameter annotations.
* Exporting type class instances and functions that make use of them to be able to be called from Java.

Documentation with examples that demonstrate calling Idris functions from Java: [link](https://idris-jvm.readthedocs.io/en/latest/ffi/calling-idris-from-java.html).

With all of these things, we can now write a complete Spring Boot application in Idris! [example](https://github.com/mmhelloworld/idris-spring-boot-example/blob/main/src/main/idris/Mmhelloworld/IdrisSpringBootExample/Main.idr).
