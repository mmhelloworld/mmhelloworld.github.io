---
layout: post
title: "Idris JVM: Guarding against Java null using Maybe type"
date: 2017-01-10 21:08
comments: true
categories: [Idris, JVM, Java]
---
Idris JVM now helps avoiding nulls getting into Idris from FFI calls using `Maybe` type.
FFI declarations can have `Maybe` type in any argument position or in the return type.

## Handling `null` from FFI call


``` haskell
getProperty : String -> JVM_IO (Maybe String)
getProperty = invokeStatic SystemClass "getProperty" (String -> JVM_IO (Maybe String))
```

The above function is an FFI call to Java's method
[`static String getProperty(String key)`](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#getProperty-java.lang.String-).
The method returns a system property value if the property is set otherwise returns null.
With `Maybe` type in the Idris function's return type, the Idris function returns `Nothing` if the returned value is null
otherwise the value is wrapped in `Just`.

###### Example

``` haskell returningnull.idr
module Main

import IdrisJvm.IO
import Java.Lang

main : JVM_IO ()
main = printLn !(getProperty "foo")  
```

``` bash
$ idris --portable-codegen jvm -p idrisjvmffi returningnull.idr -o target

$ java -cp target:/path/to/idris-jvm-runtime-1.0-SNAPSHOT.jar main.Main
Nothing

$ java -cp target:/path/to/idris-jvm-runtime-1.0-SNAPSHOT.jar -Dfoo=hello main.Main
Just "hello"

```

## Passing `Maybe` values for nullable values in FFI calls

``` haskell passingnull.idr
module Main

import IdrisJvm.IO
import Java.Lang

namespace Component
  Component : Type
  Component = JVM_Native $ Class "java/awt/Component"

namespace JOptionPane
  JOptionPaneClass : JVM_NativeTy
  JOptionPaneClass = Class "javax/swing/JOptionPane"

  showMessageDialog : Inherits Object messageTy => Maybe Component -> messageTy -> JVM_IO ()
  showMessageDialog parent message =
    invokeStatic JOptionPaneClass "showMessageDialog" (Maybe Component -> Object -> JVM_IO ()) parent (believe_me message)

main : JVM_IO ()
main = showMessageDialog Nothing "Hello Idris!"

```

In the above code, the Java method
[`JOptionPane.showMessageDialog(parentComponent, message)`](https://docs.oracle.com/javase/8/docs/api/javax/swing/JOptionPane.html#showMessageDialog-java.awt.Component-java.lang.Object-)
takes a nullable parent component and a message. If the parent component is `null` then the message is displayed in a default frame.

```
$ idris --portable-codegen jvm -p idrisjvmffi passingnull.idr -o target

$ java -cp target:/path/to/idris-jvm-runtime-1.0-SNAPSHOT.jar main.Main
```
Idris code passes `Nothing` in the above code so `null` is passed for the Java method that displays the message in a default frame as shown below.
{% img center /images/idris-passing-null.png %}
