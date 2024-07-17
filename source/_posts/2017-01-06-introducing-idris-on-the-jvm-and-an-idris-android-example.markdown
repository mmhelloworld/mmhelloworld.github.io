---
layout: post
title: "Introducing Idris on the JVM and an Idris Android example"
date: 2017-01-06 22:51
comments: true
tags: [Idris, JVM, Java, Android]
---
[Idris on the JVM!](https://github.com/mmhelloworld/idris-jvm) Yes, a dependently typed language on the JVM!
I have been working on a [JVM bytecode backend for Idris](https://github.com/mmhelloworld/idris-jvm) for the past few months and
it is now at a point that we can even write Android programs with Idris without having to write a single line of Java.
In this post, we will see how Idris works on the JVM and an example Android program written in Idris.

## Hello World

```haskell
module Main

main : IO ()
main = printLn "Hello World"
```

###### Compile:

```bash
$ idris --portable-codegen jvm -p idrisjvmffi helloworld.idr -o target
```

Dependencies are provided as Idris packages, not as Java dependencies like jar or class files.
The overall process is that the compiler reads Idris files and converts them into an intermediate JSON representation and
the JVM bytecode generator takes the JSON files and converts them into JVM bytecode class files directly.
It is only when we run a Java class, we have to provide Java dependency jars. The output option `-o` represents a directory
where the Java class files will be created.

###### Run:

```bash
$ java -cp target:/path/to/idris-jvm-runtime-1.0-SNAPSHOT.jar main.Main
"Hello World"

```

And the output folder contains,

```bash
$ tree target
target
├── Decidable
│   └── Equality.class
├── main
│   └── Main.class
└── Prelude
    ├── Basics.class
    ├── Bool.class
    ├── Chars.class
    ├── Interfaces.class
    ├── Show.class
    └── Strings.class

```

Why do we have all these classes? We only compiled `Main` module! This is because Idris performs whole program analysis/compilation
and code generator generates bytecode for all the modules that are relevant for the result produced by the main program.

## How does Idris JVM handle tail calls?

Idris JVM eliminates self-recursion with JVM `GOTO` and uses trampolines for other tail calls.
Let's look at the following examples.

##### self-recursion example

```haskell
module Main

import IdrisJvm.IO

sum : Nat -> Nat
sum n = go 0 n where
  go : Nat -> Nat -> Nat
  go acc Z = acc
  go acc n@(S k) = go (acc + n) k

main : JVM_IO ()
main = printLn (sum 50000)

```

This program would work just fine without blowing up the stack as it will be compiled down to a loop that uses JVM's `GOTO` instruction.
Here is the relevant section from bytecode:

```bash
$ javap -c -cp target:/path/to/idris-jvm-runtime-1.0-SNAPSHOT.jar main.Main

  public static java.lang.Object sum$whr$go$colon$0(java.lang.Object, java.lang.Object, java.lang.Object);
    Code:
       0: aconst_null
       1: astore        7
       3: aconst_null
       4: astore_3
       5: aconst_null
       6: astore        4
       8: aconst_null
       9: astore        5
      11: aconst_null
      12: astore        6
      14: iconst_1
      15: istore        8
      17: iload         8
      19: ifeq          130
      22: aload_2
      23: new           #80                 // class java/math/BigInteger
      26: dup
      27: ldc           #82                 // String 0
      29: invokespecial #85                 // Method java/math/BigInteger."<init>":(Ljava/lang/String;)V
      32: invokestatic  #333                // Method mmhelloworld/idrisjvmruntime/Util.equals:(Ljava/lang/Object;Ljava/lang/Object;)Z
      35: ifeq          47
      38: aload_1
      39: astore        7
      41: iconst_0
      42: istore        8
      44: goto          127
      47: new           #80                 // class java/math/BigInteger
      50: dup
      51: ldc_w         #335                // String 1
      54: invokespecial #85                 // Method java/math/BigInteger."<init>":(Ljava/lang/String;)V
      57: astore_3
      58: aload_2
      59: invokestatic  #103                // Method mmhelloworld/idrisjvmruntime/Util.asBigInt:(Ljava/lang/Object;)Ljava/math/BigInteger;
      62: aload_3
      63: invokestatic  #103                // Method mmhelloworld/idrisjvmruntime/Util.asBigInt:(Ljava/lang/Object;)Ljava/math/BigInteger;
      66: invokevirtual #338                // Method java/math/BigInteger.subtract:(Ljava/math/BigInteger;)Ljava/math/BigInteger;
      69: astore_3
      70: new           #80                 // class java/math/BigInteger
      73: dup
      74: ldc_w         #335                // String 1
      77: invokespecial #85                 // Method java/math/BigInteger."<init>":(Ljava/lang/String;)V
      80: astore        4
      82: aload_3
      83: invokestatic  #103                // Method mmhelloworld/idrisjvmruntime/Util.asBigInt:(Ljava/lang/Object;)Ljava/math/BigInteger;
      86: aload         4
      88: invokestatic  #103                // Method mmhelloworld/idrisjvmruntime/Util.asBigInt:(Ljava/lang/Object;)Ljava/math/BigInteger;
      91: invokevirtual #107                // Method java/math/BigInteger.add:(Ljava/math/BigInteger;)Ljava/math/BigInteger;
      94: astore        4
      96: iconst_0
      97: invokestatic  #41                 // Method java/lang/Integer.valueOf:(I)Ljava/lang/Integer;
     100: astore        5
     102: aload_1
     103: invokestatic  #103                // Method mmhelloworld/idrisjvmruntime/Util.asBigInt:(Ljava/lang/Object;)Ljava/math/BigInteger;
     106: aload         4
     108: invokestatic  #103                // Method mmhelloworld/idrisjvmruntime/Util.asBigInt:(Ljava/lang/Object;)Ljava/math/BigInteger;
     111: invokevirtual #107                // Method java/math/BigInteger.add:(Ljava/math/BigInteger;)Ljava/math/BigInteger;
     114: astore        6
     116: aload         5
     118: astore_0
     119: aload         6
     121: astore_1
     122: aload_3
     123: astore_2
     124: goto          127
     127: goto          17
     130: aload         7
     132: areturn

```
The third line from the last is the `GOTO` instruction that transfers the control back to the top of function instead of
actually calling the function.

##### Mutual recursion example:

```haskell
module Main

mutual
  evenT : Nat -> IO Bool
  evenT Z = pure True
  evenT (S k) = oddT k

  oddT : Nat -> IO Bool
  oddT Z = pure False
  oddT (S k) = evenT k

main : IO ()
main = printLn !(evenT 9999)

```

The above code also would work fine without killing the stack. Mutual recursion is handled using trampolines and
the tail calls are delayed and compiled down to Java 8 lambdas. As the bytecode for this is bit long, here is the
decompiled bytecode for the `evenT` function:

```java
    public static Object evenT(Object var0) {
        Object var4 = null;
        Integer var1 = null;
        Integer var2 = null;
        IdrisObject var3 = null;
        if (Util.equals(var0, BigInteger.ZERO)) {
            var1 = Integer.valueOf(0);
            var2 = Integer.valueOf(0);
            var3 = new IdrisObject(1);
            var4 = new IdrisObject(65653, new Object[]{var1, var2, var3});
        } else {
            BigInteger var5 = BigInteger.ONE;
            var5 = Util.asBigInt(var0).subtract(Util.asBigInt(var5));
            var4 = () -> {
                return oddT(var5);
            };
        }

        return var4;
    }
```

As we can see from the decompiled output above, the `oddT` call is not performed on the same call stack but a thunk
wrapping the function call is returned using lambda (which is compiled down to JVM's `invokedynamic` instruction).

Here is the relevant bit from bytecode for those who are interested:

```
        68: getstatic     #64                 // Field java/math/BigInteger.ONE:Ljava/math/BigInteger;
        71: astore_1
        72: aload_0
        73: invokestatic  #68                 // Method io/github/mmhelloworld/idrisjvm/runtime/Util.asBigInt:(Ljava/lang/Object;)Ljava/math/BigInteger;
        76: aload_1
        77: invokestatic  #68                 // Method io/github/mmhelloworld/idrisjvm/runtime/Util.asBigInt:(Ljava/lang/Object;)Ljava/math/BigInteger;
        80: invokevirtual #72                 // Method java/math/BigInteger.subtract:(Ljava/math/BigInteger;)Ljava/math/BigInteger;
        83: astore_1
        84: aload_1
        85: invokedynamic #79,  0             // InvokeDynamic #1:call:(Ljava/lang/Object;)Lio/github/mmhelloworld/idrisjvm/runtime/Thunk;
        90: astore        4
        92: goto          95
        95: aload         4
        97: areturn
```

## Java interoperability: Calling Java from Idris and calling Idris from Java

Idris JVM supports calling Java static methods, instance methods, constructors and also accessing static and instance fields from Idris.
At the moment, except for Java arrays, all the Java types can be constructed from Idris and passed to Java methods.
Support for handling nulls and exceptions is currently in progress and will soon be available.
(Update 01/10/2017: We can now
[use `Maybe` type to avoid Java nulls](http://mmhelloworld.github.io/blog/2017/01/10/idris-jvm-guarding-against-java-null-using-maybe-type/)
in Idris code)

To use Idris functions from Java, Idris JVM supports exporting Idris functions as static methods, instance methods,
constructors of an exported Java class. The exported class can also extend a Java class or implement interfaces with Idris functions.

To demonstrate these features, let's create an Android application In Idris.

### An Android application in Idris

```haskell
module Main

import IdrisJvm.IO
import Java.Lang

pythag : Int -> List (Int, Int, Int)
pythag max = [(x, y, z) | z <- [1..max], y <- [1..z], x <- [1..y],
                          x * x + y * y == z * z]

IdrisAndroidActivity : Type
IdrisAndroidActivity = JVM_Native $ Class "hello/IdrisAndroidActivity"

Bundle : Type
Bundle = JVM_Native $ Class "android/os/Bundle"

Context : Type
Context = JVM_Native $ Class "android/content/Context"

View : Type
View = JVM_Native $ Class "android/view/View"

TextView : Type
TextView = JVM_Native $ Class "android/widget/TextView"

Inherits View TextView where {}

CharSequence : Type
CharSequence = JVM_Native $ Class "java/lang/CharSequence"

Inherits CharSequence String where {}

superOnCreate : IdrisAndroidActivity -> Bundle -> JVM_IO ()
superOnCreate = invokeInstance "superOnCreate" (IdrisAndroidActivity -> Bundle -> JVM_IO ())

getApplicationContext : IdrisAndroidActivity -> JVM_IO Context
getApplicationContext = invokeInstance "getApplicationContext" (IdrisAndroidActivity -> JVM_IO Context)

newTextView : Context -> JVM_IO TextView
newTextView = FFI.new (Context -> JVM_IO TextView)

setText : Inherits CharSequence charSequence => TextView -> charSequence -> JVM_IO ()
setText this text = invokeInstance "setText" (TextView -> CharSequence -> JVM_IO ()) this (believe_me text)

setContentView : Inherits View view => IdrisAndroidActivity -> view -> JVM_IO ()
setContentView this view = invokeInstance "setContentView" (IdrisAndroidActivity -> View -> JVM_IO ()) this (believe_me view)

onCreate : IdrisAndroidActivity -> Bundle -> JVM_IO ()
onCreate this bundle = do
  superOnCreate this bundle
  context <- getApplicationContext this
  textView <- newTextView context
  setText textView $ "Hello Android from Idris! pythag 50 is " ++ show (pythag 50)
  setContentView this textView

main : IO ()
main = pure ()

androidExport: FFI_Export FFI_JVM "hello/IdrisAndroidActivity extends android/support/v7/app/AppCompatActivity" []
androidExport =
  Fun superOnCreate (Super "onCreate") $
  Fun onCreate (ExportInstance "onCreate") $
  End

```

The above program demonstrates calling Java instance methods (`setText` for example) and constructors (`newTextView`).

It further demonstrates how to handle inheritance relationship when passing subclass instances to a parent class type.
For example, function `setContentView` takes a `View` but we can pass a `TextView` as it is a subclass of `View` and we
mention that to Idris via `Inherits View TextView where {}`.

It also demonstrates how we can create a Java class that extends another class and override methods with Idris functions.
The last section `androidExport` creates a Java class named `hello/IdrisAndroidActivity` that extends `android/support/v7/app/AppCompatActivity`.
The Java class also creates a wrapper method `superOnCreate` that just delegates to `super.OnCreate` and the class also overrides `onCreate` method
with Idris' `onCreate` function. The Java class can also implement one or more Java interfaces with something like,

```bash
hello/IdrisAndroidActivity extends android/support/v7/app/AppCompatActivity implements java/lang/Runnable, java/lang/Comparable
```

A module can have multiple exports so we can actually create multiple Java classes with Idris implementation functions from an Idris module.

We can compile this as usual,

`idris --portable-codegen jvm -p idrisjvmffi idrisandroid.idr -o target`

It would produce the following class files:

```
$ tree target/
target/
├── Decidable
│   └── Equality.class
├── hello
│   └── IdrisAndroidActivity.class
├── main
│   ├── Main.class
│   └── Prelude.class
└── Prelude
    ├── Algebra.class
    ├── Applicative.class
    ├── Bool.class
    ├── Foldable.class
    ├── Interfaces.class
    ├── List.class
    ├── Monad.class
    ├── Show.class
    └── Strings.class

```

##### Deploying to Android:
1. Create an android project using Android studio with [Jack support for Java 8](https://developer.android.com/guide/platform/j8-jack.html#configuration).
1. Then package the classes compiled above along with idris-jvm-runtime-1.0-SNAPSHOT.jar classes in a single jar and copy into an
android project's `app/libs` directory.
1. Change the activity class name in android manifest file to the Idris exported class name `hello.IdrisAndroidActivity`.
1. Then run `./gradlew installDebug` from android project after starting an emulator or connected to an android device.
1. Finally we should see our Idris code running on Android! It should look something like this:
![Idris Android](/images/idris-android.png)

Happy coding!
