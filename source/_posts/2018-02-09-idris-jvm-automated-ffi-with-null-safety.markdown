---
layout: post
title: "Idris JVM: Automated FFI with null safety and exception handling"
date: 2018-02-11 00:03
comments: true
tags: [Idris, JVM, Java]
---
## Background
Idris JVM backend has supported foreign function calls for some time now. For example, to invoke `parseInt` method on 
`java.lang.Integer` class,

```haskell
invokeStatic (Class "java/lang/Integer") "parseInt" (String -> JVM_IO Int) "234" 
```

Here since the Idris compiler doesn't know anything about Java's `Integer` class or its `parseInt` method, we have to 
explicitly provide the function signature. The function call also has the explicit `Class` 
before `java/lang/Integer` and the type of invocation `invokeStatic`. 

Since we are targeting JVM bytecode, JVM has to know whether a method call is a static method call or an interface 
method call or a virtual method call. It would be nice if we don't have to worry about any of these things and 
just call a FFI function with a class name, method name and the arguments. This is the motivation behind this 
new feature along with some other nice things like null safety, construtor and method overloading resolution 
and exception handling.

#### Maybe and Either in foreign function calls
`Maybe` type and `Either` type can be used in foreign function calls for null safety and exception handling. `Maybe` 
type can be used for argument types and return types. `Maybe` type used in an argument position will pass `null`
to the target foreign function if it is `Nothing` or the actual value if it is `Just`. Similarly, `Maybe` type
used for return type will convert `null` returned from foreign function into `Nothing` and the non-null value into 
`Just.` At the bytecode level, `Maybe` wrapper doesn't exist. It gets compiled down to
`null` or the actual value.

`Either` type can only be used in return types to indicate whether the foreign function can throw exceptions.
At runtime, if the foreign function throws exception, it will be captured in the "left" of type `Throwable` or if the
foreign function completes normally, the result will be stored in the "right" of result type. There are functions `try`
and `catch` to handle exceptions which we will see later in the post.

## How it works
Before we look at some examples, first let's declare some class names as we are going to use them in multiple places and
we don't want to duplicate.


```haskell
stringClass: String
stringClass = "java/lang/String"

listInterface: String
listInterface = "java/util/List"

arrayListClass: String
arrayListClass = "java/util/ArrayList"

collectionInterface : String
collectionInterface = "java/util/Collection"

systemClass: String
systemClass = "java/lang/System"

comparatorClass : String
comparatorClass = "java/util/Comparator"

pointClass : String
pointClass = "java/awt/Point"

collectionsClass : String
collectionsClass = "java/util/Collections"

stringBuilderClass : String
stringBuilderClass = "java/lang/StringBuilder"

objectsClass : String
objectsClass = "java/util/Objects"

integerClass : String
integerClass = "java/lang/Integer"
```

And "import" some methods:

```haskell
jdkimport [
    (systemClass, ["getProperty", "setProperty"]),
    (stringClass, ["substring", "CASE_INSENSITIVE_ORDER", "valueOf"]),
    (integerClass, ["parseInt"]),
    (comparatorClass, ["compare"]),
    (arrayListClass, ["<init>", "add"]),
    (listInterface, ["get"]),
    (collectionsClass, ["max"]),
    (stringBuilderClass, ["<init>", "toString"]),
    (objectsClass, ["toString"]),
    (pointClass, ["<init>", "x"])
  ]
```
Here `jdkimport` is just an additional syntax created using Idris syntax extensions. It just calls a type provider
function written in Idris to know about these classes, methods and fields. Note that it imports fields such as
`CASE_INSENSITIVE_ORDER`, `x` and also constructors in the name of `<init>` which is the JVM internal name for 
constructors. The `jdkimport` syntax launches a JVM during compilation without any classpath so it basically can import 
all the JDK classes and methods. 

There is also another syntax called `jvmimport` that can take an additional argument, a command, which could be just the
JVM with correct classpath or could be a build tool that properly sets up the classpath from your project dependencies so
that we can "import" classes and methods from external foreign libraries. 

Once the information about JVM classes and methods is collected using type provider, appropriate call site, Idris code
similar to the one in the beginning of the post can be created using Idris elaborator reflection with just class name 
and member name from the user. As a user, we don't have to know much about these internals, we just need to import classes
and members and can use them without having to explicitly provide foreign types. Now let's look at some examples on how we can 
actually make FFI calls in the new way.

## Examples
#### 1. Safe static method call

```haskell
main : JVM_IO ()
main = do
  exceptionOrInt <- (integerClass <.> "parseInt") "1234"
  printLn $ either (const 0) id exceptionOrInt 
```
Here the type of `(integerClass <.> "parseInt")` is `String -> JVM_IO (Either Throwable Int)`. Since the method can throw 
exceptions, it returns an `Either`. Here we return `0` in case of an exception. Later in the post, we will see a 
detailed example of exception handling. As the method returns an `Int` which is a primitive type in JVM, it cannot be 
null and the FFI call already knows that hence the result `Int` is not wrapped in a `Maybe`. We don't provide any
explicit type signature for the foreign function. If we try to pass anything other than `String` for this foreign
function, it will be a compilation error!

#### 2. Unsafe static method call

```haskell
  do
    number <- (integerClass <.!> "parseInt") "23"
    printLn number
```
Here we use `<.!>` with an `!` to indicate an unsafe method call instead of `<.>`. There is also `javaUnsafe` and `java`
if you prefer names to operators. The type of `(integerClass <.!> "parseInt")` is `String -> JVM_IO Int`. 
Sometimes if we are sure that the foreign function would not return null or throw exceptions, we can use unsafe method 
calls but as the name indicates, it would fail at runtime if null is returned or an exception is thrown.

#### 3. Overloading resolution
We can pick which overloaded variant we want to use by passing appropriate types to the foreign function and 
the FFI call will automatically have corresponding types.

```haskell
printLn !((stringClass <.!> "valueOf(double)") 2.5)
printLn !((stringClass <.!> "valueOf(char)") 'H')
```
The first function takes an Idris `Double` and the second function takes Idris `Char`. The types passed to the foreign
functions to resolve overloading are JVM types.

#### 4. Safe instance method
```haskell
  do
    s <- (stringClass <.> "substring(int)") "Foobar" 1
    putStrLn !(either throw (pure . show) s) 

```
Safe instance method calls are similar to static method calls except that the instance should be passed as the first
argument. Here again, we don't provide any explicit type signature or the type of method invocation whether it is static or 
instance method but it all works out automatically in a type safe way. Here also we pick a particular overloaded version.

The type of `(stringClass <.> "substring(int)")` is `String -> Int -> JVM_IO (Either Throwable (Maybe String))`.
Since the return type is `String` and it can be null, it is in a `Maybe` and the method can throw exceptions so the 
overall type is in `Either`. 

#### 5. Exception handling

```haskell
do
  propValue <- try ((systemClass <.> "getProperty(?java/lang/String)") Nothing) [
    ([catch IllegalArgumentExceptionClass, catch NullPointerExceptionClass], \t =>
      do
        printLn "property name is null or empty"
        pure Nothing
    ),
    ([catchNonFatal], \t =>
      do
        printLn "unable to get property value"
        pure Nothing
    )
  ]
  printLn propValue
```
This example shows how to handle exceptions with different handlers and also shows how to pass a `null` to a foreign function.
If a FFI function argument type is prefixed with `?`, then the idris type would be `Maybe nativeTy` and we can pass
`Nothing` to pass a `null` to the foreign function. We can have handlers for single exception, multiple exceptions or 
for all non fatal errors similar to Scala's 
[NonFatal](https://github.com/scala/scala/blob/2.12.x/src/library/scala/util/control/NonFatal.scala).

#### 6. Constructors

```haskell
do
  arrayList1 <- (arrayListClass <.> "<init>(int)") 10
  putStrLn !(either throw toString arrayList1)

  -- Unsafe constructor
  arrayList2 <- arrayListClass <.!> "<init>()"
  putStrLn !(toString arrayList2)
```
Similar to methods, constructors can be overloaded and we can select a particular overload variant by explicitly 
specifying the foreign type. Constructors can also be invoked in a safe or unsafe way. As constructors cannot return
null, when invoked in a safe way, the result type will only be in `Either` and not wrapped in a `Maybe`.

#### 7. Fields
```haskell
do
  -- static field getter
  caseInsensitiveComparator <- stringClass <.#!> "CASE_INSENSITIVE_ORDER"
  printLn !((comparatorClass <.!> "compare") caseInsensitiveComparator "Bar" "august")

  point <- (pointClass <.!> "<init>(int,int)") 2 3

  -- instance field getter
  printLn !((pointClass <.#> "x") point)

  -- instance field setter
  (pointClass <.=> "x") point 34
  printLn !((pointClass <.#> "x") point)
```
Similar to methods and constructors, fields can also be accessed either in a safe or unsafe way using `<.#>` for safe
getter, `<.=>` for safe setter, `<.#!>` for unsafe getter and `<.=!>` for unsafe setter. Since field access
cannot throw a exception, the return type is automatically just `Maybe nativeTy`. The field types are automatically
determined without the user having to provide the foreign types of the fields.

## Summary
This post demonstrated how with Idris' powerful features FFI, type provider and elaborator reflection, we can safely and
easily access JVM foreign functions. We can access fields, methods and constructors without having to explicitly provide
foreign types and we can access them in safe way without `null` getting into Idris code and handle exceptions thrown by
foreign functions. It also showed how to call overloaded methods and constructors and how `Maybe` and `Either` types are
used with foreign functions.

