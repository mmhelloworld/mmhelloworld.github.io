---
layout: post
title: "Frege: Hello Java"
date: 2013-07-10 11:00
comments: true
tags: [Frege, Java]
---

Here is a small code demonstrating Java interoperability in Frege:

```haskell
module hellojava.HelloJava where
 
data LinkedList a = native java.util.LinkedList where
    native add :: Mutable s (LinkedList a) -> a -> ST s Bool
    native get :: Mutable s (LinkedList a) -> Int -> ST s (Maybe a) throws
        IndexOutOfBoundsException
    native new :: () -> STMutable s (LinkedList a)
    
    fromFregeList :: [a] -> STMutable s (LinkedList a)
    fromFregeList xs = LinkedList.new () >>= loop xs where
        loop (x:xs) jlist = LinkedList.add jlist x >> loop xs jlist
        loop [] jlist = return jlist
        
plusTop :: Mutable s (LinkedList Int) -> ST s (Maybe Int)
plusTop xs = do
    a <- xs.get 0
    b <- xs.get 1
    return ((+) <$> a <*> b)
 
data IndexOutOfBoundsException = native java.lang.IndexOutOfBoundsException
derive Exceptional IndexOutOfBoundsException

data Exception = native java.lang.Exception
derive Exceptional Exception
 
data NullPointerException = native java.lang.NullPointerException
derive Exceptional NullPointerException
 
pure native showThrowable toString :: Throwable -> String
 
main _ = do
    javaList <- LinkedList.fromFregeList [1, 2, 3]
    try (\xs -> plusTop xs >>= (println . maybe "Got a null pointer" show)) javaList 
        `catch` (\(npe :: NullPointerException) -> println $ showThrowable npe)
        `catch` (\(exception :: Exception) -> println $ showThrowable exception)
```

We can observe the following things from the above code:

1. Making use of a Java class and its methods
2. Using a Java object in a Frege function
3. Using Java Exceptions in functions
4. Handling Java exceptions

### 1. Making use of a Java class and its methods:

If a Java class is pure then without much effort, we can use that class in Frege. For example,

```haskell
data Integer = native java.math.BigInteger where
    pure  native abs                                  :: Integer -> Integer
    pure  native negate                               :: Integer -> Integer
    pure  native valueOf java.math.BigInteger.valueOf :: Long -> Integer
```

A Java class is declared with `data` declaration in Frege. The identifier after the `data` keyword
is the corresponding type for the Java class in Frege and the qualified Java class is identified after the `native`
keyword followed by the instance methods, static methods or even some Frege functions not defined in the
original Java class.

An important point here is that the instance methods on BigInteger take Integer as their
first argument which is the `this` reference on which the methods will be invoked.

Coming back to our original example, here 
we are trying to use the mutable Java class `java.util.LinkedList`.
An obvious difference between this one and the `BigInteger` example is that the 
functions now do not have the `pure` keyword in front.

The next difference is that the instance methods now cannot take the simple type like `LinkedList a` as we did for
`Integer` but the type is now `Mutable s (LinkedList a)` since it is not a pure function.
If we don't annotate a native function `pure` and we don't use `Mutable` to consume or return a mutable Object, it will be a 
compilation error. Mutable objects can only be used in `ST` or `IO` actions so the return type 
must be in ST or IO monad.

The [LinkedList.add()](http://docs.oracle.com/javase/7/docs/api/java/util/LinkedList.html#add(E\)) method returns a boolean. Since
it is an impure function, it should be used in `ST` monad. Here the boolean itself is pure so it is just `ST s Bool`.
Take a look at the third function `new`, `LinkedList` constructor. This function is impure and it returns
a mutable object, a new `LinkedList` instance, so the return type is `ST s (Mutable s (LinkedList a))` for which the shorthand is `STMutable s (LinkedList a)`.

Here is an example for a native function not being part of a native
`data` declaration. This is useful when a native class is already
declared in Frege in some module but the function that we are looking
for is missing in the `data` declaration.

```haskell
pure native showThrowable toString :: Throwable -> String
```

Here `showThrowable` is the Frege function name for `Throwable.toString()`. Since it is an
instance method on `Throwable`, the first argument is of type
`Throwable` and then the formal arguments' types (in this case, none) and return type.

### 2. Using a Java object in a Frege function

A native `data` declaration doesn't have to just contain the native members, it can also have
additional Frege functions. 
In our example, the function `fromFregeList` is not defined in
the Java class but it has been added as an utility function to create a `LinkedList` from a frege list.
Here again the same rule as in the previous section applies: To return a mutable Java object, 
we should use `ST s (Mutable s TheJavaType)` which is nothing but `STMutable s TheJavaType`.

In the same way, the `plusTop` function takes a mutable Java object so the parameter type is
`Mutable s (LinkedList Int)`. Also since it consumes a mutable type, it must be in `ST` monad hence 
the return type is `ST s (Maybe Int)` returning an `Maybe Int` in `ST`.

### 3. Using Java Exceptions in functions

To use a Java Exception class, it must be first defined in a Frege
module. It is the same as declaring native declaration for a Java class but 
additionally we need to derive the `Exceptional` type class so that the exception can later be handled with
`catch`.

```haskell
data IndexOutOfBoundsException = native java.lang.IndexOutOfBoundsException

derive Exceptional IndexOutOfBoundsException
```

The exceptions can then be used in native declarations as in `get` function in our example:

```haskell
native get :: Mutable s (LinkedList a) -> Int -> ST s (Maybe a) throws
    IndexOutOfBoundsException
```

### 4. Handling Java exceptions

In two ways, we can handle exceptions:

1.  Using ``action `catch` handler1 `catch` handler2``

    The type of `catch` is `Exceptional β => ST γ α -> (β->ST γ α) -> ST γ α`.

    Here the `action` is the code where an exception might be thrown
    and the handlers `handler1` and `handler2` take an exception and
    return another value in `ST` monad. The infix notation facilitates
    adding multiple handlers with better readability. Further here the
    `handler1` must be more specific(in terms of the types of the
    exceptions being handled) than `handler2`. Also note that from [Frege standard library](https://github.com/Frege/frege/blob/master/frege/prelude/PreludeIO.fr#L116) with respect to `catch`:

    > **Note** If *action* is of the form:
       `doSomething arg`
    > then, depending on the strictness of `doSomething` the argument `arg` may be evaluated
    **before** the action is returned. Exceptions (i.e. undefined values) 
    that occur in the construction of the action do **not** count as 
    exceptions thrown during execution of it, and hence cannot be catched.
    
    > Example:
    ``println (head []) `catch`  ....``
    
    > will not catch the exception that will be thrown when println evaluates
    
    > For a remedy, see `try`.

2.  Using `try`

    First, the type: `try :: Monad γ => (α-> γ β) -> α -> γ β`


    Unlike `catch`, `try` takes a function that produces a monadic value. If
    the function can throw an exception, it must result in an `ST`
    monad which can then be passed to `catch` to handle those
    exceptions. In our example, `\xs -> plusTop xs >>= (println . maybe "Got a null pointer"
    show)` is the function which when applied to a
    `java.util.LinkedList` might throw a `NullPointerException` or
    `IndexOutOfBoundsException`:
    
    ```haskell
    try (\xs -> plusTop xs >>= (println . maybe "Got a null pointer" show)) javaList
        catch` (\(npe :: NullPointerException) -> println $ showThrowable npe)
        catch` (\(exception :: Exception) -> println $ showThrowable exception)
    ```

Since the construction of action is deferred through a lambda
here, `try` eliminates the issue with `catch` mentioned in the above note.

###Extending a class or implementing an interface in Frege:

One thing that is not shown in the example is extending a Java class
or implementing an interface in Frege. Unfortunately both are not possible in Frege
yet. There is a workaround though using a Java class which extends a
class or implements an interface but instead of an implementation on
its own, it just delegates to a Frege function. For example, see
[here](https://github.com/Frege/frege/blob/master/frege/java/Lang.fr#L89)
for implementing `java.lang.Runnable` in Frege using a Java class
[frege.runtime.SwingSupport](https://github.com/Frege/frege/blob/master/frege/runtime/SwingSupport.java#L56) which takes a Frege function and then
delegates to it in `run` method implementation.


This concludes our little experimentation calling Java from Frege. The
other interesting side, calling Frege from Java, is for a
future post.
