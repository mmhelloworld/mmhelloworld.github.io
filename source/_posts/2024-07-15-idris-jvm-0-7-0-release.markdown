---
layout: post
title: Idris JVM 0.7.0 Release
date: 2024-07-15 20:51:10-04:00
comments: true
tags: [Idris, JVM, Java]
---

Happy to announce [Idris JVM 0.7.0 release](https://github.com/mmhelloworld/idris-jvm/releases/tag/latest)!
In addition to enabling Idris 0.7.0 features for the JVM backend, this release also includes:

* Exporting Idris functions to Java static methods, instance methods and constructors.
* Exporting Java classes from Idris that can extend other classes and implement Java interfaces.
* Exporting functions and classes with Java annotations including function parameter annotations.
* Exporting type class instances and functions that make use of them to be able to be called from Java.

Documentation with examples that explain calling Idris functions from Java is available here: [link](https://idris-jvm.readthedocs.io/en/latest/ffi/calling-idris-from-java.html).

Here is a complete example in Idris and Java for demonstration:

```haskell

  module Main

  import Data.String
  import Data.List

  -- Define some import aliases to avoid repeating fully qualified name
  %export
      """
      jvm:import
      idris/String IdrisString
      idris/data/List IdrisList
      idris/data/Maybe IdrisMaybe
      idris/prelude/Show
      helloworld/Color
      """
  jvmImports : List String
  jvmImports = []

  -- Export Idris types to Java
  %export
      """
      jvm:export
      IdrisList
      IdrisMaybe
      Show
      Color
      """
  typeExports : List String
  typeExports = []

  -- Exports Idris List's nil constructor as a static method into generated Java class aliased by `IdrisList`
  %export """
      jvm:public static nil
      {
          "enclosingType": "IdrisList",
          "returnType": "IdrisList"
      }
      """
  idrisNil : List a
  idrisNil = []
  
  -- Exports Idris List's cons constructor as a static method into generated Java class aliased by `IdrisList`
  %export """
      jvm:public static cons
      {
          "enclosingType": "IdrisList",
          "arguments": [{"type": "Object"}, {"type": "IdrisList"}],
          "returnType": "IdrisList"
      }
      """
  idrisCons : a -> List a -> List a
  idrisCons = (::)

 -- Exports the Show typeclass instance for Idris Int type to Java
  %export """
      jvm:public static show
      {
          "enclosingType": "idris/Int",
          "returnType": "Show"
      }
      """
  showInt : Show Int
  showInt = %search

  - Exports the Show typeclass instance for Idris String type to Java
  %export """
      jvm:public static show
      {
          "enclosingType": "IdrisString",
          "returnType": "Show"
      }
      """
  showString : Show String
  showString = %search

  data Color = Red | Green | Blue

  Show Color where
      show Red = "Red"
      show Green = "Green"
      show Blue = "Blue"

  -- Exports Color constructors as static methods to Java under class Color.
  %export """
      jvm:public static red
      {
          "enclosingType": "Color",
          "returnType": "Color"
      }
      """
  red : Color
  red = Red

  %export """
      jvm:public static green
      {
          "enclosingType": "Color",
          "returnType": "Color"
      }
      """
  green : Color
  green = Green

  %export """
      jvm:public static blue
      {
          "enclosingType": "Color",
          "returnType": "Color"
      }
      """
  blue : Color
  blue = Blue

  %export """
      jvm:public static show
      {
          "enclosingType": "Color",
          "returnType": "Show"
      }
      """
  exportShowColor : Show Color
  exportShowColor = %search

  %export """
      jvm:public static show
      {
          "enclosingType": "Color",
          "arguments": [{"type": "Color"}],
          "returnType": "String"
      }
      """
  showColor : Color -> String
  showColor = show

  -- Exports Show typeclass instance to Java for Idris List given its element's Show typeclass instance
  %export """
      jvm:public static show
      {
          "enclosingType": "IdrisList",
          "arguments": [{"type": "Show"}],
          "returnType": "Show"
      }
      """
  exportShowList : Show a => Show (List a)
  exportShowList = %search

  %export """
      jvm:public static just
      {
          "enclosingType": "IdrisMaybe",
          "arguments": [{"type": "Object"}],
          "returnType": "IdrisMaybe"
      }
      """
  exportJust : a -> Maybe a
  exportJust = Just

  %export """
      jvm:public static nothing
      {
          "enclosingType": "IdrisMaybe",
          "returnType": "IdrisMaybe"
      }
      """
  exportNothing : Maybe a
  exportNothing = Nothing

  %export """
      jvm:public static show
      {
          "enclosingType": "IdrisMaybe",
          "arguments": [{"type": "Show"}],
          "returnType": "Show"
      }
      """
  exportShowMaybe : Show a => Show (Maybe a)
  exportShowMaybe = %search

  %export """
      jvm:public static show
      {
          "enclosingType": "Show",
          "arguments": [{"type": "Show"}, {"type": "Object"}],
          "returnType": "String"
      }
      """
  exportShow : Show a => a -> String
  exportShow = show

  %export """
      jvm:public static replicate
      {
          "enclosingType": "IdrisString",
          "arguments": [{"type": "BigInteger"}, {"type": "char"}],
          "returnType": "String"
      }
      """
  exportStringReplicate : Nat -> Char -> String
  exportStringReplicate = String.replicate

  main : IO ()
  main = pure ()
```

Java calling the Idris functions:

```java

    package hello;

    import helloworld.Color;
    import idris.Int;
    import idris.data.List;
    import idris.data.Maybe;
    import idris.prelude.Show;

    import static helloworld.Color.blue;
    import static helloworld.Color.green;
    import static helloworld.Color.red;
    import static idris.data.List.cons;
    import static idris.data.List.nil;
    import static idris.data.Maybe.just;
    import static idris.data.Maybe.nothing;
    import static idris.prelude.Show.show;
    import static java.math.BigInteger.TEN;

    public class Main {

        public static void main(String[] args) {
            List idrisIntList = cons(23, cons(45, nil())); // Create an Idris list of integers
            List idrisStringList = cons("foo", cons("bar", nil()));

            // Create an Idris list of Colors defined as data Color = Red | Green | Blue
            List idrisColorList = cons(red().toIdris(), cons(blue().toIdris(), nil()));

            // Get Show instance for Idris List given a show Instance of Int
            Show intListShow = List.show(Int.show());
            Show stringListShow = List.show(idris.String.show());
            Show colorShow = Color.show();
            Show colorListShow = List.show(colorShow);
            Show colorMaybeShow = Maybe.show(colorShow);

            // Use exported Idris Show instances to print Idris List for differnt element types
            System.out.println(show(intListShow, idrisIntList.toIdris()));
            System.out.println(show(stringListShow, idrisStringList.toIdris()));
            System.out.println(show(colorListShow, idrisColorList.toIdris()));

            System.out.println(show(colorShow, green().toIdris()));
            System.out.println(Color.show(blue()));

            System.out.println(show(colorMaybeShow, just(green().toIdris()).toIdris()));
            System.out.println(show(colorMaybeShow, nothing().toIdris()));
            System.out.println(idris.String.replicate(TEN, 'A'));
        }

    }
```

Here is the output:

```text
    [23, 45]
    ["foo", "bar"]
    [Red, Blue]
    Green
    Blue
    Just Green
    Nothing
    AAAAAAAAAA

```

With all of these things, we can now write a complete Spring Boot application as well in Idris! [Example here](https://github.com/mmhelloworld/idris-spring-boot-example/blob/main/src/main/idris/Mmhelloworld/IdrisSpringBootExample/Main.idr).

[Exporting](https://idris-jvm.readthedocs.io/en/latest/ffi/calling-idris-from-java.html) Idris functions and types along with [foreign function interface](https://idris-jvm.readthedocs.io/en/latest/ffi/calling-java-from-idris.html) thus allows us to integrate Idris much easily into existing Java ecosystem.
