---
layout: post
title: "Frege: Record accessors and mutators"
date: 2014-03-15 20:41
comments: true
categories: [Frege]
---
Frege has built-in mechanism to access and mutate (non-destructive) record fields.

Consider the following type in Frege:

{% codeblock lang:haskell %}
frege> data Point = Point {x :: Int, y :: Int}
data type Point :: *

frege> derive Show Point
instance Show  Point
{% endcodeblock %}

Now we can use the following functions to get and set record fields:

{% codeblock lang:haskell %}
frege> Point.x
:: Point -> Int

frege> Point.{x = }
:: Point -> Int -> Point

frege> Point.{x <- }
:: Point -> (Int->Int) -> Point
{% endcodeblock %}

For Field `x`,

1. The function `Point.x` is the getter.
2. The function `Point.{x = }` is a setter which sets the field `x` with a new value.
3. The function `Point.{x <- }` is also a setter but applies a function to update the current value.

We can use the functions like this:

{% codeblock lang:haskell %}
frege> p = Point 3 4
value p :: Point

frege> p
Point 3 4

frege> Point.x p
3

frege> Point.{x =} p 13
Point 13 4

frege> Point.{x <-} p (+15)
Point 18 4
{% endcodeblock %}

Frege also provides some shortcuts to apply these functions:

{% codeblock lang:haskell %}
frege> p.x -- Same as `Point.x p`
3

frege> p.{x = 10} -- Same as `Point.{x = } p 10`
Point 10 4

frege> p.{x <-} -- Same as `Point.{x <-} p`
:: (Int->Int) -> Point

frege> p.{x <- (+10)} -- Same as `Point.{x <- } p (+10)`
Point 13 4
{% endcodeblock %}

Multiple updates can be combined:

{% codeblock lang:haskell %}
frege> p.{x <- (+8), y = 20} -- Increment x by 8 and set y to 20
Point 11 20
{% endcodeblock %}

Accessors and updates can be at any level deep.
Let's create another type:

{% codeblock lang:haskell %}
frege> :{
> data Circle = Circle {center :: Point, radius :: Int}
>
> derive Show Circle
> :}

data type Circle :: *
instance Show  Circle

frege>
{% endcodeblock %}

Here we have an aggregate type `Circle` which composes another type `Point` for it's field `center`.
Now we can update and select fields at different levels:

{% codeblock lang:haskell %}
frege> c = Circle {center = Point 4 5, radius = 10}
value c :: Circle

frege> c
Circle (Point 4 5) 10

frege> c.center.x
4

frege> c.{center <- Point.{x = 8}}
Circle (Point 8 5) 10

frege> c.{center <- Point.{y <- (+20)}, radius <- (*5)}
Circle (Point 4 25) 50
{% endcodeblock %}

In the latest version, Frege provides syntactic sugar for lambdas using underscores. For example, `T.foo` can be written
as `_.foo` if the type can be deduced from the context the lambda is applied. Hence the following two are equivalent.

{% codeblock lang:haskell %}
frege> c.{center <- Point.{x = 25}}
Circle (Point 25 5) 10

frege> c.{center <- _.{x = 25}}
Circle (Point 25 5) 10
{% endcodeblock %}

Frege provides another utility to check for a field's existence. This would be useful if we have multiple constructors
with different set of fields.

{% codeblock lang:haskell %}
frege> :{
> data Point = Point2d {x :: Int, y :: Int}
>            | Point3d {x :: Int, y :: Int, z :: Int}
>
> derive Show Point
> :}

data type Point :: *
instance Show  Point

frege>
{% endcodeblock %}

In the above code, we have two constructors `Point2d` and `Point3d` where the field `z` exists only for `Point3d`.
We can check for the existence of field `z` like this:

{% codeblock lang:haskell %}
frege> Point.{z?}
:: Point -> Bool

frege> hasZ = Point.{z?}

frege> hasZ $ Point3d 3 4 5
true

frege> hasZ $ Point2d 3 4
false

frege> p = Point3d 3 4 5
value p :: Point

frege> p.{z?}
true

{% endcodeblock %}

For more details on how these field existence check, accessor and mutator functions are generated for a record type,
here is the link to Frege language reference: http://www.frege-lang.org/doc/Language.pdf.

Happy coding!