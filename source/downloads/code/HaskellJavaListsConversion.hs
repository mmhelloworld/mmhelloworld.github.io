{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.ST
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Prim
import Data.Typeable
import GHC.ST 

data MutabilityType s = Mutable
                      | Immutable
                      | STMutable s

data IsItMutable = IsImmutable
                 | IsMutable

-- Copied from GHCJS.Internal.Types. Not sure why this is not exposed.
type family Mutability (a :: MutabilityType s) :: IsItMutable where
  Mutability Immutable     = IsImmutable
  Mutability Mutable       = IsMutable
  Mutability (STMutable s) = IsMutable

{- java.util.ArrayList class and its methods -}
newtype SomeArrayList (a :: MutabilityType s) = SomeArrayList JSVal deriving Typeable
type ArrayList           = SomeArrayList Immutable
type MutableArrayList    = SomeArrayList Mutable
type STArrayList s      = SomeArrayList (STMutable s)

instance IsJSVal (SomeArrayList m)

-- ArrayList Constructor
foreign import javascript unsafe "new $1()"
  arrayList_new :: JType -> ST s (STArrayList s) 

-- Adds an element to ArrayList
foreign import javascript unsafe "$2.add($1)"
  arrayList_add :: JSVal ->  STArrayList s -> ST s ()

{- java.util.Iterator class and its methods -}
newtype SomeIterator (a :: MutabilityType s) = SomeIterator JSVal deriving Typeable
type Iterator            = SomeIterator Immutable
type MutableIterator     = SomeIterator Mutable
type STIterator s        = SomeIterator (STMutable s)

instance IsJSVal (SomeIterator m)

-- Create an Iterator from an ArrayList
foreign import javascript unsafe "$1.iterator()"
  iterator :: STArrayList s -> ST s (STIterator s)

foreign import javascript unsafe "$1.hasNext()"
  iterator_hasNext :: STIterator s -> ST s Bool

foreign import javascript unsafe "$1.next()"
  iterator_next :: STIterator s -> ST s JSVal

{- Other Nashorn imports -}

-- Represents a Java type
newtype JType = JType JSVal

foreign import javascript unsafe "java.lang.System.out.println($1)"
  jprintln :: JSVal -> IO ()

foreign import javascript unsafe "java.lang.System.exit($1)"
  sysexit :: Int -> IO ()

-- Imports a Java class
foreign import javascript unsafe "Java.type($1)"
  jimport :: JSVal -> JType 

{- Create an instance of Java's ArrayList from Haskell's list -}
listToArrayList :: [JSVal] -> ST s (STArrayList s)
listToArrayList xs = do
    let arrayListClass = jimport $ toJSString "java.util.ArrayList"
    arrList <- arrayList_new arrayListClass
    go xs arrList
  where
    go [] arrList = return arrList
    go (x:xs) arrList = do
      arrayList_add x arrList
      go xs arrList

{- Create Haskell's list from Java's Iterator -}
iteratorToList :: STIterator s -> ST s [JSVal]
iteratorToList itr = reverse <$> go [] where
  go acc = do
    hasNext <- iterator_hasNext itr
    if hasNext
      then do
        next <- iterator_next itr
        go (next: acc)
      else
        return acc

-- Nashorn doesn't provide default console object. Haskell's putStrLn logs to the console.
foreign import javascript unsafe "console={ \
    \ log: function(s) { java.lang.System.out.print(s); },\
    \ info: function(s) { java.lang.System.out.print(s); },\
    \ warn: function(s) { java.lang.System.out.print(s); },\
    \ debug: function(s) { java.lang.System.out.print(s); },\
    \ error: function(s) { java.lang.System.err.print(s); }\
    \ }"
  setupConsole :: IO ()

demo = runST $ do
  jlist <- listToArrayList . map toJSInt $ [1..5]
  iterator jlist >>= iteratorToList
 
main = do
  setupConsole
  mapM_ (putStrLn . show . fromJSInt) demo
  sysexit 0
 
