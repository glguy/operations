{-# Language ExistentialQuantification #-}
{-# Language MagicHash #-}
{-# Language MultiParamTypeClasses, FlexibleInstances #-}
{-# Language RankNTypes #-}
{-# Language ConstraintKinds, DataKinds #-}
{-# Language TypeOperators #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications, AllowAmbiguousTypes #-}

module Object
  ( Object(Object)
  , type (⊆)
  , type (∈)
  , type All
  , type Use

  , upcast

  , using1, usingA
  , map1, mapA

  ) where

import TypeListOps
import GHC.Exts

data Object cs = forall a. All cs a => Object a

------------------------------------------------------------------------

upcast :: forall cs ds. cs ⊆ ds => Object ds -> Object cs
upcast (Object (x :: a)) = pickN @cs @ds @a (Object x)

using1 :: forall c cs r. c ∈ cs => Object cs -> (forall a. c a => a -> r) -> r
using1 (Object (x :: a)) f = pick1 @c @cs @a (f x)

usingA :: forall cs r. Object cs -> (forall a. All cs a => a -> r) -> r
usingA (Object x) f = f x

map1 :: forall c cs. c ∈ cs => (forall a. c a => a -> a) -> Object cs -> Object cs
map1 f (Object (x :: a)) = pick1 @c @cs @a (Object (f x))

mapA :: (forall a. All cs a => a -> a) -> Object cs -> Object cs
mapA f (Object x) = Object (f x)

------------------------------------------------------------------------

class c ∈ cs where
  pick1P :: All cs a => Proxy# a -> (c a => b) -> b

instance {-# OVERLAPS #-} c ∈ (c ': cs) where
  pick1P _ x = x

instance c ∈ cs => c ∈ (d ': cs) where
  pick1P = pick1P @c @cs

pick1 :: forall c cs a r. (c ∈ cs, All cs a) => (c a => r) -> r
pick1 = pick1P @c @cs (proxy# :: Proxy# a)

------------------------------------------------------------------------

pickN :: forall cs ds a r. (cs ⊆ ds, All ds a) => (All cs a => r) -> r
pickN = pickNP @cs @ds (proxy# :: Proxy# a)

class cs ⊆ ds where
  pickNP :: All ds a => Proxy# a -> (All cs a => r) -> r

instance '[] ⊆ ds where
  pickNP _ x = x

instance (c ∈ ds, cs ⊆ ds) => (c ': cs) ⊆ ds where
  pickNP p x = pick1P @c  @ds p
             $ pickNP  @cs @ds p
             $ x
