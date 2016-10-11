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

  , using1, usingN, usingA
  , map1, mapN, mapA

  ) where

import TypeListOps
import GHC.Exts
import Data.Constraint
import Control.Category
import Prelude hiding (id, (.))

data Object cs = forall a. All cs a => Object a

------------------------------------------------------------------------

usingN :: forall cs ds r. cs ⊆ ds => Object ds -> (forall a. All cs a => a -> r) -> r
usingN (Object (x :: a)) f = f x \\ pickN @cs @ds @a



upcast :: forall cs ds. cs ⊆ ds => Object ds -> Object cs
upcast o = usingN @cs o Object

using1 :: forall c cs r. c ∈ cs => Object cs -> (forall a. c a => a -> r) -> r
using1 = usingN @'[c]

usingA :: forall cs r. Object cs -> (forall a. All cs a => a -> r) -> r
usingA = usingN @cs

------------------------------------------------------------------------

mapN :: forall cs ds. cs ⊆ ds => Object ds -> (forall a. All cs a => a -> a) -> Object ds
mapN (Object (x :: a)) f = Object (f x) \\ pickN @cs @ds @a



map1 :: forall c cs. c ∈ cs => Object cs -> (forall a. c a => a -> a) -> Object cs
map1 = mapN @'[c]

mapA :: forall cs. Object cs -> (forall a. All cs a => a -> a) -> Object cs
mapA = mapN @cs

------------------------------------------------------------------------

class                     c ∈ cs        where pick1 :: All cs a :- c a
instance {-# OVERLAPS #-} c ∈ (c ': cs) where pick1 = weaken1
instance c ∈ cs =>        c ∈ (d ': cs) where pick1 = pick1 @c @cs . weaken2

pick1P :: forall c cs a. c ∈ cs => Proxy# a -> All cs a :- c a
pick1P _ = pick1 @c @cs @a

------------------------------------------------------------------------

pickN :: forall cs ds a r. cs ⊆ ds => All ds a :- All cs a
pickN = pickNP @cs @ds (proxy# :: Proxy# a)

class cs ⊆ ds where
  pickNP :: Proxy# a -> All ds a :- All cs a

instance '[] ⊆ ds where
  pickNP _ = top

instance (c ∈ ds, cs ⊆ ds) => (c ': cs) ⊆ ds where
  pickNP p = pick1P @c  @ds p
         &&& pickNP @cs @ds p

-- handy shortcut
instance {-# INCOHERENT #-} ds ⊆ ds where
  pickNP _ = id
