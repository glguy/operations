{-# Language ExistentialQuantification #-}
{-# Language MultiParamTypeClasses, FlexibleInstances #-}
{-# Language RankNTypes #-}
{-# Language ConstraintKinds, DataKinds #-}
{-# Language TypeOperators #-}
{-# Language InstanceSigs #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances, UndecidableSuperClasses #-}
{-# Language TypeApplications, AllowAmbiguousTypes #-}

module Object
  ( Object(Object)
  , type (⊆)
  , type (∈)
  , type All
  , type Use

  , upcast

  , using1, usingN -- usingA
  , map1, mapN -- mapA

  ) where

import TypeListOps
import Data.Kind
import Data.Constraint ( (:-), top, (&&&), weaken1, weaken2, (\\))
import Control.Category (id, (.))
import Prelude hiding (id, (.))

data Object cs = forall a. All cs a => Object a

------------------------------------------------------------------------

usingN :: forall cs ds r. cs ⊆ ds => Object ds -> (forall a. All cs a => a -> r) -> r
usingN (Object (x :: a)) f = f x \\ pickN @cs @ds @a



upcast :: forall cs ds. cs ⊆ ds => Object ds -> Object cs
upcast o = usingN @cs o Object

using1 :: forall c cs r. c ∈ cs => Object cs -> (forall a. c a => a -> r) -> r
using1 = usingN @'[c]

-- usingA :: forall cs r. Object cs -> (forall a. All cs a => a -> r) -> r
-- usingA = usingN @cs

------------------------------------------------------------------------

mapN :: forall cs ds. cs ⊆ ds => Object ds -> (forall a. All cs a => a -> a) -> Object ds
mapN (Object (x :: a)) f = Object (f x) \\ pickN @cs @ds @a


map1 :: forall c cs. c ∈ cs => Object cs -> (forall a. c a => a -> a) -> Object cs
map1 = mapN @'[c]

-- mapA :: forall cs. Object cs -> (forall a. All cs a => a -> a) -> Object cs
-- mapA = mapN @cs

------------------------------------------------------------------------

class                     c ∈ cs        where pick1 :: All cs a :- c a
instance {-# OVERLAPS #-} c ∈ (c ': cs) where pick1 = weaken1
instance c ∈ cs =>        c ∈ (d ': cs) where pick1 = pick1 @c @cs . weaken2

------------------------------------------------------------------------

type family (⊆´) cs ds :: Constraint where
  (c ': cs) ⊆´ ds = (c ∈ ds, cs ⊆ ds, cs ⊆´ ds)
  '[]       ⊆´ ds = ()

class cs ⊆´ ds => cs ⊆ ds where
  pickN :: forall a. All ds a :- All cs a

instance '[] ⊆ ds where
  pickN = top

instance ((c ': cs) ⊆´ ds) => (c ': cs) ⊆ ds where
  pickN :: forall a. All ds a :- All (c ': cs) a
  pickN = pick1 @c  @ds @a
      &&& pickN @cs @ds @a
