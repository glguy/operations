{-# Language TypeApplications    #-}
{-# Language QuantifiedConstraints #-}
{-# Language UndecidableInstances #-}
{-# Language ScopedTypeVariables #-}
{-# Language DataKinds           #-}
{-# Language TypeOperators       #-}
{-# Language FlexibleContexts    #-}
module Example where

import Object

exampleUpcast :: Object (Read & Show) -> Object Show
exampleUpcast = upcast

-- | List of objects that can be tracked with any set of constraints, as
-- long as those constraints are available for Int and Double.
producer1 :: (cs Float, cs Double) => [Object cs]
producer1 = [Object (10 :: Float), Object (4.2 :: Double), Object (5.3 :: Double)]

producer2 :: (cs Integer, cs Int) => [Object cs]
producer2 = [Object (100 :: Integer), Object (1 :: Int)]

add10 :: Num ⊆ cs => Object cs -> Object cs
add10 (Object o) = Object (o + 10)

example :: [String]
example = [ show (o+10) | Object o <- map add10 (producer1 @(Show & Num) ++ producer2)]

example1 :: [String]
example1 = [ show (recip o) | Object o :: Object (Show & Fractional) <- producer1 ]
