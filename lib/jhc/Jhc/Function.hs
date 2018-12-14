module Jhc.Function where

{-# SUPERINLINE id, const, (.), ($), ($!), flip #-}

infixr 9  .
infixr 0  $, $!, `seq`

id x = x
const x _ = x
f . g = \x -> f (g x)
f $ x = f x
f $! x = x `seq` f x
flip f x y = f y x

-- asTypeOf is a type-restricted version of const.  It is usually used
-- as an infix operator, and its typing forces its first argument
-- (which is usually overloaded) to have the same type as the second.

{-# SUPERINLINE asTypeOf #-}
asTypeOf         :: a -> a -> a
asTypeOf         =  const

{-# INLINE seq #-}
foreign import primitive seq :: a -> b -> b

infixl 0 `on`

-- | @'fix' f@ is the least fixed point of the function @f@,
-- i.e. the least defined @x@ such that @f x = x@.
fix :: (a -> a) -> a
fix f = let x = f x in x

-- | @(*) \`on\` f = \\x y -> f x * f y@.
--
-- Typical usage: @'Data.List.sortBy' ('compare' \`on\` 'fst')@.
--
-- Algebraic properties:
--
-- * @(*) \`on\` 'id' = (*)@ (if @(*) &#x2209; {&#x22a5;, 'const' &#x22a5;}@)
--
-- * @((*) \`on\` f) \`on\` g = (*) \`on\` (f . g)@
--
-- * @'flip' on f . 'flip' on g = 'flip' on (g . f)@

-- Proofs (so that I don't have to edit the test-suite):

--   (*) `on` id
-- =
--   \x y -> id x * id y
-- =
--   \x y -> x * y
-- = { If (*) /= _|_ or const _|_. }
--   (*)

--   (*) `on` f `on` g
-- =
--   ((*) `on` f) `on` g
-- =
--   \x y -> ((*) `on` f) (g x) (g y)
-- =
--   \x y -> (\x y -> f x * f y) (g x) (g y)
-- =
--   \x y -> f (g x) * f (g y)
-- =
--   \x y -> (f . g) x * (f . g) y
-- =
--   (*) `on` (f . g)
-- =
--   (*) `on` f . g

--   flip on f . flip on g
-- =
--   (\h (*) -> (*) `on` h) f . (\h (*) -> (*) `on` h) g
-- =
--   (\(*) -> (*) `on` f) . (\(*) -> (*) `on` g)
-- =
--   \(*) -> (*) `on` g `on` f
-- = { See above. }
--   \(*) -> (*) `on` g . f
-- =
--   (\h (*) -> (*) `on` h) (g . f)
-- =
--   flip on (g . f)

on :: (b -> b -> c) -> (a -> b) -> a -> a -
