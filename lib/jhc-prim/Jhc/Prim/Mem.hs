module Jhc.Prim.Mem where

import Jhc.Prim.Bits

-- gc algorithm,
-- increment generation number.
-- perform garbage collection
-- iterate over weak pointers, if the target still exists, set its gcgeneration
-- to the current one.
--
-- dereference weak pointer by checking if the generation number is current
--
-- when generation overflows, compress existings gcgenerations.
--

-- A number representing the current garbage collection generation.
newtype GcGeneration_ = GcGeneration_ BitsPtr_

data StableName a = StableName BitsPtr_ GcGeneration_

foreign import primitive fromWeak :: GcGeneration_  -> BitsPtr_ -> Maybe a
foreign import primitive toWeak :: a -> (# GcGeneration_ , BitsPtr_ #)
