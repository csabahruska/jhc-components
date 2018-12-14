{-# OPTIONS_JHC -fno-prelude -fffi #-}
module Jhc.Storable(Storable_(..)) where

import Jhc.Basics
import Jhc.Addr
import Jhc.IO

class Storable_ a where
    sizeOf_ :: a -> Word__
    alignment_ :: a -> Word__
    peekElemOff_ :: Addr__ -> Word__ -> IO a
    pokeElemOff_ :: Addr__ -> Word__ -> a -> UIO_
    peek_ :: Addr__ -> IO a
    poke_ :: Addr__ -> a -> UIO_

    alignment x = sizeOf x
    peekElemOff addr idx = res where
        res = IO $ \w -> unIO (peek_ (addr `plus` (idx `times` sizeOf_ (_f res)))) w
    pokeElemOff addr idx x = \w -> poke_ (addr `plus` (idx `times` sizeOf_ x)) x w

_f :: IO a -> a
_f _ = undefined

foreign import "Add" plus :: BitsPtr_ -> Word__ -> BitsPtr_
foreign import "UMul" plus :: Word__ -> Word__ -> Word__
