module Jhc.Prim.List where

infixr 5  :
data [] a =  a : ([] a) | []
