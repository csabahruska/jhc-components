module Jhc.Prim.SelfPtr where

newtype SelfPtr a = SelfPtr Void

withSelfPtr :: SelfPtr a -> (Ptr a -> IO b) -> IO b
withSelfPtr sp action = sp `seq` do
    r <- action $ Ptr (nodeToAddr_ (toNode_ sp))
    touch_ sp
    return r
newSelfPtr :: Int -> IO (SelfPtr a)

data Void
mallocBytesNode_ :: Int -> IO (Node_ Void)

-- A Node_ is always in WHNF.
-- toNode_ will seq the object if needed.
data Node_ a :: #
toNode_ :: a -> Node_ a
fromNode_ :: Node_ a -> a

nodeToAddr_ :: Node_ a -> Addr_
