module DataStructures.Queue
       ( Queue
       , newQueue
       , DataStructures.Queue.null
       , enq
       , deq
       , fromList
       ) where

data Queue a = Queue [a] [a]
             deriving (Eq)

instance (Show a) => Show (Queue a) where
  show (Queue xs ys) = show $ xs ++ (reverse ys)

newQueue :: Queue a
newQueue = Queue [] []

null :: Queue a -> Bool
null (Queue [] []) = True
null _             = False

enq :: Queue a -> a -> Queue a
enq (Queue xs ys) y = Queue xs (y:ys)

deq :: Queue a -> (a, Queue a)
deq (Queue (x:xs) ys) = (x, Queue xs ys)
deq (Queue [] ys) = deq (Queue (reverse ys) [])

fromList :: [a] -> Queue a
fromList xs = Queue xs []
