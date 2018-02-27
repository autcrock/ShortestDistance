module PairingHeap (
    Heap
    , findMin
    , merge
    , mergePairs
    , insert
    , deleteMin
) where

-- Compromise between simplicity and performance
-- https://stackoverflow.com/questions/27136691/is-there-a-simple-way-to-implement-a-fast-priority-queue-in-haskell
-- https://en.wikipedia.org/wiki/Pairing_heap

    data Heap a = Empty | Heap a [Heap a] deriving Show

    findMin :: Heap a -> a
    findMin (Heap h _) = h

    merge :: Ord a => Heap a -> Heap a -> Heap a
    merge Empty h = h
    merge h Empty = h
    merge h1@(Heap x hs1) h2@(Heap y hs2)
        | x < y     = Heap x (h2:hs1)
        | otherwise = Heap y (h1:hs2)

    mergePairs :: Ord a => [Heap a] -> Heap a
    mergePairs []           = Empty
    mergePairs [h]          = h
    mergePairs (h1:h2:hs)   = merge (merge h1 h2) (mergePairs hs)

    insert :: Ord a => a -> Heap a -> Heap a
    insert x = merge (Heap x [])

    deleteMin :: Ord a => Heap a -> Heap a
    deleteMin (Heap x hs) = mergePairs hs
