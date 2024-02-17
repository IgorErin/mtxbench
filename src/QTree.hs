{-# OPTIONS_GHC -Wno-partial-fields #-}

module QTree (QTree(..), leaf, node, toBS, mergeQTree) where

import Data.ByteString.Lazy
import Fmt

data QTree a =
    QLeaf { value :: a }
    | QNode { lt :: QTree a, rt :: QTree a, ll :: QTree a, rl :: QTree a }
    deriving Show

leaf :: a -> QTree a
leaf value = QLeaf { value }

node :: QTree a -> QTree a -> QTree a -> QTree a -> QTree a
node lt rt ll rl = QNode  { lt, rt, ll, rl }

showm :: Show a => QTree a -> Builder
showm (QLeaf {value}) = "Leaf ("+||value||+")"
showm (QNode {lt, rt, ll, rl}) =
    "(Node ("+|showm lt|+"), ("+|showm rt|+"), ("+|showm ll|+"), ("+|showm rl|+"))"

toBS :: Show a => QTree a -> ByteString
toBS = fmt . showm

data Info a = Try a | No

mergeQTree :: QTree (Maybe Int) -> QTree (Maybe Int)
mergeQTree = run
    where
    info :: Eq a => QTree a -> Info a
    info (QLeaf {value}) = Try value
    info (QNode {lt, rt, ll, rl}) =
        check (info lt) (info rt) (info ll) (info rl)

    check :: Eq a => Info a -> Info a -> Info a -> Info a -> Info a
    check (Try f) (Try s) (Try t) (Try r)
        | f == s && s == t && t == r = Try f
    check _ _ _ _ = No

    run :: Eq a => QTree a -> QTree a
    run n@(QNode {lt, rt, ll, rl}) = case info n of
            Try value -> QLeaf value
            No        ->
                let lt' = run lt
                    rt' = run rt
                    ll' = run ll
                    rl' = run rl
                in QNode {lt = lt', rt = rt', ll = ll', rl = rl' }
    run l@(QLeaf _) = l

