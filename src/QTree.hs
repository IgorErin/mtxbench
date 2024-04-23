{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module QTree (QTree(..), leaf, node, toBS, toBuilder, toText, mergeQTree, explicitZeros) where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Data.Maybe (fromMaybe)

import Fmt ((+|), (|+), (||+), (+||), Builder, fmt)

import GHC.Generics (Generic, Generic1)
import Control.DeepSeq

data QTree a =
    QLeaf { value :: a }
    | QNode { lt :: QTree a, rt :: QTree a, ll :: QTree a, rl :: QTree a }
    deriving (Show, Generic, Generic1, NFData, NFData1)

instance Functor QTree where
    fmap :: (a -> b) -> QTree a -> QTree b
    fmap f (QLeaf {value}) = QLeaf { value = f value }
    fmap f (QNode {lt, rt, ll, rl}) =
        let lt' = fmap f lt
            rt' = fmap f rt
            ll' = fmap f ll
            rl' = fmap f rl
        in QNode { lt = lt', rt = rt', ll = ll', rl = rl' }

leaf :: a -> QTree a
leaf value = QLeaf { value }

node :: QTree a -> QTree a -> QTree a -> QTree a -> QTree a
node lt rt ll rl = QNode  { lt, rt, ll, rl }

toBuilder :: Show a => QTree a -> Builder
toBuilder (QLeaf {value}) = "Leaf ("+||value||+")"
toBuilder (QNode {lt, rt, ll, rl}) =
    "(Node ("+|toBuilder lt|+") ("+|toBuilder rt|+") ("+|toBuilder ll|+") ("+|toBuilder rl|+"))"

toBS :: Show a => QTree a -> ByteString
toBS = fmt . toBuilder

toText :: Show a => QTree a -> Text
toText = fmt . toBuilder

data Info a = Try a | No

mergeQTree :: QTree (Maybe ()) -> QTree (Maybe ())
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

explicitZeros :: a -> QTree (Maybe a) -> QTree a
explicitZeros zero = fmap (fromMaybe zero)
