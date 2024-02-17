{-# OPTIONS_GHC -Wno-partial-fields #-}

module Convert (convert) where

import Data.Matrix.MatrixMarket
import Data.Int
import QTree (QTree, leaf, node, mergeQTree)

convert :: Matrix a -> QTree (Maybe Int)
convert (IntMatrix (rowCount, colCount) _ _ values) =
    let mtx = MkIntMatrix { rowCount, colCount, entry = values }
        tree = createTree mtx
    in mergeQTree $ createQTree tree values
convert _ = error "convert unsupported yet"

type IntEntry = [(Int32, Int32, Int)]
data IntMatrix = MkIntMatrix { rowCount :: Int, colCount :: Int, entry :: [(Int32, Int32, Int)] }

data CTree =
    Leaf { row :: Int, col :: Int }
    | Node { lt :: CTree, rt :: CTree, ll :: CTree, rl :: CTree }

createTree :: IntMatrix -> CTree
createTree (MkIntMatrix { rowCount = rc, colCount = cc, entry  = _ }) =
    let run offsetRow rowCount offsetCol colCount
            | rowCount <= 1 && colCount <= 1 = Leaf offsetRow offsetCol
            | rowCount > 1  || colCount > 1  =
                let rowHalf = rowCount `div` 2
                    colHalf = colCount `div` 2

                    lt = run offsetRow rowHalf offsetCol colHalf
                    rt = run offsetRow rowHalf (offsetCol + colHalf) (colCount - colHalf)
                    ll = run (offsetRow + rowHalf) (rowCount - rowHalf) offsetCol colHalf
                    rl = run (offsetRow + rowHalf) (rowCount - rowHalf) (offsetCol + colHalf) (colCount - colHalf)
                in Node {lt, rt, ll, rl}
            | otherwise = error $ "dim error in tree gen row = " ++ show rowCount ++ ", column = " ++ show colCount
    in run 0 rc 0 cc

createQTree :: CTree -> IntEntry -> QTree (Maybe Int)
createQTree (Leaf {row, col}) values = leaf $ find (row, col) values
    where find (col', row') ((col'', row'', value) : tl)
            | col' == fromIntegral col'' && row' == fromIntegral row'' = Just value
            | otherwise = find (col', row') tl
          find _ [] = Nothing
createQTree (Node {lt, rt, ll, rl}) values =
    let lt' = createQTree lt values
        rt' = createQTree rt values
        ll' = createQTree ll values
        rl' = createQTree rl values
    in node lt' rt' ll' rl'
