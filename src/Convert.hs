module Convert (run) where

import Helpers ( takeFst, takeSnd, takeThrd, roundUp )
import QTree as Q (QTree, leaf, node, mergeQTree)
import Data.Matrix.MatrixMarket (Matrix(IntMatrix))
import Data.Int

data MtxSparseFormat = Mtx {values :: [(Int32, Int32, Int)], linesCount :: Int, columnCount :: Int} deriving (Show)

run :: Matrix a -> QTree (Maybe Int)
run (IntMatrix (rowCount, colCount) _ _ values') =
    let mtx = Mtx { linesCount = rowCount, columnCount = colCount, values = values' }
    in toQuadTreeFromMtxFormat mtx
run _ = error "convert unsupported yet"

type Entry = [(Int32, Int32, Int)]

mtxFormatPartition :: MtxSparseFormat -> (MtxSparseFormat , MtxSparseFormat, MtxSparseFormat, MtxSparseFormat)
mtxFormatPartition mtx@(Mtx values rows columns)
    | columns == 0 || rows == 0 = (mtx, mtx, mtx, mtx)
    | otherwise =
        let -- cringe formIntegral
            halfRows = fromIntegral $ roundUp $ fromRational (toRational rows / 2)
            halfColumns = fromIntegral $ roundUp $ fromRational (toRational columns / 2)

            inner :: Entry -> Entry -> Entry -> Entry -> Entry -> (Entry, Entry, Entry, Entry)
            inner [] nw' ne' sw' se' = (nw', ne', sw', se')
            inner ((i, j, value) : tl) nw' ne' sw' se'
                | i <= halfRows && j <= halfColumns = inner tl ((i, j, value) : nw') ne' sw' se'
                | i <= halfRows && j > halfColumns = inner tl nw' ((i, j - halfColumns, value) : ne') sw' se'
                | i > halfRows && j <= halfColumns = inner tl nw' ne' ((i - halfRows, j, value) : sw') se'
                | otherwise = inner tl nw' ne' sw' ((i - halfRows, j - halfRows, value) : se')

            (nw, ne, sw, se) = inner values [] [] [] []
            -- cirnge fromIntegral
            halfRows' = fromIntegral halfRows
            halfCols' = fromIntegral halfColumns

            mnw = Mtx nw halfRows' halfCols'
            mne = Mtx ne halfRows' halfCols'
            msw = Mtx sw halfRows' halfCols'
            mse = Mtx se halfRows' halfCols'
        in
        (mnw, mne, msw, mse)

toQuadTreeFromMtxFormat ::  MtxSparseFormat -> QTree (Maybe Int)
toQuadTreeFromMtxFormat (Mtx values rows columns)
    | rows == 0 && columns == 0 = leaf Nothing
    | rows == 1 && columns == 1 && not (null values) = leaf (Just $ takeThrd $ head values)
    | otherwise = inner $ Mtx values powerSize powerSize
    where
        powerSize = 2 ^ roundUp (logBase 2 (toEnum $ max rows columns))
        maxRowIndex = rows - 1
        maxColumnIndex = columns - 1
        inner mtx'@(Mtx values' rows' columns')
            | rows' == 1
                && columns' == 1
                && length values' == 1
                && takeFst (head values') <= fromIntegral maxRowIndex
                && takeSnd (head values') <= fromIntegral maxColumnIndex =
                leaf (Just $ takeThrd $ head values')
            | null values' = leaf Nothing
            | otherwise =  mergeQTree $ node (inner nw) (inner ne) (inner sw) (inner se)
            where
                (nw, ne, sw, se) = mtxFormatPartition mtx'