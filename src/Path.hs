module Path (mkDstPath, mtxPaths, mains) where

import System.FilePath (takeBaseName, replaceExtension, replaceDirectory, (</>), (<.>))

import Data.List (isPrefixOf)

import Test.Tasty.Golden (findByExtension)

type Name = String

fnFilter :: [FilePath] -> [Name] -> [FilePath]
fnFilter fps nms = filter ((`elem` nms) . takeBaseName) fps

readNames :: FilePath -> IO [Name]
readNames fp = do
    lns <- lines <$> readFile fp

    return $
        filter (not . null) $
        filter (not . isPrefixOf "#")
        lns

availableMtx :: IO [Name]
availableMtx = readNames $ "config" </> "mtx" <.> "txt"

availableMain :: IO [Name]
availableMain = readNames $ "config" </> "main" <.> "txt"

mkDstPath :: FilePath -> FilePath ->  FilePath -> FilePath
mkDstPath fp ext dst =
    flip replaceExtension ext $
    replaceDirectory fp dst

mtxPaths :: IO [(FilePath, FilePath)]
mtxPaths = do
    allMtx <- findByExtension [".mtx"] "dataset"
    onMtx <- availableMtx

    let mtx = fnFilter allMtx onMtx
    let mtx2 = (++ "2") <$> mtx

    return $ zip mtx mtx2

mains :: IO [FilePath]
mains = do
    allMains <- findByExtension [".main"] "hvl"

    fnFilter allMains <$> availableMain
