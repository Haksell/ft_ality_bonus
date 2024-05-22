{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.Directory (doesFileExist, getSymbolicLinkTarget, listDirectory, pathIsSymbolicLink)
import System.FilePath.Posix (normalise)

data VLabel
  = VLDirectory
  | VLSymlink
  | VLFile
  deriving (Show)
type V = (FilePath, VLabel)

data ELabel
  = ELHardlink
  | ELSymlink
  deriving (Show)
type E = (FilePath, FilePath, ELabel)

type FileGraph = ([V], [E])

readDirectoryGraph :: FilePath -> IO FileGraph
readDirectoryGraph root = do
  isSymlink <- pathIsSymbolicLink root
  if isSymlink
    then onSymlink
    else do
      isFile <- doesFileExist root
      if isFile
        then onFile
        else onDirectory
 where
  onSymlink :: IO FileGraph
  onSymlink = do
    target <- normalise <$> getSymbolicLinkTarget root
    return ([(root, VLSymlink)], [(root, target, ELSymlink)])

  onFile :: IO FileGraph
  onFile = do
    return ([(normalise root, VLFile)], [])

  onDirectory :: IO FileGraph
  onDirectory = do
    children <- map (\x -> root ++ "/" ++ x) <$> listDirectory root :: IO [FilePath]
    subgraphs <- mapM readDirectoryGraph children :: IO [FileGraph]
    let v = (normalise root, VLDirectory) :: V
        es = map (normalise root,,ELHardlink) children :: [E]
        childVertices = concatMap fst subgraphs :: [V]
        childEdges = concatMap snd subgraphs :: [E]
    return (v : childVertices, es ++ childEdges)

fileGraphParams :: G.GraphvizParams FilePath VLabel ELabel () VLabel
fileGraphParams =
  G.defaultParams
    { G.fmtNode = \(_, vl) -> case vl of
        VLDirectory -> colorAttribute $ G.RGB 0 0 0
        VLSymlink -> colorAttribute $ G.RGB 40 255 40
        VLFile -> colorAttribute $ G.RGB 255 40 40
    , G.fmtEdge = \(_, _, el) -> case el of
        ELHardlink -> colorAttribute $ G.RGB 0 0 0
        ELSymlink -> colorAttribute $ G.RGB 40 255 40
    }
 where
  colorAttribute color = [G.Color $ G.toColorList [color]]

main :: IO ()
main = do
  (vs, es) <- readDirectoryGraph "tmp"
  let dotGraph = G.graphElemsToDot fileGraphParams vs es :: G.DotGraph FilePath
  let dotText = G.printDotGraph dotGraph :: TL.Text
  TL.writeFile "files.dot" dotText
