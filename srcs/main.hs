{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.Directory (doesFileExist, listDirectory)
import System.FilePath.Posix (normalise)

data VLabel = VLDirectory | VLFile deriving (Show)
type V = (FilePath, VLabel)

data ELabel = ELHardlink deriving (Show)
type E = (FilePath, FilePath, ELabel)

type FileGraph = ([V], [E])

readDirectoryGraph :: FilePath -> IO FileGraph
readDirectoryGraph root = do
  isFile <- doesFileExist root
  if isFile then onFile else onDirectory
 where
  onFile :: IO FileGraph
  onFile = return ([(normalise root, VLFile)], [])

  onDirectory :: IO FileGraph
  onDirectory = do
    children <- map (\x -> root ++ "/" ++ x) <$> listDirectory root :: IO [FilePath]
    subgraphs <- mapM readDirectoryGraph children :: IO [FileGraph]
    let vertex = (normalise root, VLDirectory) :: V
    let edges = map (normalise root,,ELHardlink) children :: [E]
    let childVertices = concatMap fst subgraphs :: [V]
    let childEdges = concatMap snd subgraphs :: [E]
    return (vertex : childVertices, edges ++ childEdges)

fileGraphParams :: G.GraphvizParams FilePath VLabel ELabel () VLabel
fileGraphParams =
  G.defaultParams
    { G.fmtNode = \(_, vl) -> case vl of
        VLDirectory -> [G.Color $ G.toColorList [G.RGB 0 0 0]]
        VLFile -> [G.Color $ G.toColorList [G.RGB 255 40 40]]
    , G.fmtEdge = \(_, _, _) -> [G.Color $ G.toColorList [G.RGB 0 0 0]]
    }

main :: IO ()
main = do
  (vertices, edges) <- readDirectoryGraph "tmp"
  let dotGraph = G.graphElemsToDot fileGraphParams vertices edges :: G.DotGraph FilePath
  let dotText = G.printDotGraph dotGraph :: TL.Text
  TL.writeFile "files.dot" dotText
