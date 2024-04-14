import Data.Graph.DGraph (DGraph, fromArcsList)
import Data.Graph.Types (Arc (..))
import Data.Graph.Visualize (plotDGraph)

myGraph :: DGraph String String
myGraph =
  fromArcsList
    [ Arc "Paper" "Rock" "Cover"
    , Arc "Rock" "Scissors" "Crush"
    , Arc "Scissors" "Paper" "Cut"
    ]

main :: IO ()
main = do
  x <- plotDGraph myGraph
  print x
