module Graph (
  Graph, Contraction,
  newGraph,
  getSize, getActiveSize, getContractions,
  hasNode, addNode, addNodes,
  getNodeCentrality, getPossibleContractions,
  contract, uncontract)
where

import Node

import qualified Data.List as List

type Contraction = (Tag, Color)

newtype Graph = Graph ([(Tag, Node)], [Contraction]) deriving (Show)

newGraph :: Graph
newGraph = Graph ([], [])

getNodeMap :: Graph -> [(Tag, Node)]
getNodeMap (Graph (ns, _)) = ns

updateNodeMap :: Graph -> [(Tag, Node)] -> Graph
updateNodeMap (Graph (_, cs)) ns = (Graph (ns, cs))

getContractions :: Graph -> [Contraction]
getContractions (Graph (_, cs)) = cs

addContraction :: Graph -> Contraction -> Graph
addContraction g ct = Graph (getNodeMap g, ct : getContractions g)

getTags :: Graph -> [Tag]
getTags = (map getTag) . getNodes

getActiveTags :: Graph -> [Tag]
getActiveTags = (map getTag) . (List.filter isActive) . getNodes

getNodes :: Graph -> [Node]
getNodes g = ns where (_, ns) = unzip (getNodeMap g)

getActiveNodes :: Graph -> [Node]
getActiveNodes = (List.filter isActive) . getNodes

getSize :: Graph -> Int
getSize = length . getNodeMap

getActiveSize :: Graph -> Int
getActiveSize = length . getActiveNodes

hasNode :: Graph -> Tag -> Bool
hasNode g t = elem t (getTags g)

addNode :: Graph -> Node -> Graph
addNode g n = Graph ((getTag n, n) : getNodeMap g, getContractions g)

addNodes :: Graph -> [Node] -> Graph
addNodes g ns = Graph (zip (map getTag ns) ns ++ getNodeMap g, getContractions g)

getNode :: Graph -> Tag -> Node
getNode g t = case lookup t (getNodeMap g) of
  Nothing -> error $ "ERROR @ Graph.getNode: Unknown tag " ++ (show t)
  Just n -> n

getPossibleContractions :: Graph -> [Contraction]
getPossibleContractions g = concatMap (getPossibleNodeContractions g) (getActiveNodes g)

getPossibleNodeContractions :: Graph -> Node -> [Contraction]
getPossibleNodeContractions g n = contractions where
  nodeTag = getTag n
  adjacentNodes = map (getNode g) (getAdjacentTags n)
  adjacentColors = List.nub (map getColor adjacentNodes)
  contractions = map (\c -> (nodeTag, c)) adjacentColors

getNodeCentrality :: Graph -> Tag -> Int
getNodeCentrality g t = getNodeCentralityRec g [] 1 t

getNodeCentralityRec :: Graph -> [Tag] -> Int -> Tag -> Int
getNodeCentralityRec graph seen depth tag = let
  adjacentTags = List.filter (\t -> not $ elem t seen) $ getAdjacentTags $ getNode graph tag
  in case length adjacentTags of
    0 -> 0
    nAdjacent -> thisLevelSum + otherLevelsSum where
      thisLevelSum = depth * nAdjacent
      newSeen = seen ++ adjacentTags
      otherLevelsSum = sum $ map (getNodeCentralityRec graph newSeen $ depth + 1) adjacentTags


contract :: Graph -> (Tag, Color) -> Graph
contract g (targetTag, targetColor) = let
  -- Find all nodes adjacent to the target that have the same color and will be contracted
  targetNode = getNode g targetTag
  adjacentTags = getAdjacentTags targetNode
  adjacentNodes = map (getNode g) adjacentTags
  adjacentColoredNodes = List.filter (\ni -> getColor ni == targetColor) adjacentNodes
  contractedNodes = targetNode : adjacentColoredNodes
  contractedTags = map getTag contractedNodes
  -- Find all nodes adjacent to the contracted nodes that will need to be updated
  affectedTags = (List.nub $ concatMap getAdjacentTags contractedNodes) List.\\ contractedTags
  -- Create a new parent node to wrap the contracted nodes
  parentTag = createParentTag targetTag
  parentNode = newNodeWithTags parentTag targetColor affectedTags contractedTags
  -- Update and return the graph
  g1 = addNode g parentNode -- Add parent node
  g2 = deactivateContractedNodes g1 contractedTags
  g3 = updateAffectedNodes g2 contractedTags affectedTags parentTag
  g4 = addContraction g3 (targetTag, targetColor)
  in g4

deactivateContractedNodes :: Graph -> [Tag] -> Graph
deactivateContractedNodes g cts = updateNodeMap g updatedNodeMap where
  updatedNodeMap = map (\(t, n) -> if elem t cts then (t, deactivate n) else (t, n)) (getNodeMap g)

updateAffectedNodes :: Graph -> [Tag] -> [Tag] -> Tag -> Graph
updateAffectedNodes g cts afts pt = let
  updatedNodes = map (updateAffectedNode cts afts pt) (getNodes g)
  in updateNodeMap g $ zip (map getTag updatedNodes) updatedNodes

updateAffectedNode :: [Tag] -> [Tag] -> Tag -> Node -> Node
updateAffectedNode cts afts pt n = case elem (getTag n) afts of
  False -> n
  True -> updateAdjacentTags n $ map (\t -> if elem t cts then pt else t) $ getAdjacentTags n

-- Pop latest contraction
-- Reactivate contracted nodes
-- Remove parent node
-- Update affected nodes to point back to the contracted nodes
uncontract :: Graph -> Graph
uncontract g = g
