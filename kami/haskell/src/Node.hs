module Node (
  Node, Tag, Color,
  newNode, newNodeWithTags,
  getTag, getColor,
  getAdjacentTags, updateAdjacentTags,
  getInternalTags, updateInternalTags,
  createChildTag, createParentTag,
  isActive, activate, deactivate)
where

type Tag = String
type Color = String

-- node tag, adjacent node tags, internal node tags
newtype Node = Node (Tag, Color, [Tag], [Tag], Bool) deriving (Show)

newNode :: Tag -> Color -> Node
newNode t c = Node (t, c, [], [], True)

newNodeWithTags :: Tag -> Color -> [Tag] -> [Tag] -> Node
newNodeWithTags t c ats its = Node (t, c, ats, its, True)

getTag :: Node -> Tag
getTag (Node (t, _, _, _, _)) = t

getColor :: Node -> Color
getColor (Node (_, c, _, _, _)) = c

getAdjacentTags :: Node -> [Tag]
getAdjacentTags (Node (_, _, ats, _, _)) = ats

updateAdjacentTags :: Node -> [Tag] -> Node
updateAdjacentTags (Node (t, c, _, its, a)) ats = Node (t, c, ats, its, a)

getInternalTags :: Node -> [Tag]
getInternalTags (Node (_, _, _, its, _)) = its

updateInternalTags :: Node -> [Tag] -> Node
updateInternalTags (Node (t, c, ats, _, a)) its = Node (t, c, ats, its, a)

isActive :: Node -> Bool
isActive (Node (_, _, _, _, a)) = a

activate :: Node -> Node
activate (Node (t, c, ats, its, _)) = Node (t, c, ats, its, True)

deactivate :: Node -> Node
deactivate (Node (t, c, ats, its, _)) = Node (t, c, ats, its, False)

createChildTag :: Tag -> Tag
createChildTag (_ : t) = t

createParentTag :: Tag -> Tag
createParentTag t = '^' : t
