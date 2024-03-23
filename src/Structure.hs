{-|
Module      : Structure
Description : Decision tree structure.
Author      : Tereza Burianova <xburia28@stud.fit.vutbr.cz>

This module contains all datatype declarations and functions that work with the decision tree stsructure.
-}

module Structure (Tree(None, Leaf, Node), TreeZipper, buildTree) where

import Text.Read (readEither)
import Data.List.Split (splitOn)

instance Show Tree where
    show :: Tree -> String
    show = showTree 0

showTree :: Int -> Tree -> [Char]
showTree n (Leaf cl) = replicate n ' ' ++ "Leaf: " ++ cl ++ "\n"
showTree n (Node _ ind th l r) = replicate n ' ' ++ "Node: " ++ show ind ++ ", " ++ show th ++ "\n" ++ showTree (n+2) l ++ showTree (n+2) r
showTree n _ = replicate n ' ' ++ "Invalid tree structure.\n"

-- Tree datatype
type Class = String
type Index = Int
type Threshold = Float
type IsFree = Bool
data Tree =
    None |
    Leaf Class |
    Node IsFree Index Threshold Tree Tree
    deriving (Eq)

-- Tree zipper
data Step = L IsFree Index Threshold Tree | R IsFree Index Threshold Tree deriving (Show)
type TreeZipper = [Step]

-- Create root, add other nodes and extract the tree without the zipper context.
buildTree :: [String] -> Either String Tree
buildTree linesAdd =
    case stringToTree (head linesAdd) >>= (\x -> addSubtrees (x,[]) (tail linesAdd)) of
        Right (tree, _) -> Right tree
        Left string -> Left string

-- Add subtrees from list to the existing tree.
addSubtrees :: (Tree, TreeZipper) -> [String] -> Either String (Tree, TreeZipper)
addSubtrees tree [] = Right tree
addSubtrees tree (current:addLines) = insertToTree (getRoot tree) current >>= (`addSubtrees` addLines)

-- Inserts one node from string to tree.
insertToTree :: (Tree, TreeZipper) -> String -> Either String (Tree, TreeZipper)
-- Trying to insert into a Leaf or None.
insertToTree (Leaf _, _) _ = Left "Invalid tree structure: cannot insert into Leaf."
insertToTree (None, _) _ = Left "Invalid tree structure: cannot insert into None."
-- Left subtree of the current Node is empty - add new item.
insertToTree (Node isFree ind th None right, ctx) strTree =
    stringToTree strTree >>= (\x -> return $ getRoot (Node isFree ind th x right, ctx))
-- Left node is free - go left.
insertToTree (Node isFree ind th left@(Node True _ _ _ _) right, ctx) strTree =
    insertToTree (left, L isFree ind th right:ctx) strTree
-- Right subtree of the current Node is empty - add new item.
insertToTree (Node isFree ind th left None, ctx) strTree =
    stringToTree strTree >>= (\x -> return $ getRoot (Node isFree ind th left x, ctx))
-- Left subtree of the current Node is not free, right subtree is free Node - go right.
insertToTree (Node isFree ind th left right@(Node True _ _ _ _), ctx) strTree
    = insertToTree (right, R isFree ind th left:ctx) strTree
-- No subtree is available - label as not free and go up.
insertToTree node strTree = goUpAndLabel node >>= (`insertToTree` strTree)

-- converts a String into the Tree datatype (one node or leaf)
stringToTree :: String -> Either String Tree
stringToTree line
        | null line = Left "The line is empty."
        | head values == "Node" && length values == 3 = readNode
        | head values == "Leaf" && length values == 2 = Right $ Leaf (values!!1)
        | otherwise = Left "Invalid tree structure - wrong label or number of arguments."
    where
        values = splitOn ":" line
        readNode =
            readEither (values!!1) >>= (\x ->
            readEither (values!!2) >>= (\y ->
            pure (Node True x y None None)))

-- Labels the current node as filled and focuses on the parent of the current node.
goUpAndLabel :: (Tree, TreeZipper) -> Either String (Tree, TreeZipper)
goUpAndLabel (Node _ indC thC lC rC, L isFree ind th right:rest) = Right (Node isFree ind th (Node False indC thC lC rC) right, rest)
goUpAndLabel (Node _ indC thC lC rC, R isFree ind th left:rest) = Right (Node isFree ind th left (Node False indC thC lC rC), rest)
goUpAndLabel _ = Left "Invalid tree structure: Cannot go up in the tree structure."

-- Gets the current tree with focus on the root node.
getRoot :: (Tree, TreeZipper) -> (Tree, TreeZipper)
getRoot (top, []) = (top, [])
getRoot node = getRoot (goUp node)
    where
        goUp (current, L isFree ind th right:rest) = (Node isFree ind th current right, rest)
        goUp (current, R isFree ind th left:rest) = (Node isFree ind th left current, rest)
        goUp _ = (None, [])
