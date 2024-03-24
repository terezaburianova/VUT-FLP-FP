{-|
Module      : Classification
Description : Classification of data based on a decision tree.
Author      : Tereza Burianova <xburia28@stud.fit.vutbr.cz>

This module contains the functions for data classification based on the provided decision tree.
-}

module Classification (classifyAll) where

import Structure (Tree(Node, Leaf))

-- Get classes for more data.
classifyAll :: Tree -> [[Float]] -> [String]
classifyAll tree = map (decideData tree)

-- Get a class for provided data.
decideData :: Tree -> [Float] -> String
decideData (Leaf c) _ = c
decideData (Node _ inx th l r) clData
    | length clData <= inx = "Not enough data for required index."
    | otherwise = if clData!!inx <= th then decideData l clData else decideData r clData
decideData _ _ = "Invalid node for decision."
