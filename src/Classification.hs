{-|
Module      : Classification
Description : desc
Author      : Tereza Burianova <xburia28@stud.fit.vutbr.cz>

desc
-}

module Classification where
import Structure

classifyAll :: Tree -> [[Float]] -> [String]
classifyAll tree = map (decideData tree)

decideData :: Tree -> [Float] -> String
decideData (Leaf c) _ = c
decideData (Node _ inx th l r) clData
    | length clData <= inx = "Not enough data for required index."
    | otherwise = if clData!!inx <= th then decideData l clData else decideData r clData
decideData _ _ = "Invalid node for decision."
