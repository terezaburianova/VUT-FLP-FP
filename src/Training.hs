module Training (trainTree) where

import Structure (Tree(Node, Leaf, None), Class)
import Data.List (minimumBy, nub, partition, sort, transpose)
import Data.Ord (comparing)

type Midpoint = Float
type ColIndex = Int

trainTree :: [[Float]] -> [Class] -> Tree
trainTree cols clas
    | null cols = None
    | length (nub clas) == 1 = Leaf (head clas)
    | null clasL = Leaf (head clasR)
    | null clasR = Leaf (head clasL)
    | otherwise = Node True index midpoint (trainTree colsL clasL) (trainTree colsR clasR)
    where
        ((_,midpoint),index) = getColByGini cols clas
        ((colsL,clasL),(colsR,clasR)) = partitionedData cols clas index midpoint

-- Get the chosen column and the midpoint based on the Gini index (lowest value).
getColByGini :: [[Float]] -> [Class] -> ((Float, Midpoint), ColIndex)
getColByGini cols clas = minimumBy (comparing (fst . fst)) allColsZip
    where
        allCols = map (`giniCol` clas) cols
        allColsZip = zip allCols [0..]

-- Compute Gini index of one data column.
giniCol :: [Float] -> [Class] -> (Float, Midpoint)
giniCol col clas = minimumBy (comparing fst) allGinis
    where
        allGinis = allGinisCol col clas (midpoints $ sort col)

-- Helper function to get all Gini index values for a column.
allGinisCol :: [Float] -> [Class] -> [Midpoint] -> [(Float, Midpoint)]
allGinisCol _ _ [] = []
allGinisCol col clas (mp:mps) = (giniForMidpoint mp col clas, mp) : allGinisCol col clas mps

-- Get list of midpoints from a column. midpoints $ fst col
midpoints :: [Float] -> [Midpoint]
midpoints [] = []
midpoints (x:xs)
    | null xs = []
    | otherwise = x + (head xs - x) / 2 : midpoints xs

-- Compute Gini index for one midpoint of the column.
giniForMidpoint :: Midpoint -> [Float] -> [Class] -> Float
giniForMidpoint midpoint col clas = giniWSum (length fstClas, length sndClas) (length col) (gini fstClas, gini sndClas)
    where
        zippedColsClasses = zip col clas
        (fstClas,sndClas) = partitionedClasses zippedColsClasses midpoint

-- Compute Gini index for a node by doing a weighted average of its leaves.
giniWSum :: (Int, Int) -> Int -> (Float, Float) -> Float
giniWSum itemsInHalves itemsTotal ginis
        | itemsTotalF == 0.0 = 0.0
        | otherwise = (itemsInFst / itemsTotalF) * fst ginis + (itemsInSnd / itemsTotalF) * snd ginis
    where
        itemsInFst = fromIntegral (fst itemsInHalves)
        itemsInSnd = fromIntegral (snd itemsInHalves)
        itemsTotalF = fromIntegral itemsTotal

-- Compute Gini index of one leaf.
gini :: [Class] -> Float
gini clas = 1 - giniSum (nub clas) clas

-- Helper function for Gini index computation.
giniSum :: [Class] -> [Class] -> Float
giniSum [] _ = 0
giniSum _ [] = 0
giniSum (clas:uniqueClas) clasData = (fromIntegral (occurences clas clasData) / fromIntegral (length clasData)) ** 2 - giniSum uniqueClas clasData

-- Get number of occurences of a String in a list of Strings.
occurences :: String -> [String] -> Int
occurences val = length . filter (== val)

-- Get two lists of Classes partitioned based on a column of numerical data and a midpoint.
partitionedClasses :: [(Float, Class)] -> Midpoint -> ([Class], [Class])
partitionedClasses pairs midpoint = (map snd fstHalf, map snd sndHalf)
    where (fstHalf,sndHalf) = partition (\(x, _) -> x <= midpoint) pairs

partitionedData :: [[Float]] -> [Class] -> ColIndex -> Midpoint -> (([[Float]], [Class]), ([[Float]], [Class]))
partitionedData datas clas ind mp = ((getCols leftD, getClas leftD), (getCols rightD, getClas rightD))
    where
        (leftD, rightD) = partition (\(list,_) -> list!!ind <= mp) $ zip (transpose datas) clas
        getCols = transpose . map fst
        getClas = map snd
