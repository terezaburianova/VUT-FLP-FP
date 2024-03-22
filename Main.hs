{-|
Module      : Main
Description : Entry point for FLP project.
Author      : Tereza Burianova <xburia28@stud.fit.vutbr.cz>

This module serves as the entry point for the FLP project.
It loads the file, parses the arguments and performs actions based on the current mode.
-}

module Main where

import Structure ( Tree, buildTree, TreeZipper )
import Classification
import Data.Char (isSpace)
import Data.List (sortBy)
import Data.Function (on)

main :: IO ()
main = do
    -- (subtask:args) <- getArgs
    load >>= print
    -- case subtask of
    --     "1" -> classify
    --     "2" -> train

load :: IO (Either String Tree)
load = do
    contents <- readFile "tree.txt"
    let newContents = removeSpaces . replace ',' ':' $ lines contents
    return $ buildTree newContents

-- helper functions for string processing
replace :: Char -> Char -> [String] -> [String]
replace _ _ [] = []
replace what with (str:strL) = map (\x -> if x == what then with else x) str : replace what with strL

removeSpaces :: [String] -> [String]
removeSpaces = map $ filter (not . isSpace)
