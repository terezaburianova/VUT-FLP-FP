{-|
Module      : Main
Description : Entry point for FLP project.
Author      : Tereza Burianova <xburia28@stud.fit.vutbr.cz>

This module serves as the entry point for the FLP project.
It loads the file, parses the arguments, performs actions based 
on the current mode and prints out the results to the standard output.
-}

module Main where

import Structure (buildTree)
import Classification (classifyAll)
import Training (trainTree)
import Data.Char (isSpace)
import Data.List (transpose)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import System.Directory (doesFileExist)

main :: IO ()
main = do
    (subtask:args) <- getArgs
    case subtask of
        "-1" -> classify args
        "-2" -> train args
        _ -> putStrLn $ "Invalid task number: " ++ subtask

classify :: [String] -> IO ()
classify args =
    if length args < 2 then
        putStrLn "Invalid number of arguments."
    else do
        fileExists1 <- doesFileExist $ head args
        fileExists2 <- doesFileExist $ args!!1
        if fileExists1 && fileExists2 then do
            treeContents <- readFile (head args)
            dataContents <- readFile (args!!1)
            let tree = buildTree $ removeSpaces . replace ',' ':' $ lines treeContents
                clData :: [[Float]]
                clData = map (map read . splitOn ",") $ lines dataContents :: [[Float]]
            case tree of
                (Left e) -> putStrLn e
                (Right t) -> putStr $ init $ unlines $ classifyAll t clData
        else
            putStrLn "Missing input file."

train :: [String] -> IO ()
train args = do
    if null args then
        putStrLn "Invalid number of arguments."
    else do
        fileExists <- doesFileExist $ head args
        if fileExists then do
            trainContents <- readFile (head args)
            let trainData = transpose $ map (splitOn ",") . lines $ trainContents
            if length trainData < 2 then
                putStrLn "Invalid structure of training data."
            else do
                let classes = last trainData
                    datas = map (map read) $ init trainData
                    tree = trainTree datas classes
                print tree
        else
            putStrLn "Missing input file."

-- helper functions for string processing.
replace :: Char -> Char -> [String] -> [String]
replace _ _ [] = []
replace what with (str:strL) = map (\x -> if x == what then with else x) str : replace what with strL

removeSpaces :: [String] -> [String]
removeSpaces = map $ filter (not . isSpace)
