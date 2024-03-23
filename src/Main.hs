{-|
Module      : Main
Description : Entry point for FLP project.
Author      : Tereza Burianova <xburia28@stud.fit.vutbr.cz>

This module serves as the entry point for the FLP project.
It loads the file, parses the arguments and performs actions based on the current mode.
-}

module Main where

import Structure (buildTree)
import Classification as C
import Data.Char (isSpace)
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
            -- trainContents <- readFile (head args)
            putStrLn "Not ready yet."
        else
            putStrLn "Missing input file."


-- helper functions for string processing
replace :: Char -> Char -> [String] -> [String]
replace _ _ [] = []
replace what with (str:strL) = map (\x -> if x == what then with else x) str : replace what with strL

removeSpaces :: [String] -> [String]
removeSpaces = map $ filter (not . isSpace)
