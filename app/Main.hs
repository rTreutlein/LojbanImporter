{-# LANGUAGE DataKinds #-}
module Main where

import OpenCog.Lojban.Lujvo
import OpenCog.Lojban.Selmaho
import OpenCog.AtomSpace

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [t,filePath] -> do
            file <- readFile filePath
            case t of
                "l" -> let parsed = mapM_ parseLujvoLine (lines file)
                       in runOnNewAtomSpace (parsed >> debug)
                "s" -> do
                    map <- parseSelmao file
                    print map
        _ -> putStrLn "Please give to args (l (lujvo)| s (selmaho)) - file name"
