{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PartialTypeSignatures  #-}
module Main where

import OpenCog.Lojban.Lujvo
import OpenCog.Lojban.Selmaho
import OpenCog.Lojban.Gismu
import OpenCog.AtomSpace

import System.Environment

import Text.XML.HXT.Core
import qualified Data.Map as M

main :: IO ()
main = do
    args <- getArgs
    case args of
        (src:_) -> application src
        _ -> putStrLn "Please provide source file"

application :: String -> IO ()
application src = do
    gismu <- runX (readDocument [] src >>> getChildren >>> getValsi >>> getGismu)
    cmavo <- runX (readDocument [] src >>> getChildren >>> getValsi >>> getCmavo)
    print $ M.fromListWith (++) $ map f cmavo
    where f (s,c) = (takeWhile p s,[c])
          p e = e `notElem` "1234567890*"


getGismu :: ArrowXml a => a XmlTree String
getGismu = hasAttrValue "type" (== "gismu")
           >>>
           getAttrValue "word"

getCmavo :: ArrowXml a => a XmlTree (String,String)
getCmavo = hasAttrValue "type" (== "cmavo")
           >>>
           (getChildren >>> hasName "selmaho" /> getText)
           &&&
           getAttrValue "word"

getValsi :: ArrowXml a => a XmlTree XmlTree
getValsi = hasName "dictionary"
           />
           hasName "direction"
           />
           hasName "valsi"
