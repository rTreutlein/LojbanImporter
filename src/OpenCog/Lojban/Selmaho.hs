module OpenCog.Lojban.Selmaho where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.String

import Prelude hiding (writeFile)
import Data.Serialize
import Data.ByteString.Lazy hiding (foldr)

import qualified Data.Map as M

parseSelmao :: String -> IO (M.Map String [String])
parseSelmao file = do
    let parsed = foldr foldf M.empty (lines file)
        foldf l m = case parseSelmaoLine l of
                    Just (e,k) -> snd $ M.insertLookupWithKey ilwt k [e] m
                    Nothing -> m
        ilwt key [newval] oldval = newval : oldval
    writeFile "cmavoSelmahoMap.bin" $ encodeLazy parsed
    return parsed

parseSelmaoLine :: String -> Maybe (String,String)
parseSelmaoLine line = do
    let parsed = parse parseSelmaoLine' "cmavo_selaho_order.txt" line
    case parsed of
        Left e  -> error $ show e
        Right a -> Just a

parseSelmaoLine' :: Parser (String,String)
parseSelmaoLine' = do
    space
    word <- many (noneOf " ")
    space
    selmao <- many (noneOf " ")
    return (word,selmao)
