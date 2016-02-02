module OpenCog.Lojban.Lujvo where

import OpenCog.AtomSpace
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.String
import Data.List hiding (insert)
import Data.Array

import Debug.Trace


parseLujvoLine :: String -> AtomSpace ()
parseLujvoLine line = do
    let parsed = parse parseLine' "Lujvo" line
    case parsed of
        Left e  -> return ()
        Right a -> a

parseLine' :: Parser (AtomSpace ())
parseLine' = do
    lujvo <- many (noneOf " ")
    space
    gismu <- sepBy (many letterChar) (char '+')
    rels  <- sepBy (parseRels gismu) (char ',')
    let n = length rels
        a = listArray (1,5) (replicate 5 (Node "ConceptNode" "zo'e" noTv))
        e = Link "EquivalenceLink"
                [Link "EvaluationLink"
                    [Node "PredicateNode" lujvo noTv
                    ,genVariabelList n
                    ] noTv
                ,Link "AndLink" (genEvalLinks gismu rels a) noTv
                ] noTv
    return $ insert e

parseRels :: [String] -> Parser [String]
parseRels gismus = do
    skipSome (noneOf "12345")
    digitChar
    string " = "
    sepBy (many alphaNumChar <* char ' ' <* many (noneOf ", ")) (string " = ")

genVariabelList :: Int -> Atom
genVariabelList n = Link "ListLink" (genVariabelList' n 1) noTv

genVariabelList' :: Int -> Int -> [Atom]
genVariabelList' 1 i = [Node "VariableNode" ("$X"++show i) noTv]
genVariabelList' n i =
    (Node "VariableNode" ("$X"++show i) noTv):genVariabelList' (n-1) (i+1)

genEvalLinks :: [AtomName] -> [[AtomName]] -> Array Int Atom -> [Atom]
genEvalLinks [] _  _ = []
genEvalLinks (g:gs) rels a = Link "EvaluationLink"
                                [Node "PredicateNode" g noTv
                                ,Link "ListLink" (elems $ genCVList g rels 1 a) noTv
                                ] noTv
                           :genEvalLinks gs rels a

genCVList :: AtomName -> [[AtomName]] -> Int -> Array Int Atom -> Array Int Atom
genCVList g [x] n a
    | g `myelem` x = a//getUpdate g x n
    | otherwise    = a
genCVList g (x:xs) n a
    | g `myelem` x = genCVList g xs (n+1) $ a//getUpdate g x n
    | otherwise    = genCVList g xs (n+1) a

getUpdate g x n = [(i,node)]
    where node        = Node "VariableNode" ("$X"++show n) noTv
          (Just elem) = myelem' g x
          i           = read [last elem]

myelem :: (Eq a, Foldable t) => [a] -> t [a] -> Bool
myelem a = foldl (\b e -> b || a `isPrefixOf` e) False

myelem' :: String -> [String] -> Maybe String
myelem' a = foldl foldFunc Nothing
    where foldFunc Nothing e = if a `isPrefixOf` e then Just e else Nothing
          foldFunc (Just b) e = Just b
