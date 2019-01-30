{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BuildGraph 
    ( listAll
    , readFiles
    , listDrvs
    , getNodes
    , getEdges
    , getNodeMap
    , removeNothing
    , writeDot
    ) where

import Data.Either (lefts, rights)
import System.Directory
import System.FilePath (takeFileName, dropExtension)
import Data.List
import Prelude hiding (readFile, writeFile)
import Data.Text.IO (readFile, writeFile)
import qualified Data.Text.Lazy.IO as LazyIO
import qualified Data.Maybe as Mb
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as T.Lazy
import qualified Data.Attoparsec.Text as APT
import Nix.Derivation (Derivation(inputDrvs), parseDerivation) 
import Text.Regex.TDFA
import Filesystem.Path.CurrentOS (toText, filename)
import Data.GraphViz hiding (makeNode )
import Data.GraphViz.Commands
import Data.GraphViz.Attributes.Complete (Attribute(Comment))
import Data.GraphViz.Printing
import Data.GraphViz.Attributes
import Debug.Trace
import Data.List

data Category = Haskell
              | Javascript
              | Texlive 
              | Ruby
              | Java
              | OCaml
              | Rust
              | Cpp
              | Python2
              | Python3
              | Perl 
              | Archive
              | Patch
              | Diff
              | Golang
              | Font 
              | Lisp 
              | None
              deriving (Show)

type Vertex = (T.Text, Category)
type Edge = (T.Text, T.Text, T.Text)
type DrvGraph = ([Vertex], [Edge])

classify :: T.Text -> Category
classify drvName
    | s drvName =~ s"(\\.tar\\.gz$|\\.xz$|\\.zip$|\\.tar.bz2$|\\.tgz$)"    = Archive
    | s drvName =~ s"texlive*"                              = Texlive
    | s drvName =~ s"^python2"                              = Python2
    | s drvName =~ s"^python3"                              = Python3
    | s drvName =~ s"(\\.otf$|\\.ttf$)"                     = Font
    | s drvName =~ s"(^node.*|\\.js)$"                      = Javascript
    | s drvName =~ s"(.*.gem|^ruby)"                        = Ruby
    | s drvName =~ s"(\\.jar$|\\.pom$)"                     = Java
    | s drvName =~ s"^ocaml.*"                              = OCaml
    | s drvName =~ s"^rust.*"                               = Rust
    | s drvName =~ s"^perl.*"                               = Perl
    | s drvName =~ s"^lisp.*"                               = Lisp
    | s drvName =~ s".patch$"                               = Patch
    | s drvName =~ s".diff$"                                = Diff
    | s drvName =~ s"(golang|^go-|-[A-Za-z0-9]{7}$)"        = Golang
    | s drvName =~ s"\\.cabal$"                             = Haskell
    | otherwise                                             = None
  where s :: T.Text -> String
        s = T.unpack

outputFile :: FilePath
outputFile = "out.dot"

makeEdge :: (T.Text, T.Text) -> Edge
makeEdge (name1, name2) = (name1, name2, (""))

makeNode :: (T.Text, Category) -> Vertex
makeNode (name, attribute) = (name, attribute)

writeDot :: String -> IO ()
writeDot storePath = do
    print "reading derivations"
    derivations <- getNodeMap storePath
    print "write oneHot derivations with dependencies"
    let filteredDrvs = removeNothing derivations
    writeOneHotMap filteredDrvs
    print "preparing vertices and edges"
    let nodeList = getNodes filteredDrvs
    let nodeAttributes = classify <$> nodeList
    let edgeList = getEdges filteredDrvs
    let edgesLabeled = map makeEdge edgeList
    let nodesLabeled = map makeNode $ zip nodeList nodeAttributes
    print $ "nvertices " ++ show (length nodesLabeled)
    print $ "nedges " ++ show (length edgesLabeled)
    print "preparing diagram"
    let graphInDotFormat = graphElemsToDot fileGraphParams nodesLabeled edgesLabeled
    LazyIO.writeFile outputFile $ renderDot $ toDot graphInDotFormat

writeOneHotMap :: Map.Map T.Text [T.Text] -> IO()
writeOneHotMap drvMap = do
    let allDrvs = nub $ concat $ [Map.keys drvMap, concat (Map.elems drvMap)]
    let lineTuples = Map.toList (map2oneHot drvMap allDrvs)
    let lineStrings = map (\(x, y)-> T.pack (show x ++ " " ++ show y)) lineTuples
         in writeFile "out.map" $ T.unlines lineStrings

map2oneHot :: Map.Map T.Text [T.Text] -> [T.Text] -> Map.Map T.Text T.Text
map2oneHot drvMap allDrvs = Map.map (oneHot allDrvs) drvMap

oneHot :: [T.Text] -> [T.Text] -> T.Text
oneHot union elList = T.pack (show ((Mb.fromMaybe (-1)) <$> (fmap ($union) (elemIndex <$> elList))))

shrinkPath :: T.Text -> T.Text
shrinkPath path = T.tail $ T.dropWhile (/='-') $ T.pack $ dropExtension $ takeFileName $ T.unpack path

listAll :: String -> IO ()
listAll storePath = do
    drvs <- getNodeMap storePath
    let filteredDrvs = removeNothing drvs 
    let edges = getEdges filteredDrvs
    let nodes = getNodes filteredDrvs
    print edges 
    print nodes

fileGraphParams :: GraphvizParams T.Text Category T.Text () Category
fileGraphParams = defaultParams {
                                 fmtNode = \(v, vl) -> ([Comment (T.Lazy.pack (show vl))])
                                ,fmtEdge = \(from, to, el) -> [fillColor Yellow]
                                }


getNodes :: Map.Map T.Text [T.Text] -> [T.Text]
getNodes nodeMap = Map.keys nodeMap

getEdges :: Map.Map T.Text [T.Text] -> [(T.Text, T.Text)]
getEdges nodeMap = (concatMap (\(k, vs) -> map (\v -> (k, v)) vs) . Map.toList) nodeMap

removeNothing :: Map.Map T.Text (Maybe [T.Text]) -> Map.Map T.Text [T.Text]
removeNothing nodeMap = Map.map Mb.fromJust $ Map.filter Mb.isJust nodeMap

getNodeMap :: String -> IO (Map.Map T.Text (Maybe [T.Text]))
getNodeMap storePath = do
    files <- readFiles $ listDrvs storePath
    let nodeMap = Map.map parseNode files
    return nodeMap

parseNode :: T.Text -> Maybe [T.Text]
parseNode drv = case parseNix drv of
                Nothing -> Nothing
                Just derivation -> Just $ shrinkPath <$> (rights (toText <$> Map.keys (inputDrvs derivation)))

parseNix :: T.Text -> Maybe Derivation
parseNix drv = case APT.parse parseDerivation drv of
        APT.Fail _ _ string   -> Nothing 
        APT.Done _ derivation -> Just derivation

readFiles :: IO [String] -> IO (Map.Map T.Text T.Text)
readFiles iofnames = do
    fnames <- iofnames
    texts <- sequence (readFile <$> fnames)
    return $ Map.fromList $ zip (shrinkPath <$> (T.pack <$> fnames)) texts

listDrvs :: String -> IO [String]
listDrvs storePath = do
    fnames <- getDirectoryContents storePath
    let paths = (storePath++) <$> fnames
    return $ filter (isSuffixOf ".drv") paths
