-----------------------------------------------------------------------------
--
-- Module      :  OsmXmlParser
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module OsmXmlParser (
    parseRelation,
    parseNode,
    parseWay
) where

import Text.XML.Expat.Tree
import Data.Maybe
import Data.Typeable
import qualified OsmData as D
import Data.Int

parseRelation :: (Node String String) -> Either String D.Relation
parseRelation n = do
    id <- lookupA n "id"
    members <- parseRelationMembers n
    attrs <- parseAttribs n
    return D.Relation{D.relID = id, D.members = members, D.relTags = attrs}

parseWay :: (Node String String) -> Either String D.Way
parseWay n = do
    id <- lookupA n "id"
    nodes <- parseWayNodes n
    attrs <- parseAttribs n
    return D.Way{D.wayID = id, D.nodes = nodes, D.wayTags = attrs}

parseNode :: (Node String String) -> Either String D.Node
parseNode n = let la = lookupA n
    in do
        id <- lookupA n "id"
        lat <- la "lat"
        lon <- la "lon"
        attrs <- parseAttribs n
        return D.Node{D.nodeID = id, D.latitude = lat, D.longitude = lon, D.nodeTags = attrs}

parseSubchildren :: String ->
                    (Node String String -> Either String b) ->
                    Node String String ->
                    Either String [b]
parseSubchildren s f n = mapM f $ filter (\x -> eName x == s) (eChildren n)

parseAttribs :: (Typeable a, Read a, Typeable b, Read b) => Node String String -> Either String [(a, b)]
parseAttribs = parseSubchildren "tag" getKV
    where getKV node = do
            key <- lookupA node "k"
            val <- lookupA node "v"
            return (key, val)

parseWayNodes = parseSubchildren "nd" getRef

getRef :: Node String String -> Either String Int64
getRef = (flip lookupA) "ref"

parseRelationMembers :: Node String String -> Either String [(D.ObjectType, Int64, String)]
parseRelationMembers = parseSubchildren "member" getMem
    where getMem node = do
            ref <- getRef node
            role <- lookupA node "role"
            tp <- lookupAUsing readObjectType node "type"
            return (tp, ref, role)


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just r) = Right r
maybeToEither l Nothing = Left l

lookupA :: (Typeable a, Read a) => (Node String String) -> String -> Either String a
lookupA = lookupAUsing tryRead

lookupAUsing :: (Typeable a) =>
                (String -> Maybe a) ->
                (Node String String) ->
                String ->
                (Either String a)
lookupAUsing readFunc node attr = do
        val <- maybeToEither notFoundMsg (lookup attr (eAttributes node))
        convertAttrUsing readFunc attr val
    where
        notFoundMsg = "Key " ++ attr ++ " not found in " ++ (eName node)

convertAttrUsing :: Typeable a => (String -> Maybe a) -> String -> String -> Either String a
convertAttrUsing readFunc attr val = maybeToEither (convertErrMsg retType) ret
    where
        ret = readFunc val
        retType = typeOf $ fromJust ret
        convertErrMsg tp = "Unable to convert attribute " ++ attr ++ "'s value " ++ val ++ " to desired type " ++ show tp

tryRead :: Read a => String -> Maybe a
tryRead str = let r = reads str
              in
                if null r
                    then tryRead $ "\""++str++"\""
                    else Just (fst . head $ r)


readObjectType :: String -> Maybe D.ObjectType
readObjectType "node" = Just D.NodeType
readObjectType "way" = Just D.WayType
readObjectType "relation" = Just D.RelationType
readObjectType _ = Nothing

