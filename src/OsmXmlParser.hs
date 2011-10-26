{-# LANGUAGE OverloadedStrings #-}

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
import FromString
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

parseRelation :: (UNode B.ByteString) -> Either String D.Relation
parseRelation n = do
    id <- lookupA n "id"
    members <- parseRelationMembers n
    attrs <- parseAttribs n
    return D.Relation{D.relID = id, D.members = members, D.relTags = attrs}

parseWay :: (UNode B.ByteString) -> Either String D.Way
parseWay n = do
    id <- lookupA n "id"
    nodes <- parseWayNodes n
    attrs <- parseAttribs n
    return D.Way{D.wayID = id, D.nodes = nodes, D.wayTags = attrs}

parseNode :: (UNode B.ByteString) -> Either String D.Node
parseNode n = let la = lookupA n
    in do
        id <- lookupA n "id"
        lat <- la "lat"
        lon <- la "lon"
        attrs <- parseAttribs n
        return D.Node{D.nodeID = id, D.latitude = lat, D.longitude = lon, D.nodeTags = attrs}

parseSubchildren :: B.ByteString ->
                    (UNode B.ByteString -> Either String b) ->
                    UNode B.ByteString ->
                    Either String [b]
parseSubchildren s f n = mapM f $ filter (\x -> eName x == s) (filter elements $ eChildren n)
    where
        elements (Element _ _ _) = True
        elements _ = False

parseAttribs :: (Typeable a, Parseable a, Typeable b, Parseable b) =>
                    UNode B.ByteString ->
                    Either String [(a, b)]
parseAttribs = parseSubchildren "tag" getKV
    where getKV node = do
            key <- lookupA node "k" 
            val <- lookupA node "v" 
            return (key, val)

parseWayNodes = parseSubchildren "nd" getRef

getRef :: UNode B.ByteString -> Either String Int64
getRef = (flip lookupA) "ref"

parseRelationMembers :: UNode B.ByteString -> Either String [D.RelationMapping]
parseRelationMembers = parseSubchildren "member" getMem
    where getMem node = do
            ref <- getRef node
            role <- lookupA node "role"
            tp <- lookupA node "type"
            return (tp, ref, role)


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just r) = Right r
maybeToEither l Nothing = Left l

lookupA :: (Typeable a, Parseable a) =>
            (UNode B.ByteString) ->
            B.ByteString ->
            (Either String a)
lookupA node attr = do
        val <- maybeToEither notFoundMsg (lookup attr (eAttributes node))
        convertAttr attr val
    where
        notFoundMsg :: String
        notFoundMsg = "Key " ++ (BC.unpack attr) ++ " not found in " ++ (BC.unpack (eName node))

convertAttr :: (Typeable a, Parseable a) => B.ByteString -> B.ByteString -> Either String a
convertAttr attr val = maybeToEither (convertErrMsg retType) ret
    where
        ret = parseString val
        retType = typeOf $ fromJust ret
        convertErrMsg :: Show a => a -> String
        convertErrMsg tp = "Unable to convert attribute " ++ (BC.unpack attr) ++ "'s value " ++ (BC.unpack val) ++ " to desired type " ++ show tp













