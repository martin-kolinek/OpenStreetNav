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
parseSubchildren s f n = mapM f $ filter (\x -> eName x == s) (filter elements $ eChildren n)
    where
        elements (Element _ _ _) = True
        elements _ = False

parseAttribs :: (Typeable a, Parseable a, Typeable b, Parseable b) =>
                    Node String String ->
                    Either String [(a, b)]
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
            tp <- lookupA node "type"
            return (tp, ref, role)


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just r) = Right r
maybeToEither l Nothing = Left l

lookupA :: (Typeable a, Parseable a) =>
            (Node String String) ->
            String ->
            (Either String a)
lookupA node attr = do
        val <- maybeToEither notFoundMsg (lookup attr (eAttributes node))
        convertAttr attr val
    where
        notFoundMsg = "Key " ++ attr ++ " not found in " ++ (eName node)

convertAttr :: (Typeable a, Parseable a) => String -> String -> Either String a
convertAttr attr val = maybeToEither (convertErrMsg retType) ret
    where
        ret = parseString val
        retType = typeOf $ fromJust ret
        convertErrMsg tp = "Unable to convert attribute " ++ attr ++ "'s value " ++ val ++ " to desired type " ++ show tp













