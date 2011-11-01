{-# LANGUAGE OverloadedStrings #-}

module OsmXmlParser (
    parseOsmSAX
) where

import Data.Char
import Text.XML.Expat.Tree
import Text.XML.Expat.SAX
import Data.Maybe
import Data.Typeable
import qualified OsmData as D
import Data.Int
import Control.Monad
import FromString
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

parseOsmSAX :: (a -> D.Node -> a) ->
            (a -> D.Way -> a) ->
            (a -> D.Relation -> a) ->
            (String -> a) ->
            (a -> Int -> a) ->
            a ->
            [SAXEvent B.ByteString B.ByteString] ->
            a
parseOsmSAX n w r e p s ((StartElement "osm" []):rest) = parseSAXEvents n w r e p 0 s rest
parseOsmSAX _ _ _ e _ _ _ = e "wrong xml start"

parseSAXEvents :: (a -> D.Node -> a) ->
            (a -> D.Way -> a) ->
            (a -> D.Relation -> a) ->
            (String -> a) ->
            (a -> Int -> a) ->
            Int ->
            a ->
            [SAXEvent B.ByteString B.ByteString] ->
            a
parseSAXEvents _ _ _ e _ _ _ [] = e "unexpected end of xml"
parseSAXEvents _ _ _ e _ _ _ ((FailDocument err):_) = e $ show err
parseSAXEvents _ _ _ _ _ _ s ((EndElement "osm"):[]) = s
parseSAXEvents n w r e p int s sax@((StartElement name _):_) = internal
    where
        continue newStart newRest
            | newRest == [] = e "unfinished element"
            | newRest /= [] && head newRest /= EndElement name = e "element mismatch"
            | otherwise = parseSAXEvents n w r e p nextint (p newStart int) (tail newRest)
                            where ni = (int+1)
                                  nextint = ni `seq` ni
        handleTopLevel getter handler = let el = getter sax
                                        in case el of
                                            Left err -> e err
                                            Right (rest, rEl) -> continue (handler s rEl) rest
        internal
            | name == "node" = handleTopLevel getNode n
            | name == "relation" = handleTopLevel getRelation r
            | name == "way" = handleTopLevel getWay w
parseSAXEvents n w r e p int s ((Comment _):rest) = parseSAXEvents n w r e p int s rest
parseSAXEvents n w r e p int s ((CharacterData d):rest) = if BC.all isSpace d
    then
        parseSAXEvents n w r e p int s rest
    else
        e $ "unexpected character data " ++ (BC.unpack d)
parseSAXEvents _ _ _ e _ _ _ (pc:_) = e $ "unexpected xml piece " ++ (show pc)

getNode ((StartElement name attrs):rest) = do
    id <- lookupA attrs "id"
    lat <- lookupA attrs "lat"
    lon <- lookupA attrs "lon"
    (tgs, mmbrs, nds, newRest) <- getSubTags rest
    unless (null mmbrs) $ Left "node containing member elements"
    unless (null nds) $ Left "node containing nd elements"
    return (newRest, D.Node id tgs lat lon)

getRelation ((StartElement name attrs):rest) = do
    id <- lookupA attrs "id"
    (tgs, mmbrs, nds, newRest) <- getSubTags rest
    unless (null nds) $ Left "relation containing nd elements"
    return (newRest, D.Relation id mmbrs tgs)

getWay ((StartElement name attrs):rest) = do
    id <- lookupA attrs "id"
    (tgs, mmbrs, nds, newRest) <- getSubTags rest
    unless (null mmbrs) $ Left "way containing member elements"
    return (newRest, D.Way id nds tgs)

getSubTags :: [SAXEvent B.ByteString B.ByteString] ->
               Either String ([D.Tag], [D.RelationMapping], [Int64], [SAXEvent B.ByteString B.ByteString])
getSubTags [] = Left "unexpected end of xml"
getSubTags rst@((FailDocument _):_) = return ([], [], [], rst)
getSubTags rst@((EndElement _):_) = return ([], [], [], rst)
getSubTags all@((StartElement _ _):_) = do
    (newTags, newMembers, newNodes, newRest) <- getSubTag all
    (tgs, mmbrs, nds, rst) <- getSubTags newRest
    return (newTags ++ tgs, newMembers ++ mmbrs, newNodes ++ nds, rst)
getSubTags ((Comment _):rest) = getSubTags rest
getSubTags ((CharacterData d):rest) = if BC.all isSpace d
                                        then
                                            getSubTags rest
                                        else
                                            Left $ "unexpected character data " ++ (BC.unpack d)
getSubTags (pc:_) = Left $ "unexpected xml piece " ++ (show pc)


getSubTag ((StartElement name attrs):rest) = do
    newRest <- checkEndElement name rest
    when (name /= "tag" && name /= "nd" && name /= "member") (Left "unknown subelement" >> return ())
    newTags <- getTags name attrs []
    newMembers <- getMembers name attrs []
    newNodes <- getNds name attrs []
    return (newTags, newMembers, newNodes, newRest)

checkEndElement name ((EndElement ename):rest) = do
    unless (name == ename) (Left ("unmatched element " ++ (BC.unpack name)) >> return ())
    return rest
checkEndElement _ rest@((FailDocument _):_) = return rest
checkEndElement _ _ = Left "Expected element end"

getTags "tag" attrs tgs = do
    key <- lookupA attrs "k"
    val <- lookupA attrs "v"
    return $ (key, val):tgs
getTags _ _ tgs = return tgs

getMembers "member" attrs mmbrs = do
    tp <- lookupA attrs "type"
    ref <- lookupA attrs "ref"
    role <- lookupA attrs "role"
    return $ (tp, ref, role):mmbrs
getMembers _ _ mmbrs = return mmbrs

getNds "nd" attrs nds = do
    ref <- lookupA attrs "ref"
    return $ ref:nds
getNds _ _ nds = return nds

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just r) = Right r
maybeToEither l Nothing = Left l

lookupA :: (Typeable a, Parseable a) =>
            [(B.ByteString, B.ByteString)] ->
            B.ByteString ->
            (Either String a)
lookupA list attr = do
        val <- maybeToEither notFoundMsg (lookup attr list)
        convertAttr attr val
    where
        notFoundMsg :: String
        notFoundMsg = "Key " ++ (BC.unpack attr) ++ " not found"

convertAttr :: (Typeable a, Parseable a) => B.ByteString -> B.ByteString -> Either String a
convertAttr attr val = maybeToEither (convertErrMsg retType) ret
    where
        ret = parseString val
        retType = typeOf $ fromJust ret
        convertErrMsg :: Show a => a -> String
        convertErrMsg tp = "Unable to convert attribute " ++ (BC.unpack attr) ++ "'s value " ++ (BC.unpack val) ++ " to desired type " ++ show tp













