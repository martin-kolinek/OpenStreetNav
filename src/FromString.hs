{-#LANGUAGE TypeSynonymInstances#-}

module FromString (
    Parseable,
    parseString
) where

import Data.Int
import OsmData

class Parseable a where
    parseString :: String -> Maybe a

tryRead x = let r = reads x
              in
                if null r
                    then Nothing
                    else Just (fst . head $ r)

instance Parseable String where
    parseString = Just . id

instance Parseable Int where
    parseString = tryRead

instance Parseable Int64 where
    parseString = tryRead

instance Parseable Double where
    parseString = tryRead

instance Parseable Float where
    parseString = tryRead

instance Parseable ObjectType where
    parseString "node" = Just NodeType
    parseString "way" = Just WayType
    parseString "relation" = Just RelationType
    parseString _ = Nothing




