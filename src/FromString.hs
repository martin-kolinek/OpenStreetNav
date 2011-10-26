{-#LANGUAGE TypeSynonymInstances, OverloadedStrings#-}

module FromString (
    Parseable,
    parseString
) where

import Data.Int
import OsmData
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Lex.Double
import Data.Char

class Parseable a where
    parseString :: B.ByteString -> Maybe a

instance Parseable B.ByteString where
    parseString = Just

instance Parseable Int where
    parseString = (fmap fst) . BC.readInt

instance Parseable Int64 where
    parseString = (fmap fromInteger) . ((fmap fst) . BC.readInteger)

instance Parseable Double where
    parseString = (fmap fst) . readDouble

instance Parseable ObjectType where
    parseString "node" = Just NodeType
    parseString "way" = Just WayType
    parseString "relation" = Just RelationType
    parseString _ = Nothing




