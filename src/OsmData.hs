{-# LANGUAGE DeriveDataTypeable #-}

module OsmData (
    Tag,
    Node(..),
    Way(..),
    Role(..),
    RelationMapping(..),
    Relation(..),
    ObjectType(..)
) where

import Database.HDBC
import Data.Convertible.Base
import Data.Typeable
import Data.Int
import Data.ByteString as B

type Tag = (B.ByteString, B.ByteString)

data Node = Node {nodeID:: Int64, nodeTags:: [Tag], latitude:: Double, longitude:: Double}
    deriving(Show, Eq, Read)

data Way =  Way {wayID:: Int64, nodes:: [Int64], wayTags:: [Tag]}
    deriving(Show, Eq, Read)

type Role = B.ByteString

type RelationMapping = (ObjectType, Int64, Role)

data Relation = Relation {relID:: Int64, members:: [RelationMapping], relTags:: [Tag]}
    deriving(Show, Eq, Read)

data ObjectType = NodeType | WayType | RelationType deriving (Eq, Typeable, Show, Read)









