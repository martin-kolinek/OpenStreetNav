{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable #-}

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

type Tag = (String, String)

data Node = Node {nodeID:: Int64, nodeTags:: [Tag], latitude:: Double, longitude:: Double}
    deriving(Show, Eq, Read)

data Way =  Way {wayID:: Int64, nodes:: [Int64], wayTags:: [Tag]}
    deriving(Show, Eq, Read)

type Role = String

type RelationMapping = (ObjectType, Int64, Role)

data Relation = Relation {relID:: Int64, members:: [RelationMapping], relTags:: [Tag]}
    deriving(Show, Eq, Read)

data ObjectType = NodeType | WayType | RelationType deriving (Eq, Typeable, Show, Read)

instance Convertible ObjectType SqlValue where
    safeConvert NodeType = Right (SqlInt32 0)
    safeConvert WayType = Right (SqlInt32 1)
    safeConvert RelationType = Right (SqlInt32 2)

instance Convertible SqlValue ObjectType where
    safeConvert (SqlInt32 0) = Right NodeType
    safeConvert (SqlInt32 1) = Right WayType
    safeConvert (SqlInt32 2) = Right RelationType
    safeConvert v = convError "Value out of range" v








