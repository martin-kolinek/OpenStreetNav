{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module SqlRepresentation.Common (
    nodesTable,
    waysTable,
    edgesTable,
    relationsTable,
    relationContentsTable,
    attributesTable,
    execute,
    toSql,
    run,
    beginTran,
    commitTran,
    rollbackTran
) where

import Database.SQLite3
import Data.Convertible
import OsmData
import Data.Int
import qualified Database.HDBC as H
import qualified Data.ByteString.Char8 as B

nodesTable = "Nodes"

waysTable = "Ways"

edgesTable = "Edges"

relationsTable = "Relations"

relationContentsTable = "RelationContents"

attributesTable = "Attributes"

toSql :: Convertible a SQLData => a -> SQLData
toSql = convert

beginTran :: Database -> IO ()
beginTran db = run db "BEGIN TRANSACTION" []

commitTran db = run db "COMMIT TRANSACTION" []

rollbackTran db = run db "ROLLBACK TRANSACTION" []

execute :: Statement -> [SQLData] -> IO ()
execute st args = do
    bind st args
    step st
    reset st

run :: Database -> String -> [SQLData] -> IO ()
run db s args = do
    st <- prepare db s
    execute st args
    finalize st

instance Convertible ObjectType SQLData where
    safeConvert NodeType = Right (SQLInteger 0)
    safeConvert WayType = Right (SQLInteger 1)
    safeConvert RelationType = Right (SQLInteger 2)

instance Convertible Int64 SQLData where
    safeConvert = Right . SQLInteger

instance Convertible B.ByteString SQLData where
    safeConvert = Right . SQLText . B.unpack

instance Convertible Double SQLData where
    safeConvert = Right . SQLFloat

instance Convertible ObjectType H.SqlValue where
    safeConvert NodeType = Right (H.SqlInt32 0)
    safeConvert WayType = Right (H.SqlInt32 1)
    safeConvert RelationType = Right (H.SqlInt32 2)
