-----------------------------------------------------------------------------
--
-- Module      :  SqlRepresentation
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


module SqlRepresentation (
    checkAllTableDescriptions,
    checkAllTablesPresent,
    createTables,
    InsertRelationStatement,
    InsertWayStatement,
    InsertNodeStatement,
    prepareInsertRelation,
    prepareInsertWay,
    prepareInsertNode,
    execInsertRelation,
    execInsertWay,
    execInsertNode
) where

import Data.Int
import Database.HDBC
import Data.List
import Data.Char
import Data.Maybe
import OsmData

nodesTable = "Nodes"

waysTable = "Ways"

edgesTable = "Edges"

relationsTable = "Relations"

relationContentsTable = "RelationContents"

attributesTable = "Attributes"

nodesTableCreate = "CREATE TABLE " ++ nodesTable ++
    " (ID INTEGER NOT NULL PRIMARY KEY, Latitude REAL NOT NULL, Longitude REAL NOT NULL)"

waysTableCreate = "CREATE TABLE " ++ waysTable ++
    " (ID INTEGER NOT NULL PRIMARY KEY)"

edgesTableCreate = "CREATE TABLE " ++ edgesTable ++
    " (WayID INTEGER NOT NULL, StartNodeID INTEGER NOT NULL, EndNodeID INTEGER NOT NULL)"

relationsCreate = "CREATE TABLE " ++ relationsTable ++
    " (ID INTEGER NOT NULL PRIMARY KEY)"

relationContentsCreate = "CREATE TABLE " ++ relationContentsTable ++
    " (RelationID INTEGER NOT NULL, Role TEXT NOT NULL, ObjectID INTEGER NOT NULL, ObjectType INTEGER NOT NULL)"

attributesCreate = "CREATE TABLE " ++ attributesTable ++
    " (ObjectID INTEGER NOT NULL, ObjectType INTEGER NOT NULL, Key TEXT, Value TEXT)"

toLowerS = map toLower

allTables = sort . (map toLowerS) $
    [nodesTable, waysTable, edgesTable, relationsTable, relationContentsTable, attributesTable]

allTableCols = map (\(x,y)->(toLowerS x, y)) [
    (nodesTable,
        [("ID", SqlIntegerT, False),
        ("Latitude", SqlRealT, False),
        ("Longitude", SqlRealT, False)]),
    (waysTable,
        [("ID", SqlIntegerT, False)]),
    (edgesTable,
        [("WayID", SqlIntegerT, False),
        ("StartNodeID", SqlIntegerT, False),
        ("EndNodeID", SqlIntegerT, False)]),
    (relationsTable,
        [("ID", SqlIntegerT, False)]),
    (relationContentsTable,
        [("RelationID", SqlIntegerT, False),
        ("Role", SqlVarCharT, False),
        ("ObjectID", SqlIntegerT, False),
        ("ObjectType", SqlIntegerT, False)]),
    (attributesTable,
        [("ObjectID", SqlIntegerT, False),
        ("ObjectType", SqlIntegerT, False),
        ("Key", SqlVarCharT, False),
        ("Value", SqlVarCharT, False)])
    ]

createTables :: IConnection a => a -> IO()
createTables conn = do
    mapM (\x -> run conn x []) [nodesTableCreate, waysTableCreate, edgesTable, relationsCreate,
        relationContentsCreate, attributesCreate]
    commit conn

checkAllTablesPresent conn = do
    tables <- getTables conn
    return (allTables == (sort . (map toLowerS)) tables)

checkAllTableDescriptions conn = checkTableDescriptions conn allTables

checkTableDescriptions :: IConnection a => a -> [String] -> IO Bool
checkTableDescriptions _ [] = return True
checkTableDescriptions conn (table:tables) = do
    descs <- describeTable conn table
    val <- checkTableDescriptions conn tables
    let myCds = lookup table allTableCols
    let thisVal = isJust myCds && checkTableDescription descs (fromJust myCds)
    return (val && thisVal)

checkTableDescription :: [(String, SqlColDesc)] -> [(String, SqlTypeId, Bool)] -> Bool
checkTableDescription cds myCds = (length cds == length myCds) && all (checkColumns cds) myCds

checkColumns :: [(String, SqlColDesc)] -> (String, SqlTypeId, Bool) -> Bool
checkColumns cds myCd@(name, _, _) = let entry = lookup name cds
    in
        isJust entry &&
        checkColumn (name, fromJust entry) myCd

checkColumn :: (String, SqlColDesc) -> (String, SqlTypeId, Bool) -> Bool
checkColumn (name1, cp) (name2, tp, null) =
    colType cp == tp &&
    name1 == name2 &&
    not (isJust (colNullable cp)) ||
    fromJust (colNullable cp) == null


data InsertNodeStatement = InsertNodeStatement Statement Statement

prepareInsertNode :: IConnection a => a -> IO InsertNodeStatement
prepareInsertNode conn = do
    st <- prepare conn $
        "INSERT INTO " ++ nodesTable ++ " (ID, Latitude, Longitude) VALUES (?, ?, ?)"
    st2 <- prepareAttrStatement conn
    return (InsertNodeStatement st st2)

execInsertNode :: InsertNodeStatement ->
                    Node ->
                    IO ()
execInsertNode (InsertNodeStatement nodeSt attrSt) node = do
    execute nodeSt [toSql $ nodeID node, toSql $ latitude node, toSql $ longitude node]
    mapM (\(x,y) -> execute attrSt [toSql $ nodeID node, toSql NodeType, toSql x,toSql y]) (nodeTags node)
    return ()

data InsertWayStatement = InsertWayStatement Statement Statement Statement

prepareInsertWay :: IConnection a => a -> IO InsertWayStatement
prepareInsertWay conn = do
    st1 <- prepare conn $
        "INSERT INTO " ++ waysTable ++ " (ID) VALUES (?)"
    st2 <- prepare conn $
        "INSERT INTO " ++ edgesTable ++ " (WayID, StartNodeID, EndNodeID) VALUES (?, ?, ?)"
    st3 <- prepareAttrStatement conn
    return (InsertWayStatement st1 st2 st3)

execInsertWay :: InsertWayStatement -> Way -> IO ()
execInsertWay (InsertWayStatement waySt wayNodeSt attrSt) way = do
    execute waySt [toSql $ wayID way]
    mapM (\(x, y) -> execute attrSt [toSql $ wayID way, toSql WayType, toSql x, toSql y]) $ wayTags way
    mapM (\(x, y) -> execute wayNodeSt [toSql $ wayID way, toSql x, toSql y]) (pairUp $ nodes way)
    return ()

prepareAttrStatement conn = prepare conn $ "INSERT INTO " ++ attributesTable ++
        " (ObjectID, ObjectType, Key, Value) VALUES (?, ?, ?, ?)"

data InsertRelationStatement = InsertRelationStatement Statement Statement Statement

prepareInsertRelation :: IConnection a => a -> IO InsertRelationStatement
prepareInsertRelation conn = do
    st1 <- prepare conn $ "INSERT INTO " ++ relationsTable ++
        " (ID) VALUES (?)"
    st2 <- prepare conn $ "INSERT INTO " ++ relationContentsTable ++
        " (RelationID, Role, ObjectID, ObjectType) VALUES (?, ?, ?, ?)"
    st3 <- prepareAttrStatement conn
    return (InsertRelationStatement st1 st2 st3)

execInsertRelation :: InsertRelationStatement ->
                        Relation ->
                        IO ()
execInsertRelation (InsertRelationStatement relSt contentsSt attrSt) rel = do
    execute relSt [toSql $ relID rel]
    mapM (\(x, y, z) -> execute contentsSt [toSql $ relID rel, toSql y, toSql z, toSql x])
            (members rel)
    mapM (\(x, y) -> execute attrSt [toSql $ relID rel, toSql RelationType, toSql x, toSql y])
            (relTags rel)
    return ()

pairUp :: [a] -> [(a,a)]
pairUp [] = []
pairUp x = zip x (tail x)






