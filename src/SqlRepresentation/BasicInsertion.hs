module SqlRepresentation.BasicInsertion (
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

import SqlRepresentation.Common
import Data.Int
import Database.HDBC
import Data.List
import Data.Char
import Data.Maybe
import OsmData

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
    execute nodeSt [toSql id, toSql $ latitude node, toSql $ longitude node]
    mapM_ (execAttrSt attrSt id NodeType) (nodeTags node)
    return ()
    where id = nodeID node

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
execInsertWay (InsertWayStatement waySt edgeSt attrSt) way = do
    execute waySt [toSql id]
    mapM (execAttrSt attrSt id WayType) (wayTags way)
    mapM execEdgeSt (pairUp $ nodes way)
    return ()
    where
        id = wayID way
        execEdgeSt (x, y) = execute edgeSt [toSql id, toSql x, toSql y]


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
    execute relSt [toSql id]
    mapM execAddContents (members rel)
    mapM (execAttrSt attrSt id RelationType) (relTags rel)
    return ()
    where
        id = relID rel
        execAddContents (objType, objID, role) =
            execute contentsSt [toSql $ id, toSql role, toSql objID, toSql objType]

pairUp :: [a] -> [(a,a)]
pairUp [] = []
pairUp x = zip x (tail x)

execAttrSt attrSt id objType (x, y) = execute attrSt [toSql $ id, toSql objType, toSql x,toSql y]

