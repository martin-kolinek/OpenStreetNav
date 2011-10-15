module SqlRepresentation.TableDefinition (
    checkAllTableDescriptions,
    checkAllTablesPresent,
    createTables,
    trySelecting
) where

import SqlRepresentation.Common
import Data.Int
import Database.HDBC
import Data.List
import Data.Char
import Data.Maybe
import OsmData

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

nodesTest = "SELECT ID, Latitude, Longitude FROM " ++ nodesTable ++ " LIMIT 1"
waysTest = "SELECT ID FROM " ++ waysTable ++ " LIMIT 1"
edgeTest = "SELECT WayID, StartNodeID, EndNodeID FROM " ++ edgesTable ++ " LIMIT 1"
relationTest = "SELECT ID FROM " ++ relationsTable ++ " LIMIT 1"
relContentsTest = "SELECT RelationID, Role, ObjectID, ObjectType FROM " ++ relationContentsTable ++ " LIMIT 1"
attributesTest = "SELECT ObjectID, ObjectType, Key, Value FROM " ++ attributesTable ++ " LIMIT 1"

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
        mapM exec [nodesTableCreate, waysTableCreate, edgesTableCreate, relationsCreate,
            relationContentsCreate, attributesCreate]
        commit conn
    where exec x = run conn x []

checkAllTablesPresent :: IConnection a => a -> IO Bool
checkAllTablesPresent conn = do
    tables <- getTables conn
    return (allTables == (sort . (map toLowerS)) tables)

checkAllTableDescriptions :: IConnection a => a -> IO Bool
checkAllTableDescriptions conn = checkTableDescriptions conn allTables

checkTableDescriptions :: IConnection a => a -> [String] -> IO Bool
checkTableDescriptions _ [] = return True
checkTableDescriptions conn (table:tables) = do
    servColDescs <- describeTable conn table
    rest <- checkTableDescriptions conn tables
    let myColDescs = lookup table allTableCols
    let thisVal = isJust myColDescs && checkTableDescription servColDescs (fromJust myColDescs)
    return (rest && thisVal)

checkTableDescription :: [(String, SqlColDesc)] -> [(String, SqlTypeId, Bool)] -> Bool
checkTableDescription servColDescs myColDescs = (length servColDescs == length myColDescs) &&
                                                all (checkColumns servColDescs) myColDescs

checkColumns :: [(String, SqlColDesc)] -> (String, SqlTypeId, Bool) -> Bool
checkColumns servColDescs myColDesc@(name, _, _) = let entry = lookup name servColDescs
    in
        isJust entry &&
        checkColumn (name, fromJust entry) myColDesc

checkColumn :: (String, SqlColDesc) -> (String, SqlTypeId, Bool) -> Bool
checkColumn (name1, cp) (name2, tp, null) =
    colType cp == tp &&
    name1 == name2 &&
    not (isJust (colNullable cp)) ||
    fromJust (colNullable cp) == null

trySelecting :: IConnection a => a -> IO Bool
trySelecting conn = do
    catchSql runTestSelects catchErr
        where
            execTest t = quickQuery' conn t []
            runTestSelects = do
                mapM execTest [nodesTest, waysTest, edgeTest, relationTest, relContentsTest, attributesTest]
                return True
            catchErr x = return False
