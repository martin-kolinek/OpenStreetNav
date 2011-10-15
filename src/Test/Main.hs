module Main (
    main
) where

import Test.HUnit
import System.Exit
import OsmXmlParser
import Database.HDBC
import Database.HDBC.Sqlite3
import SqlRepresentation.BasicInsertion
import SqlRepresentation.TableDefinition
import qualified Text.XML.Expat.Tree as Xml
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified OsmData as D
import System.Directory
import Data.Int
import Data.List
import OsmData
import Control.Exception

testNode = (fst . Xml.parse Xml.defaultParseOptions $ BS.pack
    "<node id=\"23\" lat=\"1.5\" lon=\"2.5\"><tag k=\"key\" v=\"val\" /></node>" :: (Xml.Node String String))

testWay = (fst . Xml.parse Xml.defaultParseOptions $ BS.pack
    "<way id=\"22\"><nd ref=\"11\"/><nd ref=\"12\" /><tag k=\"key\" v=\"val\" /></way>" :: (Xml.Node String String))

testRel = (fst . Xml.parse Xml.defaultParseOptions $ BS.pack (
    "<relation id=\"43\">"++
        "<member type=\"node\" ref=\"10\" role=\"role\"/>"++
        "<member type=\"way\" ref=\"11\" role=\"role\"/>"++
        "<member type=\"relation\" ref=\"12\" role=\"role\"/>"++
        "<tag k=\"key\" v=\"val\" />"++
    "</relation>") :: (Xml.Node String String))

testNode2 = fst . Xml.parse Xml.defaultParseOptions $ BS.pack (
    "<node id=\"5410667\" lat=\"48.1528677\" lon=\"17.1138423\" user=\"YuraH\" uid=\"374661\" visible=\"true\" version=\"7\" changeset=\"9445252\" timestamp=\"2011-10-01T19:29:09Z\">  <tag k=\"highway\" v=\"traffic_signals\"/> </node>")

testNode3 = fst . Xml.parse Xml.defaultParseOptions $ BS.pack (
    "<node id=\"5410667\" lat=\"48.1528677\" lon=\"17.1138423\" user=\"YuraH\" uid=\"374661\" visible=\"true\" version=\"7\" changeset=\"9445252\" timestamp=\"2011-10-01T19:29:09Z\">  <tag k=\"highway\\\" v=\"traffic_signals\"/> </node>")

testNode' = D.Node {D.nodeID = 23, D.latitude = 1.5, D.longitude = 2.5, D.nodeTags = [("key", "val")]}
testWay' = D.Way {D.wayID = 22, D.nodes = [11,12], D.wayTags = [("key", "val")]}
testRel' = D.Relation {D.relID = 43, D.members = [(D.NodeType, 10, "role"), (D.WayType, 11, "role"), (D.RelationType, 12, "role")],
                D.relTags = [("key", "val")]}

testXmlParser = TestLabel "Xml parser" $ TestList
        [TestLabel "way" testParseWay, TestLabel "node" testParseNode, TestLabel "rel" testParseRel,
        TestLabel "node2" testParseNode2, TestLabel "node3" testParseNode3]

testParseWay = TestCase $ do
    let tway = parseWay testWay
    case tway of
        (Left err) -> assertFailure err
        (Right way) -> assertEqual "Way equality" way testWay'

testParseNode = TestCase $ do
    let tnode = parseNode testNode
    case tnode of
        (Left err) -> assertFailure err
        (Right node) -> assertEqual "Node equality" node testNode'

testParseRel = TestCase $ do
    let trel = parseRelation testRel
    case trel of
        (Left err) -> assertFailure err
        (Right rel) -> assertEqual "Node equality" rel testRel'

testParseNode2 = TestCase $ do
    let tnode = parseNode testNode2
    case tnode of
        (Left err) -> assertFailure err
        _ -> return ()

testParseNode3 = TestCase $ do
    let tnode = parseNode testNode3
    case tnode of
        (Left err) -> assertFailure err
        _ -> return ()

testDB = TestLabel "Sql representation" $ TestList
        [TestLabel "db create and check" testDBCreate,
        TestLabel "db statements" testDBStatements,
        TestLabel "db wrong schema" testDBWrongSchema]

testDBCreate = TestCase $ finally test cleanup
    where
        test = do
            conn <- connectSqlite3 "test1.db"
            createTables conn
            commit conn
            disconnect conn
            conn2 <- connectSqlite3 "test1.db"
            a <- checkAllTablesPresent conn2
            assertBool "all tables present" a
            --disabled due to sqlite3 backend not supporting describeTable
            --b <- checkAllTableDescriptions conn2
            --assertBool "descriptions alright" b
            b <- trySelecting conn2
            assertBool "selection" b
            disconnect conn2
        cleanup = removeFile "test1.db"


testDBWrongSchema = TestCase $ finally test cleanup
    where
        test = do
            conn <- connectSqlite3 "test3.db"
            run conn "CREATE TABLE Test (TestCol INTEGER NOT NULL)" []
            commit conn
            disconnect conn
            conn <- connectSqlite3 "test3.db"
            a <- checkAllTablesPresent conn
            assertBool "tables present test" $ not a
            b <- trySelecting conn
            assertBool "selection test " $ not b
            disconnect conn
        cleanup = removeFile "test3.db"

testDBStatements = TestCase $ finally test cleanup
    where
        test = do
            conn <- connectSqlite3 "test2.db"
            createTables conn
            commit conn
            nd <- prepareInsertNode conn
            execInsertNode nd testNode'
            commit conn
            rows <- quickQuery' conn "SELECT ID, Latitude, Longitude FROM Nodes" []
            assertEqual "1 node" 1 (length rows)
            let row = head rows
            assertBool "node value tests" $
                row !! 0 == toSql (23 :: Int64) &&
                row !! 1 == toSql (1.5 :: Double) &&
                row !! 2 == toSql (2.5 :: Double)
            w <- prepareInsertWay conn
            execInsertWay w testWay'
            commit conn
            rows <- quickQuery' conn "SELECT ID FROM Ways" []
            assertEqual "1 way" 1 (length rows)
            let row = head rows
            assertBool "way id" $ row !! 0 == toSql (22 :: Int64)
            rows <- quickQuery' conn "SELECT ID, WayID, StartNodeID, EndNodeID FROM Edges" []
            assertEqual "1 edge" 1 (length rows)
            let row = head rows
            assertBool "edge tests" $
                row !! 1 == toSql (22 :: Int64) &&
                row !! 2 == toSql (11 :: Int64) &&
                row !! 3 == toSql (12 :: Int64)
            rel <- prepareInsertRelation conn
            execInsertRelation rel testRel'
            commit conn
            rows <- quickQuery' conn "SELECT ID FROM Relations" []
            assertEqual "1 rel" 1 (length rows)
            let row = head rows
            assertBool "rel id" $
                row !! 0 == toSql (43 :: Int64)
            rows <- quickQuery' conn "SELECT ID, RelationID, Role, ObjectID, ObjectType FROM RelationContents" []
            assertEqual "3 relcont" 3 (length rows)
            let rows' = sortBy (\x y -> compare (fromSql (x !! 3) :: Int64) (fromSql (y !! 3) :: Int64)) rows
            let row = rows' !! 0
            assertBool "relcont1" $
                row !! 1 == toSql (43 :: Int64) &&
                row !! 2 == toSql ("role" :: String) &&
                row !! 3 == toSql (10 :: Int64) &&
                row !! 4 == toSql NodeType
            let row = rows' !! 1
            assertBool "relcont2" $
                row !! 1 == toSql (43 :: Int64) &&
                row !! 2 == toSql ("role" :: String) &&
                row !! 3 == toSql (11 :: Int64) &&
                row !! 4 == toSql WayType
            let row = rows' !! 2
            assertBool "relcont2" $
                row !! 1 == toSql (43 :: Int64) &&
                row !! 2 == toSql ("role" :: String) &&
                row !! 3 == toSql (12 :: Int64) &&
                row !! 4 == toSql RelationType
            rows <- quickQuery' conn "SELECT ID, ObjectID, ObjectType, Key, Value FROM Attributes" []
            assertEqual "tagcount 3" 3 $ length rows
            let rows' = sortBy (\x y -> compare (fromSql (x !! 1) :: Int64) (fromSql (y !! 1) :: Int64)) rows
            let row = rows' !! 0
            assertBool "tag1" $
                row !! 1 == toSql (22 :: Int64) &&
                row !! 2 == toSql WayType &&
                row !! 3 == toSql ("key" :: String) &&
                row !! 4 == toSql ("val" :: String)
            let row = rows' !! 1
            assertBool "tag1" $
                row !! 1 == toSql (23 :: Int64) &&
                row !! 2 == toSql NodeType &&
                row !! 3 == toSql ("key" :: String) &&
                row !! 4 == toSql ("val" :: String)
            let row = rows' !! 2
            assertBool "tag1" $
                row !! 1 == toSql (43 :: Int64) &&
                row !! 2 == toSql RelationType &&
                row !! 3 == toSql ("key" :: String) &&
                row !! 4 == toSql ("val" :: String)
            disconnect conn
        cleanup = removeFile "test2.db"

allTests = TestList [testDB, testXmlParser]

isRight (Right _) = True
isRight _ = False

main = do
     cnts <- runTestTT allTests
     if errors cnts == 0 && failures cnts == 0
        then exitSuccess
        else exitFailure




































