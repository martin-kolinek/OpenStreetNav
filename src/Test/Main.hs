-----------------------------------------------------------------------------
--
-- Module      :  Test
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

module Main (
    main
) where

import Test.HUnit
import System.Exit
import OsmXmlParser
import qualified Text.XML.Expat.Tree as Xml
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified OsmData as D

testNode = (fst . Xml.parse Xml.defaultParseOptions $ BS.pack
    "<node id=\"23\" lat=\"1.5\" lon=\"2.5\"><tag k=\"key\" v=\"val\" /></node>" :: (Xml.Node String String))

testWay = (fst . Xml.parse Xml.defaultParseOptions $ BS.pack
    "<way id=\"22\"><nd ref=\"11\"/><tag k=\"key\" v=\"val\" /></way>" :: (Xml.Node String String))

testRel = (fst . Xml.parse Xml.defaultParseOptions $ BS.pack (
    "<relation id=\"43\">"++
        "<member type=\"node\" ref=\"10\" role=\"role\"/>"++
        "<member type=\"way\" ref=\"11\" role=\"role\"/>"++
        "<member type=\"relation\" ref=\"12\" role=\"role\"/>"++
        "<tag k=\"key\" v=\"val\" />"++
    "</relation>") :: (Xml.Node String String))

testNode' = D.Node {D.nodeID = 23, D.latitude = 1.5, D.longitude = 2.5, D.nodeTags = [("key", "val")]}
testWay' = D.Way {D.wayID = 22, D.nodes = [11], D.wayTags = [("key", "val")]}
testRel' = D.Relation {D.relID = 43, D.members = [(D.NodeType, 10, "role"), (D.WayType, 11, "role"), (D.RelationType, 12, "role")],
                D.relTags = [("key", "val")]}

testXmlParser = TestList [testParseWay]

testParseWay = TestCase $ do
    let tway = parseWay testWay
    case tway of
        (Left err) -> assertFailure err
        (Right way) -> assertEqual "Way equality" way testWay'

isRight (Right _) = True
isRight _ = False

main = do
     cnts <- runTestTT testXmlParser
     if errors cnts == 0 && failures cnts == 0
        then exitSuccess
        else exitFailure




































