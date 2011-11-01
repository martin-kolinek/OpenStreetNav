{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import SqlRepresentation.BasicInsertion
import SqlRepresentation.TableDefinition
import SqlRepresentation.Common
import Database.HDBC.Sqlite3 as HS
import Database.HDBC as H
import Database.SQLite3 hiding (finalize)
import System.Exit
import System.IO
import System.Console.ParseArgs
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Text.XML.Expat.SAX
import OsmXmlParser
import Control.Seq
import Control.Exception
import Control.DeepSeq

progressStep = 10000

validArgs =
    [
    Arg {argIndex = "osm",
        argAbbr = Just 'i',
        argName = Just "input_osm",
        argData = argDataRequired "osm filename" ArgtypeString,
        argDesc = "Input osm xml file"},
    Arg {argIndex = "sqlite",
        argAbbr = Just 'o',
        argName = Just "output_sqlite",
        argData = argDataRequired "sqlite filename" ArgtypeString,
        argDesc = "Output sqlite file"}
    ]


main = do
    parsedArgs <- parseArgsIO ArgsComplete validArgs
    let maybeConf = getConf parsedArgs
    if isJust maybeConf
        then (uncurry importData) (fromJust maybeConf)
        else (putStrLn $ argsUsage parsedArgs) >> exitFailure


getConf parsedArgs = do
    osm <- getArg parsedArgs "osm"
    sqlite <- getArg parsedArgs "sqlite"
    return (osm, sqlite)

importData :: String -> String -> IO ()
importData osm sqlite = do
    conn <- HS.connectSqlite3 sqlite
    checkDB conn
    H.disconnect conn
    conn2 <- open sqlite
    text <- L.readFile osm
    nodeSt <- prepareInsertNode conn2
    waySt <- prepareInsertWay conn2
    relSt <- prepareInsertRelation conn2
    beginTran conn2
    let p = parse defaultParseOptions text :: [SAXEvent B.ByteString B.ByteString]
    success <- parseOsmSAX
                    (addInsertNode nodeSt)
                    (addInsertWay waySt)
                    (addInsertRel relSt)
                    errorHandler
                    progressHandler
                    (return True)
                    p
    if success
        then do
            commitTran conn2
            putStrLn "Creating indexes..."
            createIndexes conn2
            putStrLn "Success"
        else do
            rollbackTran conn2
            putStrLn "Aborted"

addInsertNode nodeSt act node = act >> execInsertNode nodeSt node >> return True
addInsertWay waySt act way = act >> execInsertWay waySt way >> return True
addInsertRel relSt act rel = act >> execInsertRelation relSt rel >> return True
errorHandler str = putStrLn str >> return False
progressHandler act int = do
    val <- act
    when (int `mod` progressStep == 0) $ putStrLn ("Processed " ++ show int ++ " xml pieces")
    return val


checkDB :: IConnection a => a -> IO ()
checkDB conn = do
    tables <- getTables conn
    if tables == []
        then createTables conn >> H.commit conn
        else do
            result1 <- checkAllTablesPresent conn
            result2 <- trySelecting conn
            if result1  && result2
                then return ()
                else do
                    hPutStrLn stderr "SqLite file already contains different schema"
                    exitFailure


dbExists conn = return ()
