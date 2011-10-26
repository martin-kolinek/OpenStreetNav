{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import SqlRepresentation.BasicInsertion
import SqlRepresentation.TableDefinition
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Exit
import System.IO
import System.Console.ParseArgs
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Text.XML.Expat.Tree
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
    conn <- connectSqlite3 sqlite
    checkDB conn
    text <- L.readFile osm
    let p = parse defaultParseOptions text :: (UNode B.ByteString, Maybe XMLParseError)
    result <- catchSql (importOsm p conn) (handleErr)
    case result of
        Nothing -> do
            commit conn
            putStrLn "Creating indexes..."
            createIndexes conn
            putStrLn "Success"
        Just msg -> do
            rollback conn
            hPutStrLn stderr msg
    disconnect conn
    where handleErr err = return $ Just $ "Database error: " ++ show err

importOsm (tree, err) conn = do
    nodeSt <- prepareInsertNode conn
    waySt <- prepareInsertWay conn
    relSt <- prepareInsertRelation conn
    xmlRes <- foldM (foldFunc nodeSt waySt relSt) (Right 0) (eChildren tree)
    case xmlRes of
        Right _ -> return Nothing
        Left msg -> return $ Just msg
    where
        foldFunc nodeSt waySt relSt lastResult n =  do
            case lastResult of
                failure@(Left _) -> return failure
                Right int -> do
                    newResult <- (importNode nodeSt waySt relSt n int)
                    let r' = withStrategy rdeepseq newResult
                    evaluate r'
                    showProgress int
                    return r'
        showProgress int = when (int `mod` progressStep == 0) (putStrLn (msg int))
        msg int = "Processed " ++ show int ++ "xml pieces"

importNode :: InsertNodeStatement ->
                InsertWayStatement ->
                InsertRelationStatement ->
                UNode B.ByteString ->
                Int ->
                IO (Either String Int)
importNode nodeSt waySt relSt n@(Element name _ _) int = do
    case name of
        "node" -> eitherIO (execInsertNode nodeSt) (parseNode n)
        "way" -> eitherIO (execInsertWay waySt) (parseWay n)
        "relation" -> eitherIO (execInsertRelation relSt) (parseRelation n)
        _ -> return success
    where
        eitherIO :: (a -> IO b) -> (Either String a) -> IO (Either String Int)
        eitherIO f (Left s) = return (Left s)
        eitherIO f (Right x) = f x >> return success
        success = Right $ int + 1
importNode nodeSt waySt relSt _ int = return (Right (int+1))


checkDB conn = do
    tables <- getTables conn
    if tables == []
        then createTables conn
        else do
            result1 <- checkAllTablesPresent conn
            result2 <- trySelecting conn
            if result1  && result2
                then return ()
                else do
                    hPutStrLn stderr "SqLite file already contains different schema"
                    exitFailure


dbExists conn = return ()
