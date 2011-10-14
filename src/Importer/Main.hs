module Main (
    main
) where

import SqlRepresentation
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Exit
import System.IO
import System.Console.ParseArgs
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Lazy as L
import Text.XML.Expat.Tree
import OsmXmlParser
import Control.Seq
import Control.Exception
import Control.DeepSeq

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
    let p = parse defaultParseOptions text :: (UNode String, Maybe XMLParseError)
    result <- catchSql (importOsm p conn) (handleErr)
    case result of
        Nothing -> do
            commit conn
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
    xmlRes <- foldM (foldFunc nodeSt waySt relSt) (Right ()) (eChildren tree)
    case xmlRes of
        Right _ -> return Nothing
        Left msg -> return $ Just msg
    where foldFunc nodeSt waySt relSt lastResult n = do
                                let n' = withStrategy rdeepseq n
                                evaluate n'
                                newResult <- (importNode nodeSt waySt relSt n')
                                let r' = withStrategy rdeepseq (lastResult >> newResult)
                                evaluate r'
                                return r'


importNode nodeSt waySt relSt n@(Element name _ _) = do
    case name of
        "node" -> eitherIO (execInsertNode nodeSt) (parseNode n)
        "way" -> eitherIO (execInsertWay waySt) (parseWay n)
        "relation" -> eitherIO (execInsertRelation relSt) (parseRelation n)
        _ -> return (Right ())
    where
        eitherIO :: (a -> IO b) -> (Either String a) -> IO (Either String b)
        eitherIO f (Left s) = return (Left s)
        eitherIO f (Right x) = f x >>= (return . Right)
importNode nodeSt waySt relSt _ = return (Right ())


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
