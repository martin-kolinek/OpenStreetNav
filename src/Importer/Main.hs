-----------------------------------------------------------------------------
--
-- Module      :  Main
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
    let (tree, err) = parse defaultParseOptions text :: (UNode String, Maybe XMLParseError)
    --catchSql (importOsm tree)
    case err of
        Nothing -> do
            commit conn
            hPutStrLn stderr "Success"
        Just e -> do
            rollback conn
            hPutStrLn stderr $ "Error parsing osm: " ++ show e
    disconnect conn

importOsm tree = return ()

checkDB conn = do
    tables <- getTables conn
    if tables == []
        then createTables conn
        else do
            result1 <- checkAllTablesPresent conn
            result2 <- checkAllTableDescriptions conn
            if result1 && result2
                then return ()
                else do
                    hPutStrLn stderr "SqLite file already contains different schema"
                    exitFailure


dbExists conn = return ()
