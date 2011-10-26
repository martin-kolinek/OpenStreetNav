module Main (
    main
) where

import SlabDecomposition
import Data.Int
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Environment
import System.Exit
import Control.Monad
import Control.Exception
import Control.Seq
import qualified Data.Array as A
import qualified Data.Map as M

type MySegment = CanonicalSegment Int64

select1 = "select n1.latitude, n1.longitude, n2.latitude, n2.longitude, e.WayID from Edges e join Nodes n1 on n1.ID = e.StartNodeID join Nodes n2 on n2.ID = e.EndNodeID join Attributes a on a.ObjectType = 1 and a.ObjectID = e.WayID where a.Key = 'landuse' and a.Value = 'residential'"
select2 = "select n.latitude, n.longitude from Edges e join Nodes n on n.ID = e.StartNodeID join Attributes a on a.ObjectID = e.WayID and a.ObjectType = 1 where a.Key = 'highway' LIMIT 1;"

main = do
    args <- getArgs
    unless (length args == 1) (putStrLn "expected argument" >> exitFailure)
    conn <- connectSqlite3 (head args)
    res <- quickQuery conn select1 []
    let slmap = (constructSlabMap (map createSegment res) 0) :: FinSlabMap MySegment
    let (b1, b2) = A.bounds slmap
    putStrLn $ "Map size " ++ (show (b2-b1+1))
    res2 <- quickQuery' conn select2 []
    let intr = filter (interesting slmap) (map createPoint res2)
    putStr "Success "
    print $ length intr
    disconnect conn

createSegment (v1:v2:v3:v4:v5:[]) = CanonSegment (Point (fromSql v1) (fromSql v2))
                                                (Point (fromSql v3) (fromSql v4))
                                                (fromSql v5)

createPoint (v1:v2:[]) = Point (fromSql v1) (fromSql v2)


interesting slmap point = same $ getUpDownSegments point slmap
    where same (Just (CanonSegment _ _ x), Just (CanonSegment _ _ y)) = x == y
          same _ = False




