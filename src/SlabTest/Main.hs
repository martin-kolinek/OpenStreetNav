module Main (
    main
) where

import SlabDecomposition
import Data.Int
import Database.SQLite3
import SqlRepresentation.Common
import System.Environment
import System.Exit
import Control.Monad
import Control.Exception
import Control.Seq
import qualified Data.Array as A
import qualified Data.Map as M

type MySegment = CanonicalSegment Int64

select1 = "select n1.latitude, n1.longitude, n2.latitude, n2.longitude, e.WayID from Edges e join Nodes n1 on n1.ID = e.StartNodeID join Nodes n2 on n2.ID = e.EndNodeID join Attributes a on a.ObjectType = 1 and a.ObjectID = e.WayID where a.Key = 'landuse' and a.Value = 'residential'"
select2 = "select n.latitude, n.longitude from Edges e join Nodes n on n.ID = e.StartNodeID join Attributes a on a.ObjectID = e.WayID and a.ObjectType = 1 where a.Key = 'highway' LIMIT 30000;"

main = do
    args <- getArgs
    unless (length args == 1) (putStrLn "expected argument" >> exitFailure)
    conn <- open (head args)
    st <- prepare conn select1
    res <- query st []
    let slmap = (constructSlabMap (map createSegment res) 0) :: FinSlabMap MySegment
    let (b1, b2) = A.bounds slmap
    putStrLn $ "Map size " ++ (show (b2-b1+1))
    finalize st
    st <- prepare conn select2
    res2 <- query st []
    let intr = filter (interesting slmap) (map createPoint res2)
    putStr "aSuccess "
    print $ length intr
    finalize st
    close conn

createSegment ((SQLFloat v1):(SQLFloat v2):(SQLFloat v3):(SQLFloat v4):(SQLInteger v5):_) =
                                    CanonSegment (Point v1 v2)
                                                (Point v3 v4)
                                                v5

createPoint ((SQLFloat v1):(SQLFloat v2):_) = Point v1 v2


interesting slmap point = same $ getUpDownSegments point slmap
    where same (Just (CanonSegment _ _ x), Just (CanonSegment _ _ y)) = x == y
          same _ = False




