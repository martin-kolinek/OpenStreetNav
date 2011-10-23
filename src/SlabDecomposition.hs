module SlabDecomposition (
) where

import qualified Data.Map as M
import Data.List
import Control.Monad

data Slab = Slab Double (M.Map Double [Segment])

type SlabMap = M.Map Double Slab

data Segment = Segment Point Point

data Point = Point Double Double

addPoint :: Double -> SlabMap -> SlabMap
addPoint new map = if M.member new map
    then map
    else addPoint' map new

addPoint' map new = let (_, slab) = findClosestLeft new map
                        newSlab = extendSlab slab new
    in M.insert new newSlab map

addSegment :: Segment -> SlabMap -> SlabMap
addSegment seg map = foldl' modifyMap map (subkeys map (left seg) (right seg))
    where
        modifyMap :: SlabMap -> Double  -> SlabMap
        modifyMap m key = M.adjust (splitSlab seg) key m

findClosestLeft value = M.findMax . fst . M.split value
findClosestRight value = M.findMin . snd . M.split value
findClosest value map = let l@(left, _) = findClosestLeft value map
                            r@(right, _) = findClosestRight value map
                        in
                            if abs (left - value) > abs (right - value) then r else l

left :: Segment -> Double
left (Segment (Point x1 y1) (Point x2 y2)) = min x1 x2

right :: Segment -> Double
right (Segment (Point x1 y1) (Point x2 y2)) = max x1 x2

subkeys map min max = [min] ++ skeys ++ [max]
    where
        uppermap = (snd . M.split min) map
        boundedmap = (fst . M.split max) map
        skeys = M.keys boundedmap

extendSlab (Slab origLeft map) newLeft = Slab newLeft $ foldl' addLine M.empty (extractSegs map)
    where
        extractSegs m = let assoc = M.toAscList m
                        in join $ fmap include assoc
        addLine m (y, seg)
            | right seg == origLeft = m
            | otherwise = adjustWithDefault (seg:) (getYOnLine seg newLeft) [] m

include :: (a, [b]) -> [(a, b)]
include (x, ys) = map (\y -> (x,y)) ys

adjustWithDefault fun key def map = case M.lookup key map of
    Nothing -> (upd . (M.insert key def)) map
    Just _ -> upd map
    where upd = M.adjust fun key


getYOnLine (Segment (Point x1 y1) (Point x2 y2)) x
    | x1 == x2 = if x==x1 then x else error "getYOnLine x not on line"
    | otherwise = let k = (y1-y2)/(x1-x2)
                      q = y1 - k*x1
                  in k*x + q

splitSlab :: Segment -> Slab -> Slab
splitSlab seg (Slab x map) = Slab x $ adjustWithDefault (seg:) (getYOnLine seg x) [] map

emptySlabMap minx = M.singleton minx (Slab minx M.empty)

constructSlabMap segments min = foldl' addSegmentProperly (emptySlabMap min) segments
    where
        addSegmentProperly :: SlabMap -> Segment -> SlabMap
        addSegmentProperly m s = (addSegment s) . (addPoint (left s)) . (addPoint (right s)) $ m

getUpDownSegments :: Point -> SlabMap -> (Maybe Segment, Maybe Segment)
getUpDownSegments p@(Point x y) map = let (_, slab) = findClosestLeft x map
                                    in
                                        extractFirst $ findInSlab p slab

findInSlab :: Point -> Slab -> (Maybe (Segment, Double), Maybe (Segment, Double))
findInSlab (Point x y) (Slab slabx map) = let (_, segs1) = findClosestLeft y map
                                              (_, segs2) = findClosestRight y map
                                              segs = segs1 ++ segs2
                                              assignY seg = (seg, getYOnLine seg x)
                                              segsWithYs = fmap assignY segs
                                              sortFunc (_, y1) (_, y2) = compare y1 y2
                                              sortedSegs = sortBy sortFunc segsWithYs
                                          in getTreshold ((<y) . snd) sortedSegs

extractFirst :: (Maybe (Segment, Double), Maybe (Segment, Double)) -> (Maybe Segment, Maybe Segment)
extractFirst (mx, my) = (mx >>= f, my >>= f)
    where f (x, y) = return x

getTreshold _ [] = (Nothing, Nothing)
getTreshold pred [x]
    | pred x = (Just x, Nothing)
    | otherwise = (Nothing, Nothing)
getTreshold pred (x:(rest@(y:xs)))
    | pred x && not (pred y) = (Just x, Just y)
    | pred x && pred y = getTreshold pred rest
    | not (pred x) = (Nothing, Nothing)

