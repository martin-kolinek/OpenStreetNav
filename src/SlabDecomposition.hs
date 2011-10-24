module SlabDecomposition (
    SlabMap,
    Segment(..),
    Point(..),
    getUpDownSegments,
    constructSlabMap
) where

import qualified Data.Map as M
import Data.List
import Control.Monad

data Slab s = Slab Double (M.Map (Double, Double) s)

type SlabMap s = M.Map Double (Slab s)

class Segment a where
    left :: a -> Double
    right :: a -> Double
    points :: a -> (Point, Point)

data Point = Point Double Double deriving (Eq, Ord, Show)

addPoint :: Segment s => Double -> SlabMap s -> SlabMap s
addPoint new map = if M.member new map
    then map
    else addPoint' map new

addPoint' :: Segment s => SlabMap s -> Double -> SlabMap s
addPoint' map new = let (_, slab) = findClosestLeft new map
                        newSlab = extendSlab slab new
    in M.insert new newSlab map

addSegment :: Segment s => s -> SlabMap s -> SlabMap s
addSegment seg map = foldl' (modifyMap seg) map (subkeys map (left seg) (right seg))
    where
        modifyMap :: Segment s => s -> SlabMap s -> Double  -> SlabMap s
        modifyMap seg m key = M.adjust (splitSlab seg) key m

findClosestLeft :: Ord k => k -> M.Map k v -> (k, v)
findClosestLeft value = M.findMax . fst . M.split value
findClosestRight value = M.findMin . snd . M.split value
findClosest value map = let l@(left, _) = findClosestLeft value map
                            r@(right, _) = findClosestRight value map
                        in
                            if abs (left - value) > abs (right - value) then r else l

subkeys map min max = [min] ++ skeys ++ [max]
    where
        uppermap = (snd . M.split min) map
        boundedmap = (fst . M.split max) map
        skeys = M.keys boundedmap

extendSlab :: Segment s => Slab s -> Double -> Slab s
extendSlab (Slab origLeft map) newLeft = Slab newLeft $ foldl' addLine M.empty (M.toAscList map)
    where
        addLine m (y, seg)
            | right seg == origLeft = m
            | otherwise = M.insert (getYOnLine seg newLeft, getYOnLine seg newLeft + 1) seg m

include :: (a, [b]) -> [(a, b)]
include (x, ys) = map (\y -> (x,y)) ys

adjustWithDefault fun key def map = case M.lookup key map of
    Nothing -> (upd . (M.insert key def)) map
    Just _ -> upd map
    where upd = M.adjust fun key

getYOnLine seg = (uncurry getYOnLine') (points seg)

getYOnLine' (Point x1 y1) (Point x2 y2) x
    | x1 == x2 = if x==x1 then x else error "getYOnLine x not on line"
    | otherwise = let k = (y1-y2)/(x1-x2)
                      q = y1 - k*x1
                  in k*x + q

splitSlab :: Segment s => s -> Slab s -> Slab s
splitSlab seg (Slab x map) = Slab x $ M.insert (getYOnLine seg x, getYOnLine seg x+1) seg map

emptySlabMap minx = M.singleton minx (Slab minx M.empty)

constructSlabMap :: Segment s => [s] -> Double -> SlabMap s
constructSlabMap segments min = foldl' addSegmentProperly (emptySlabMap min) segments
    where
        addSegmentProperly :: Segment s => SlabMap s -> s -> SlabMap s
        addSegmentProperly m s
            | left s == right s = m
            | otherwise = (addSegment s) . (addPoint (left s)) . (addPoint (right s)) $ m

getUpDownSegments :: Segment s => Point -> SlabMap s -> (Maybe s, Maybe s)
getUpDownSegments p@(Point x y) map = let (_, slab) = findClosestLeft x map
                                    in
                                        extractFirst $ findInSlab p slab

findInSlab :: Segment s => Point -> Slab s -> (Maybe (s, Double), Maybe (s, Double))
findInSlab (Point x y) (Slab slabx map) = let (_, segs1) = findClosestLeft (y, 0) map
                                              (_, segs2) = findClosestRight (y, 0) map
                                              segs = segs1 : [segs2]
                                              assignY seg = (seg, getYOnLine seg x)
                                              segsWithYs = fmap assignY segs
                                              sortFunc (_, y1) (_, y2) = compare y1 y2
                                              sortedSegs = sortBy sortFunc segsWithYs
                                          in getTreshold ((<y) . snd) sortedSegs

extractFirst :: (Maybe (a, b), Maybe (a, b)) -> (Maybe a, Maybe a)
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

