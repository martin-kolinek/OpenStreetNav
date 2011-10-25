module SlabDecomposition (
    SlabMap,
    Segment(..),
    Point(..),
    CanonicalSegment(..),
    getUpDownSegments,
    constructSlabMap,
    findInSlab
) where

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Monad
import Debug.Trace

data Slab s = Slab Double (M.Map (Double, Double) s) deriving Show

type SlabMap s = M.Map Double (Slab s)

class Segment a where
    left :: a -> Double
    left s = let (Point x _, _) = points s in x
    right :: a -> Double
    right s = let (_, Point x _) = points s in x
    points :: a -> (Point, Point)

data CanonicalSegment a = CanonSegment Point Point a deriving (Show, Eq)

instance Segment (CanonicalSegment a) where
    points (CanonSegment p1 p2 _) = (min p1 p2, max p1 p2)

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

subkeys map min max = [min] ++ skeys ++ [max]
    where
        uppermap = (snd . M.split min) map
        boundedmap = (fst . M.split max) uppermap
        skeys = M.keys boundedmap

extendSlab :: Segment s => Slab s -> Double -> Slab s
extendSlab (Slab origLeft map) newLeft = Slab newLeft $ foldl' addLine M.empty (M.toAscList map)
    where
        addLine m (y, seg)
            | right seg == origLeft = m
            | otherwise = M.insert (getYOnLine seg newLeft, getYOnLine seg (newLeft + 1)) seg m

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
splitSlab seg (Slab x map) = Slab x $ M.insert (getYOnLine seg x, getYOnLine seg (x+1)) seg map

emptySlabMap minx = M.singleton minx (Slab minx M.empty)

constructSlabMap :: Segment s => [s] -> Double -> SlabMap s
constructSlabMap segments min = foldl' addSegmentProperly (emptySlabMap min) segments
    where
        addSegmentProperly :: Segment s => SlabMap s -> s -> SlabMap s
        addSegmentProperly m s
            | left s == right s = m
            | otherwise = (addSegment s) .(addPoint (left s)) . (addPoint (right s)) $ m

getUpDownSegments :: (Eq s) => Segment s => Point -> SlabMap s -> (Maybe s, Maybe s)
getUpDownSegments p@(Point x y) map = let (_, slab) = findClosestLeft x map
                                    in
                                        findInSlab p slab

abovePoint (Point x y) segm = (getYOnLine segm x) > y

findInSlab :: (Eq s) => Segment s => Point -> Slab s -> (Maybe s, Maybe s)
findInSlab p@(Point x y) (Slab _ map)
    | M.null map = (Nothing, Nothing)
    | otherwise = let (lower, _) = M.split (y, 0) map
                      kv = if M.null lower then M.elemAt 0 map else M.findMax lower
                      (ml, mh) = findTreshold (abovePoint p) kv map
                  in (fmap snd ml, fmap snd mh)


findTreshold :: (Ord k, Eq v) => (v -> Bool) -> (k, v) -> M.Map k v -> (Maybe (k, v), Maybe (k, v))
findTreshold pred start map = findTreshold' (Nothing, Nothing) pred start map

findTreshold' :: (Ord k, Eq v) => (Maybe (k,v), Maybe (k,v)) -> (v -> Bool) -> (k,v) -> M.Map k v -> (Maybe (k,v), Maybe (k,v))
findTreshold' found@(foundf, foundt) pred start@(startk, startv) map
    | pred startv = if foundt == Just start
            then found
            else
                let (lower, _) = M.split startk map
                in if M.null lower then (Nothing, Just start)
                        else findTreshold' (foundf, Just start) pred (M.findMax lower) map
    | otherwise = if foundf == Just start
            then found
            else
                let (_, higher) = M.split startk map
                in if M.null higher then (Just start, Nothing)
                        else findTreshold' (Just start, foundt) pred (M.findMin higher) map


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

