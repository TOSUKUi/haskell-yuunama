module Environment
    ( dijkstra
    , dijkstraBFS
    )
where

import qualified Data.Map                      as M
import qualified Data.List                     as L
import           Debug.Trace

type Coord = (Int, Int)


type CoordCost = M.Map Coord Int


data World = Field {
    mobs :: M.Map Int [Mob]
    , rocks :: M.Map Int [Rock]
    , field :: Coord
}

data Mob = Mob {
    ident :: Int
    , kind :: String
    , coord :: Coord
    , name :: String
    , hp :: Int
    , mp :: Int
    , offense :: Int
    , defense :: Int
    , attackRatio :: Int
}

data Rock = Rock {
    coordR :: Coord
    , nutrition :: Int
    , magic :: Int
}


-- action :: Mob -> World -> Mob
-- action mob world = filter (isEnemy mob) (search world mob)


isEnemy :: Mob -> Mob -> Bool
isEnemy mob1 mob2 = kind mob1 /= kind mob2

-- isReachable :: Coord -> Coord -> [Rock] -> Bool

dijkstra :: Int -> Coord -> [Coord] -> [Coord] -> Int -> [(Coord, Int)]
dijkstra limit (-1, -1) candidates rocks distance = []
dijkstra limit (x, y) candidates rocks distance
    | elem (x, y) rocks
    = []
    | distance > limit
    = []
    | otherwise
    = let
          vright = if elem (x + 1, y) candidates then (x + 1, y) else (-1, -1)
          vleft = if elem (x + 1, y) candidates then (x + 1, y) else (-1, -1)
          vabove = if elem (x, y + 1) candidates then (x, y + 1) else (-1, -1)
          vbelow = if elem (x, y - 1) candidates then (x, y - 1) else (-1, -1)
          nextCandidates = L.delete (x, y) candidates
      in
          [((x, y), distance)]
          ++ (dijkstra limit vright nextCandidates rocks (distance + 1))
          ++ (dijkstra limit vleft nextCandidates rocks (distance + 1))
          ++ (dijkstra limit vabove nextCandidates rocks (distance + 1))
          ++ (dijkstra limit vbelow nextCandidates rocks (distance + 1))


dijkstraBFS :: Int -> Coord -> [Coord] -> Int -> CoordCost
dijkstraBFS limit (x, y) rocks maxDistance = snd amap
  where
    amap = go 1 [(x, y)] M.empty rocks
    go :: Int -> [Coord] -> CoordCost -> [Coord] -> ([Coord], CoordCost)
    go 0 _ m rocks = ([], m)
    go n (q : qs) m rocks
        | null nqueue
        = go (n - 1) qs nm (q : rocks)
        | otherwise
        = let ngo = go (n - 1 + length nqueue) qs nm (q : rocks)
          in  (nqueue ++ (fst ngo), snd ngo)
        where (nqueue, nm) = next q qs rocks m






-- 探索始点 始点のコスト 既探索ノード+岩の場所 すべての探索コスト集合 -> ([探索候補キュー], [探索コスト集合])
next :: Coord -> [Coord] -> [Coord] -> CoordCost -> ([Coord], CoordCost)
next (x, y) qs rocks m =
    let nexts =
                (if elem (x + 1, y) rocks then [] else [(x + 1, y)])
                    ++ (if elem (x + 1, y) rocks then [] else [(x + 1, y)])
                    ++ (if elem (x, y + 1) rocks then [] else [(x, y + 1)])
                    ++ (if elem (x, y - 1) rocks then [] else [(x, y - 1)])
        Just cost = M.lookup (x, y) m
    in  traceShow nexts
            $ (nexts ++ qs, L.foldl' (updateCoordCost (cost + 1)) m nexts)


updateCoordCost :: Int -> CoordCost -> Coord -> CoordCost
updateCoordCost cost m coord = M.update
    (\costExist -> if costExist > cost then Just cost else Just costExist)
    coord
    m




distance :: Coord -> Coord -> Int
distance (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))


attack :: Mob -> Mob -> Mob
attack a b = if kind a == kind b
    then b
    else b
        { hp = ( (hp b)
               - (offense a)
               + (ceiling . logBase 10 $ fromIntegral (defense b))
               )
        }

-- search :: World -> Mob -> [Mob]



-- update :: World -> World
-- update world = map M.toList (mobs world)




