module Environment
    ( dijkstraBFS
    , updateCoordCost
    )
where

import qualified Data.Map                      as M
import qualified Data.List                     as L
import           Debug.Trace

type Coord = (Int, Int)


type CoordCost = M.Map Coord Int


data World = Field {
    mobs :: M.Map Int Mob
    , rocks :: M.Map Int Rock
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


action :: Mob -> World -> Mob
action mob world = filter (isEnemy mob) (search world mob 5)

search :: World -> Mob -> Int -> [Mob]
search world searcher searchLength = filter
    (\mob -> if M.lookup (coord mob) m == Nothing then False else True)
    (map snd (M.toList $ mobs world))

  where
    m = dijkstraBFS searchLength
                    (coord searcher)
                    (map (coordR . snd) (M.toList $ rocks world))




isEnemy :: Mob -> Mob -> Bool
isEnemy mob1 mob2 = kind mob1 /= kind mob2

-- isReachable :: Coord -> Coord -> [Rock] -> Bool
dijkstraBFS :: Int -> Coord -> [Coord] -> CoordCost
dijkstraBFS limit (x, y) rocks = snd amap
  where
    amap = go 1 [(x, y)] (M.singleton (x, y) 0) limit rocks
    go :: Int -> [Coord] -> CoordCost -> Int -> [Coord] -> ([Coord], CoordCost)
    go 0 _ m limit rocks = ([], m)
    go n (q : qs) m limit rocks
        | null nqueue = go (n - 1) nqueue nm limit (q : rocks)
        | otherwise   = go (length nqueue) nqueue nm limit (q : rocks)
        where (nqueue, nm) = next q qs limit rocks m

-- 探索始点 始点のコスト 既探索ノード+岩の場所 すべての探索コスト集合 -> ([探索候補キュー], [探索コスト集合])
next :: Coord -> [Coord] -> Int -> [Coord] -> CoordCost -> ([Coord], CoordCost)
next (x, y) qs limit rocks m =
    let nexts =
                (if elem (x + 1, y) rocks then [] else [(x + 1, y)])
                    ++ (if elem (x - 1, y) rocks then [] else [(x - 1, y)])
                    ++ (if elem (x, y + 1) rocks then [] else [(x, y + 1)])
                    ++ (if elem (x, y - 1) rocks then [] else [(x, y - 1)])
        cost = case M.lookup (x, y) m of
            Nothing -> limit + 1
            Just a  -> a
    in  if cost + 1 > limit
            then (qs, m)
            else (qs ++ nexts, L.foldl' (updateCoordCost (cost + 1)) m nexts)

updateCoordCost :: Int -> CoordCost -> Coord -> CoordCost
updateCoordCost cost m coord = case M.lookup coord m of
    Nothing        -> nm
    Just costExist -> if costExist > cost then nm else m
    where nm = M.insert coord cost m




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




