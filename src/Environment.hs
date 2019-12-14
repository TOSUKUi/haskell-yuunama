module Environment
    ()
where

import qualified Data.Map                      as M


type Coord = (Int, Int)


data World = Field {
    mobs :: M.Map Coord Mob
    , rocks :: M.Map Coord Rock
    , field :: Coord
}


data Mob = Mob {
    kind :: String
    , name :: String
    , hp :: Int
    , mp :: Int
    , offense :: Int
    , defense :: Int
    , attackRatio :: Int
}

data Rock = Rock {
    nutrition :: Int
    , magic :: Int
}

attack :: Mob -> Mob -> Mob
attack a b = if kind a == kind b
    then b
    else b
        { hp = ( (hp b)
               - (offense a)
               + (ceiling . logBase 10 $ fromIntegral (defense b))
               )
        }

search :: World -> (Coord, Mob) -> [Mob]
search world (coord, mob) = (coord + 5)


update :: World -> World
update world = M.toList



