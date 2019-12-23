module Ui
    ( showMap
    )
where

import qualified Data.Map                      as M
import           Data.Maybe

type Coord = (Int, Int)
type CoordCost = M.Map Coord Int

showMap :: Coord -> [Coord] -> CoordCost -> IO ()
showMap (xMax, yMax) rocks coordCost = do
    let output =
            [ [ out
              | y <- [1 .. yMax]
              , let out = if elem (x, y) rocks
                        then " ■ "
                        else convertToSymbol (M.lookup (x, y) coordCost)
              ]
            | x <- [1 .. xMax]
            ]
    putStr
        (foldl (\next row -> (foldl (++) " " row) ++ "\n" ++ next) " " output)

convertToSymbol :: Maybe Int -> String
convertToSymbol cost = fromMaybe " * " (fmap (\x -> " " ++ show x ++ " ") cost)





