module Lib
    ( someFunc
    , Space (..)
    , Piece (..)
    , SpaceSize
    , empty
    , make
    , sparse
    , canonic
    , piece
    , size
    , a
    , shadow
    , rotx
    , roty
    , rotz
    , rotations
    , move
    , moves
    , flail
    ) where

import Data.List
import Data.Maybe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type SpaceSize = [Int]
data  Space = Space' SpaceSize [[[String]]] deriving (Eq, Ord)

instance Show Space where
  show = showSpace ""

showSpace :: String -> Space -> String
showSpace p (Space' [_,_,0] [ys]) = "\n" ++ concat [concat [p, "|", concat [padtwo x ++ " "| x <- xs], "|\n"] | xs <- ys]
showSpace p (Space' [x,y,z] (ys:zs)) = concat [showSpace p (Space' [x,y,0] [ys]),
    p ++ " " ++ concat (replicate (y+1) "--"),
    showSpace (p ++ " ") (Space' [x,y,z-1] zs)]

padtwo :: String -> String
padtwo [x] = [x] ++ " "
padtwo x = x

empty :: SpaceSize -> Space
empty [x,y] = Space' [x-1,y-1,0] [replicate y b]
  where b = replicate x " "
empty [x,y,z] = Space' [x-1,y-1,z-1] $ replicate z p
  where Space' _ [p] = empty [x, y]

data Piece = Piece' String [(Int,Int,Int)] deriving (Eq, Ord)

instance Show Piece where
  show = show . make
 
a = make (Piece' "A"  [(0,0,0),(0,1,0),(1,0,0),(1,1,0)])

size :: Piece -> SpaceSize
size (Piece' _ xyzs) = [foldl max 0 (map (st n) xyzs) | n <- [0..2]]

st :: Int -> (Int, Int, Int) -> Int
st 0 (x, _, _) = x
st 1 (_, y, _) = y
st 2 (_, _, z) = z

canonic :: Piece -> Piece
canonic (Piece' n xyzs) = Piece' n (sort xyzs)

piece :: String -> [(Int, Int, Int)] -> Piece
piece n xyzs = canonic $ Piece' n xyzs

make :: Piece -> Space
make p = spacemap d c
  where d = size p
        (Piece' n xyzs) = p
        c = \e -> if e `elem` xyzs then n else " "

spacemap :: SpaceSize -> ((Int,Int,Int)->String) -> Space
spacemap d c = Space' d [[[c (x,y,z) | x <- [0..xs]] | y <- [0..ys]] | z <- [0..zs]]
  where [xs,ys,zs] = d

sparse :: Space -> Piece
sparse (Space' [xs,ys,zs] s) = piece n xyzs
  where n = head . (filter (/= " ")) . nub $ concat $ concat s
        xyzs = [(i,j,k) | (z,k) <- zip s [0..zs]
                        , (y,j) <- zip z [0..ys]
                        , (x,i) <- zip y [0..xs]
                        , x == n]


shadow :: Piece -> Piece
shadow (Piece' n xyzs) = piece (n++"'") [(x,y,z-1)
                                        | (x,y,z) <- xyzs,
                                          z > 0, 
                                          not ((x,y,z-1) `elem` xyzs)]

rotx :: Piece -> Piece
rotx p = piece n [(x,z,ys-y) | (x,y,z) <- xyzs]
  where [_, ys, _] = size p
        Piece' n xyzs = p

roty :: Piece -> Piece
roty p = piece n [(zs-z,y,x) | (x,y,z) <- xyzs]
  where [_, _, zs] = size p
        Piece' n xyzs = p

rotz :: Piece -> Piece
rotz p = piece n [(ys-y,x,z) | (x,y,z) <- xyzs]
  where [_, ys, _] = size p
        Piece' n xyzs = p


rotations :: Piece -> [Piece]
rotations p = sort . nub $ concat [ take 4 $ iterate rotx b | b <- concat [take 4 $ iterate roty a | a <- take 4 $ iterate rotz p]]


move :: (Int,Int,Int) -> Piece -> Piece
move (dx,dy,dz) (Piece' n xyzs) = piece n [(x+dx,y+dy,z+dz) | (x,y,z) <- xyzs]

moves :: SpaceSize -> Piece -> [Piece]
moves [sx,sy,sz] p = [move (dx,dy,dz) p
                     | dx <- [0..sx-px],
                       dy <- [0..sy-py],
                       dz <- [0..sz-pz],
                       px <= sx,
                       py <= sy,
                       pz <= sz]
  where
    [px, py, pz] = size p

flail :: SpaceSize -> Piece -> [Piece]
flail s p = concatMap (moves s) (rotations p)

data Puzzle = Puzzle' { space :: SpaceSize
                      , pieces :: [Piece]
                      } deriving (Show, Eq, Ord)

puzzle :: SpaceSize -> [Piece] -> Puzzle
puzzle = Puzzle'

data Solution = Solution' { solutionsize :: SpaceSize
                          , statepiece :: Piece
                          , unused :: [Piece]
                          , solved :: [Piece]
                          } deriving (Eq, Ord)

instance Show Solution where
  show s = progress ++ layout
    where progress = "\n" ++ names solved ++ " ... " ++ names unused
          names f = (intercalate " " ) (map (\(Piece' n _) -> n) (f s))
          layout = show $ spacemap (solutionsize s) c
          c = (fromMaybe " ") . (`lookup` piecenames)
          piecenames = [(xyz,n) | (Piece' n xyzs) <- solved s , xyz <- xyzs]

solution :: SpaceSize -> [Piece] -> [Piece] -> Solution
solution s = Solution' s (piece "=" [])

volume :: SpaceSize -> Int
volume = foldl (*) 1 . map (+1)


solve :: Puzzle -> [Solution]
solve x | volume(s) /= sum (map (volume . size) $ pieces x) = []
        | otherwise = solve' $ solution s ps []
  where Puzzle' s ps = x

solve' :: Solution -> [Solution]
solve' x | us == [] = [x]
         | otherwise = concatMap solve' [ Solution' ss (mergepiece sp p) (filter (/= u) us) (p:sps)
                                        | u <- us,
                                          p <- flail ss u,
                                          valid x p ]
  where Solution' ss sp us sps = x

valid :: Solution -> Piece -> Bool
valid (Solution' _ sp _ _) p = (ss `intersect` sps == ss) && (ps `intersect` sps == [])
  where (Piece' _ ss) = shadow p
        (Piece' _ sps) = sp
        (Piece' _ ps) = p

mergepiece :: Piece -> Piece -> Piece
mergepiece (Piece' n sts) (Piece' _ ps) = piece n (sts ++ ps)

