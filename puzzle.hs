import Control.Applicative
import Control.Monad (guard)
import Text.Printf
import Data.List (nub, nubBy)
import qualified Data.Set as Set

--- Basic types
type Point = (Int, Int, Int)
type Configuration = Set.Set Point
type PointSet      = Set.Set Point

--- n-fold composition
nfold :: (a -> a) -> Int -> (a -> a)
nfold f n = foldr (.) id (replicate n f)

--- Rotations about the axes
type Rotation = (Configuration -> Configuration)

xRotate, yRotate, zRotate :: Int -> Rotation
xRotate = nfold $ Set.map $ \(x, y, z) -> (x, -z, y)
yRotate = nfold $ Set.map $ \(x, y, z) -> (z, y, -x)
zRotate = nfold $ Set.map $ \(x, y, z) -> (-y, x, z)

allRotations :: [Rotation]
allRotations = [ x . y . z |
                 x <- xRotate <$> [0..3],
                 y <- yRotate <$> [0..3],
                 z <- zRotate <$> [0..3] ]

--- Translations parallel to the axes
type Translation = (Configuration -> Configuration)

xTranslate, yTranslate, zTranslate :: Int -> Translation
xTranslate n = Set.map $ \(x, y, z) -> (x + n, y, z)
yTranslate n = Set.map $ \(x, y, z) -> (x, y + n, z)
zTranslate n = Set.map $ \(x, y, z) -> (x, y, z + n)

allTranslations = [ x . y . z |
                    x <- xTranslate <$> [0..2],
                    y <- yTranslate <$> [0..2],
                    z <- zTranslate <$> [0..2] ]

--- The pieces
tPiece :: Configuration
tPiece = Set.fromList [ (0, 0, 0),
                        (0, 1, 0),
                        (0, 2, 0),
                        (1, 1, 0) ]

zPiece :: Configuration
zPiece = Set.fromList [ (0, 0, 0),
                        (0, 1, 0),
                        (1, 1, 0),
                        (1, 2, 0) ]

lPiece :: Configuration
lPiece = Set.fromList [ (0, 0, 0),
                        (0, 1, 0),
                        (0, 2, 0),
                        (1, 0, 0) ]

vPiece :: Configuration
vPiece = Set.fromList [ (0, 0, 0),
                        (0, 1, 0),
                        (1, 0, 0) ]

allPieces = [ tPiece,
              zPiece,
              lPiece,
              lPiece,
              lPiece,
              lPiece,
              vPiece ]

--- The box
box :: PointSet
box = Set.fromList [ (x, y, z) |
                     x <- [0..2],
                     y <- [0..2],
                     z <- [0..2] ]

pieceWithinBox :: Configuration -> Bool
pieceWithinBox = (`Set.isSubsetOf` box)

--- Enumerate all possible unique legal orientations of a piece
legalOrientations :: Configuration -> [Configuration]
legalOrientations piece = nub $ filter pieceWithinBox $
                          allOrienations piece
    where allOrienations piece =
              [ (translation . rotation) piece |
                rotation    <- allRotations,
                translation <- allTranslations ]

--- Intersection and union of points in pieces!
intersects :: PointSet -> PointSet -> Bool
intersects piece piece' =
    not $ Set.null $ Set.intersection piece piece'

union :: PointSet -> PointSet -> PointSet
union piece piece' = Set.union piece piece'

--- Predicate: is this a correct solution?
checkSolution :: [Configuration] -> Bool
checkSolution placedPieces = (foldr1 union placedPieces) == box

--- Solve the puzzle using backtracking
canAddTo :: [Configuration] -> Configuration -> Bool
canAddTo placedPieces placed' =
    not $ any (intersects placed') placedPieces

legalOrientationsAlist' = [ (piece, (legalOrientations piece)) |
                            piece <- allPieces ]

legalOrientations' piece =
    case (lookup piece legalOrientationsAlist') of
      Just orientations -> orientations
      Nothing           -> []

solvePuzzle :: [Configuration] -> [[Configuration]]
solvePuzzle pieces = solvePuzzle' pieces []
    where solvePuzzle' :: [Configuration] -> [Configuration] -> [[Configuration]]
          solvePuzzle' []      placedPieces
              | checkSolution placedPieces = return placedPieces
              | otherwise                  = []
          solvePuzzle' (u:us)  placedPieces =
              do placed' <- legalOrientations' u
                 guard $ canAddTo placedPieces placed'
                 solvePuzzle' us (placed':placedPieces)


main = do print $ (solvePuzzle allPieces) !! 0
