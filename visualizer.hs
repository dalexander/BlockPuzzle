
import Graphics.UI.GLUT
import Data.IORef    ( IORef, newIORef )
import System.Exit   ( exitWith, ExitCode(ExitSuccess) )
import Control.Monad ( zipWithM_ )
import Colors

type Point = (Int, Int, Int)
type Piece = [Point]

data State = State { cameraAltitude, cameraAzimuth :: IORef GLdouble,
                     cameraDistance                :: IORef GLdouble,
                     displayedPieceIndices         :: IORef [Int]     }

solution :: [Piece]
solution = [[(1,0,0),(1,0,1),(2,0,1)],
            [(1,2,0),(2,2,0),(2,2,1),(2,2,2)],
            [(2,0,0),(2,1,0),(2,1,1),(2,1,2)],
            [(0,0,2),(0,1,2),(0,2,1),(0,2,2)],
            [(1,0,2),(1,1,2),(1,2,2),(2,0,2)],
            [(0,0,1),(0,1,1),(1,1,1),(1,2,1)],
            [(0,0,0),(0,1,0),(0,2,0),(1,1,0)]]

makeState :: IO State
makeState = do
  alt  <- newIORef (-30)
  az   <- newIORef 20
  dist <- newIORef 10
  idx  <- newIORef [1..7]
  return $ State { cameraAltitude = alt,
                   cameraAzimuth  = az,
                   cameraDistance = dist,
                   displayedPieceIndices = idx }

myInit :: IO ()
myInit = do
   ambient (Light 0) $= Color4 0 0 0 1
   clearColor $= Color4 0 0 0 0
   hint LineSmooth $= DontCare
   lineWidth $= 1.5
   depthFunc $= Just Less

drawCube :: Color3 GLfloat -> Point -> IO ()
drawCube color' (x, y, z) =
    let x' = (fromIntegral x) + 0.5::GLfloat
        y' = (fromIntegral y) + 0.5::GLfloat
        z' = (fromIntegral z) + 0.5::GLfloat
    in preservingMatrix $ do
      translate $ Vector3 x' y' z'
      color color'
      renderObject Solid (Cube 1)
      color $ Color4 (0::GLfloat) 0 0 0
      renderObject Wireframe (Cube 1)

drawPiece :: Color3 GLfloat -> Piece -> IO ()
drawPiece color' piece = zipWithM_ drawCube (repeat color') piece

orientCamera :: GLdouble -> GLdouble -> GLdouble -> IO ()
orientCamera altitude azimuth distance = do
  translate $ Vector3 0 0 (-distance)
  rotate (-altitude) (Vector3 1 0 0)
  rotate azimuth (Vector3 0 1 0)
  translate $ Vector3 (-1.5::GLdouble) (-1.5) (-1.5) -- center box

reshape :: ReshapeCallback
reshape size@(Size w h) = do
  viewport $= (Position 0 0, size)
  matrixMode $= Projection
  loadIdentity
  perspective 60 (fromIntegral w / fromIntegral h) 1 100

display :: State -> DisplayCallback
display state = do
  alt <- get (cameraAltitude state)
  az  <- get (cameraAzimuth  state)
  d   <- get (cameraDistance state)
  clear [ ColorBuffer, DepthBuffer ]
  preservingMatrix $ do
    orientCamera alt az d
    zipWithM_ drawPiece allColors solution
  flush

keyboard :: State -> KeyboardMouseCallback
keyboard state (Char c) Down _ _ =
    case c of
      '='   -> do cameraDistance state $~ (/1.1)
                  postRedisplay Nothing
      '-'   -> do cameraDistance state $~ (*1.1)
                  postRedisplay Nothing
      '\27' -> exitWith ExitSuccess
      _     -> return ()
keyboard state (SpecialKey key) Down _ _ =
  case key of
    KeyUp    -> do cameraAltitude state $~ (+10)
                   postRedisplay Nothing
    KeyDown  -> do cameraAltitude state $~ (+(-10))
                   postRedisplay Nothing
    KeyLeft  -> do cameraAzimuth state $~ (+(-10))
                   postRedisplay Nothing
    KeyRight -> do cameraAzimuth state $~ (+10)
                   postRedisplay Nothing
    _        -> return ()

keyboard _ _ _ _ _ = return ()


main :: IO ()
main = do
  (progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
  initialWindowSize $= Size 500 500
  initialWindowPosition $= Position 100 100
  createWindow progName
  state <- makeState
  myInit
  displayCallback $= display state
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboard state)
  mainLoop
