-- Adapted from the Yampa package.
-- Displays a square moving in a circle. To move the position drag it with the
-- mouse.

import Data.Function ((&))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Graphics.UI.SDL as SDL
import Streamly.Prelude as Stream

------------------------------------------------------------------------------
-- SDL Graphics Init
------------------------------------------------------------------------------

sdlInit :: IO ()
sdlInit = do
  SDL.init [InitVideo]

  let width  = 640
      height = 480
  _ <- SDL.setVideoMode width height 16 [SWSurface]
  SDL.setCaption "Test" ""

------------------------------------------------------------------------------
-- Display a box at a given coordinates
------------------------------------------------------------------------------

display :: (Double, Double) -> IO ()
display (playerX, playerY) = do
  screen <- getVideoSurface

  -- Paint screen green
  let format = surfaceGetPixelFormat screen
  bgColor <- mapRGB format 55 60 64
  _ <- fillRect screen Nothing bgColor

  -- Paint small red square, at an angle 'angle' with respect to the center
  foreC <- mapRGB format 212 108 73
  let side = 20
      x = round playerX
      y = round playerY
  _ <- fillRect screen (Just (Rect x y side side)) foreC

  -- Double buffering
  SDL.flip screen

------------------------------------------------------------------------------
-- Wait and update Controller Position if it changes
------------------------------------------------------------------------------

updateController :: IORef (Double, Double) -> IO ()
updateController ref = do
    e <- pollEvent
    case e of
        MouseMotion x y _ _ -> writeIORef ref (fromIntegral x, fromIntegral y)
        _ -> return ()

------------------------------------------------------------------------------
-- Periodically refresh the output display
------------------------------------------------------------------------------

updateDisplay :: IORef (Double, Double) -> IO ()
updateDisplay ref = do
    time <- SDL.getTicks
    (x, y) <- readIORef ref
    let t = fromIntegral time * speed / 1000
     in display (x + cos t * radius, y + sin t * radius)

    where

    speed  = 6
    radius = 60

main :: IO ()
main = do
    sdlInit
    ref <- newIORef (0,0)
    let mouseStream = Stream.repeatM (updateController ref)
        displayStream = Stream.repeatM (updateDisplay ref)

    mouseStream `Stream.parallel` displayStream -- AsyncT IO ()
        & Stream.constRate 40                   -- AsyncT IO ()
        & Stream.fromAsync                      -- SerialT IO ()
        & Stream.drain                          -- IO ()
