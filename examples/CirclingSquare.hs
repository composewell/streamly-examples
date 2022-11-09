-- Adapted from the Yampa package.
-- Displays a square moving in a circle. To move the position drag it with the
-- mouse.

import Control.Concurrent (forkIO)
import Data.Function ((&))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import SDL (($=), V2(..), V4(..), Point(..))

import qualified Data.Text as Text
import qualified SDL
import qualified Streamly.Prelude as Stream

------------------------------------------------------------------------------
-- SDL Graphics Init
------------------------------------------------------------------------------

sdlInit :: IO (SDL.Window, SDL.Renderer)
sdlInit = do
    SDL.initializeAll
    window <- SDL.createWindow (Text.pack "Circling Square") SDL.defaultWindow
    rend <- SDL.createRenderer window (-1) SDL.defaultRenderer
    return (window, rend)

------------------------------------------------------------------------------
-- Wait and update Controller Position if it changes
------------------------------------------------------------------------------

handleEvents :: IORef (Double, Double) -> IO Bool
handleEvents ref = do
    e <- SDL.waitEvent
    case SDL.eventPayload e of
        SDL.MouseMotionEvent (SDL.MouseMotionEventData _ _ _ (P (V2 x y)) _) -> do
            writeIORef ref (fromIntegral x, fromIntegral y)
            return True
        SDL.WindowClosedEvent _ ->
            return False
        _ -> return True

------------------------------------------------------------------------------
-- Display a rectangle at given coordinates
------------------------------------------------------------------------------

display :: (Double, Double) -> SDL.Renderer -> IO ()
display (playerX, playerY) rend = do
  -- Paint the screen with background color
  SDL.rendererDrawColor rend $= V4 55 60 64 255
  SDL.fillRect rend Nothing

  -- Paint a small square at the current position
  SDL.rendererDrawColor rend $= V4 212 108 73 255
  let diag = 15
      x = round playerX
      y = round playerY
  SDL.fillRect rend (Just (SDL.Rectangle (P (V2 x y)) diag))

------------------------------------------------------------------------------
-- Periodically refresh the output display
------------------------------------------------------------------------------

updateDisplay :: IORef (Double, Double) -> SDL.Renderer -> IO ()
updateDisplay ref rend = do
    time <- SDL.ticks
    (x, y) <- readIORef ref
    let t = fromIntegral time * speed / 1000
     in display (x + cos t * radius, y + sin t * radius) rend
    SDL.present rend

    where

    speed  = 6
    radius = 60

main :: IO ()
main = do
    (window, rend) <- sdlInit
    ref <- newIORef (0,0)

    _ <- forkIO $ do
            Stream.repeatM (updateDisplay ref rend) -- Stream IO ()
                & Stream.delay (1/60)               -- Stream IO ()
                & Stream.drain                      -- IO ()

    -- MacOS requires pollEvents to run in the Main thread.
    Stream.repeatM (handleEvents ref) & Stream.drainWhile (== True)
    SDL.destroyWindow window
