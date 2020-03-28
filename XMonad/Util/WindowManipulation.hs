module XMonad.Util.WindowManipulation (ScreenSide(..), moveFloatWin) where

import Control.Monad (unless)
import Data.Int
import Data.Maybe (fromJust, isNothing)
import Graphics.X11.Xlib
import XMonad.Actions.FloatKeys
import XMonad.Core
import XMonad.Operations
import qualified XMonad.StackSet as W

-- Screen movement aliases
data ScreenSide = STop | SBot | SRight | SLeft

-- move Y disregarding X or move X disregarding Y
moveFloatWin :: ScreenSide -> X ()
moveFloatWin STop = moveFloatWinY 0 0
moveFloatWin SBot = do
  h <- screenDim rect_height
  moveFloatWinY h 1
moveFloatWin SLeft = moveFloatWinX 0 0
moveFloatWin SRight = do
  w <- screenDim rect_width
  moveFloatWinX w 1

-- return a function of the screen dimension
screenDim f = withWindowSet $ \w ->
  return (fromIntegral . f . screenRect  . W.screenDetail $ W.current w)

-- move float win disregarding X
moveFloatWinY y gy = withWindowSet $ \ws -> do
  let w = W.peek ws
  unless (isNothing w) $ do
    r <- getWinDims $ fromJust w
    keysMoveWindowTo (screenOff ws + rectXOff r ws, y) (0, gy) $ fromJust w

-- move float win disregarding Y
moveFloatWinX x gx = withWindowSet $ \ws -> do
  let w = W.peek ws
  unless (isNothing w) $ do
    r <- getWinDims $ fromJust w
    keysMoveWindowTo (screenOff ws + x, rectYOff r ws) (gx, 0) $ fromJust w

getWinDims w = floatLocation w >>= (\(_, r) -> return r)

-- get X offset for screen
screenOff ws =
  let
    sid     = W.screen $ W.current ws
    toInt n = fromIntegral n :: Int32
    screenW = fromIntegral (rect_width $ screenRect sd) :: Int32
      where sd = W.screenDetail $ W.current ws
  in toInt sid * screenW

-- get X offset for floating rectantle
rectXOff (W.RationalRect x y w h) ws =
  let
    screenW = fromIntegral (rect_width $ screenRect sd) :: Rational
      where sd = W.screenDetail $ W.current ws
  in floor (x * screenW) :: Int32

-- get Y offset for floating rectantle
rectYOff (W.RationalRect x y w h) ws =
  let
    screenH = fromIntegral (rect_height $ screenRect sd) :: Rational
      where sd = W.screenDetail $ W.current ws
  in floor (y * screenH) :: Int32
