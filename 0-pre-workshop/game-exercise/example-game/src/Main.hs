module Main where

import           Graphics.Gloss                       hiding (Circle)
import           Graphics.Gloss.Interface.IO.Interact hiding (Circle)

main :: IO ()
main =
  play
    (InWindow "yin-yang" (300, 300) (100, 100))
    black
    20
    initial
    view
    input
    step

data World
  = Go [Shape] Int
  | Pause Int

data Shape
  = Circle
  | Square

initial :: World
initial = Go [Circle] 0

input :: Event -> World -> World
input (EventKey (Char 'p') _ _ _) (Go _ n)                 = Pause n -- Pause logic
input (EventKey (MouseButton LeftButton) Up _ _) (Go _ n)  = Pause n -- Pause logic with mouse click
input (EventKey (MouseButton LeftButton) Up _ _) (Pause n) = Go [Circle] n -- Resume logic with mouse click
input (EventKey (Char 's') _ _ _) (Go _ n)                 = Go [Square] n
input (EventKey (Char 'c') _ _ _) (Go _ n)                 = Go [Circle] n
input (EventKey (Char 'r') _ _ _) (Pause n)                = Go [Circle] n
input _ w                                                  = w -- Otherwise, keep the same

num :: (Integral a, Num b) => a -> b
num x = fromIntegral $ (x + 1) `mod` 255

step :: Float -> World -> World
step _ (Go _ n)  = Go (replicate (num n) Circle) ((n + 1) `mod` 255)
step _ (Pause n) = Pause n

view :: World -> Picture
view w =
  case w of
    (Go xs n) -> pictures $ map (r n) xs
    (Pause _) ->
      translate (-120) 0 $
      scale 0.2 0.2 $ color white $ text "Press r to resume"
  where
    c x = makeColorI 150 x x 255
    r x Square = color (c x) (rectangleSolid (num x) (num x))
    r x Circle = color (c x) (thickCircle (num x) (num x))
