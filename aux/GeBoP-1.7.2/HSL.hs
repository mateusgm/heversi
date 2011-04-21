
---------
-- HSL -- 
---------

module HSL 
  ( hsl
  , colorHSL, colorHue, colorSat, colorLum
  , setHue, setSat, setLum
  ) where

import Graphics.UI.WX

hsl      :: Float -> Float -> Float -> Color
colorHSL :: Color -> (Float, Float, Float)
colorHue :: Color -> Float
colorSat :: Color -> Float
colorLum :: Color -> Float
setHue   :: Float -> Color -> Color
setSat   :: Float -> Color -> Color
setLum   :: Float -> Color -> Color

hsl h s l
  | h < 0 || h > 1 || s < 0 || s > 1 || l < 0 || l > 1 = error "hsl: out of range"
  | s == 0 = let v :: Int
                 v = round $ 255 * l
             in rgb v v v
  | otherwise = let var_2 | l < 0.5   = l * (1 + s)
                          | otherwise = (l + s) - (s * l)
                    var_1 = 2 * l - var_2
                    r :: Int
                    r = round $ 255 * hue2rgb var_1 var_2 (h + 1/3)
                    g :: Int
                    g = round $ 255 * hue2rgb var_1 var_2  h
                    b :: Int
                    b = round $ 255 * hue2rgb var_1 var_2 (h - 1/3)
                in rgb r g b

hue2rgb :: Float -> Float -> Float -> Float
hue2rgb v1 v2 vh = let vh_ | vh < 0    = vh + 1
                           | vh > 1    = vh - 1
                           | otherwise = vh
                       v | 6 * vh_ < 1 = v1 + (v2 - v1) * 6 * vh_
                         | 2 * vh_ < 1 = v2
                         | 3 * vh_ < 2 = v1 + (v2 - v1) * (2/3 - vh_) * 6
                         | otherwise   = v1
                   in v

colorHSL c =
  let var_r :: Float
      var_r = (fromInteger $ colorRed   c) / 255
      var_g :: Float
      var_g = (fromInteger $ colorGreen c) / 255
      var_b :: Float
      var_b = (fromInteger $ colorBlue  c) / 255
      var_min = minimum [var_r, var_g, var_b]
      var_max = maximum [var_r, var_g, var_b]
      del_max = var_max - var_min
      l = (var_max + var_min) / 2
      s | del_max == 0 = 0
        | l < 0.5      = del_max / (    var_max + var_min)
        | otherwise    = del_max / (2 - var_max - var_min)
      del_r = ((var_max - var_r) / 6 + del_max / 2) / del_max
      del_g = ((var_max - var_g) / 6 + del_max / 2) / del_max
      del_b = ((var_max - var_b) / 6 + del_max / 2) / del_max
      h_ | del_max == 0     = 0
         | var_r == var_max = del_b - del_g
         | var_g == var_max = 1/3 + del_r - del_b
         | var_b == var_max = 2/3 + del_g - del_r
         | otherwise        = error "Unexpected value"
{-
      h | h_ < 0 = h_ + 1
        | h_ > 1 = h_ - 1
        | otherwise        = error "Unexpected value"
-}
  in (h_, s, l)

colorHue = (\(h, _, _) -> h) . colorHSL

colorSat = (\(_, s, _) -> s) . colorHSL

colorLum = (\(_, _, l) -> l) . colorHSL

setHue h c 
  | h < 0 || h > 1 = error "setHue: out of range"
  | otherwise      = let (_, s, l) = colorHSL c in hsl h s l

setSat s c 
  | s < 0 || s > 1 = error "setSat: out of range"
  | otherwise      = let (h, _, l) = colorHSL c in hsl h s l

setLum l c 
  | l < 0 || l > 1 = error "setLum: out of range"
  | otherwise      = let (h, s, _) = colorHSL c in hsl h s l
