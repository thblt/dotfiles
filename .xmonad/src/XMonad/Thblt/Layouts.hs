{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module XMonad.Thblt.Layouts (layoutHook, MyTransformers (..), myActiveColor, myInactiveColor) where

import Data.Typeable

import XMonad hiding (layoutHook)
import           XMonad.Hooks.ManageDocks            (ToggleStruts (ToggleStruts),
                                                      avoidStruts,
                                                      docksEventHook,
                                                      manageDocks)
import XMonad.Layout.BinarySpacePartition  (ResizeDirectional (..),
                                             Rotate (Rotate),
                                             SelectMoveNode (..),
                                             TreeBalance (..),
                                             emptyBSP)
import XMonad.Layout.BorderResize (borderResize)
import XMonad.Layout.Gaps
import XMonad.Layout.IfMax
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders (withBorder)
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Spacing (smartSpacingWithEdge)

import XMonad.Thblt.Misc

data MySpacing = MySpacing {
  myGaps       :: Int,
  myBorderSize :: Int,
  myDecoHeight :: Int
  }

data MyTransformers = GAPS  deriving (Read, Show, Eq, Typeable)
instance Transformer MyTransformers Window where
  transform GAPS x k = k (smartSpacingWithEdge (negate $ myGaps mySpacing) $ x) (const x)

-- The layouts I use
myBSP = emptyBSP
myTall = Mirror $ Tall 1 (3/100) (3/4)

layoutHook = avoidStruts $ mkToggle (FULL ?? GAPS ?? EOT) $
             ifMax 1 Full $
             borderResize
             . myDecoration
             . smartSpacingWithEdge (myGaps mySpacing)
             . withBorder (fromIntegral $ myBorderSize mySpacing) $
             myBSP ||| myTall

myDecoration = id
-- myDecoration = noFrillsDeco shrinkText def {
--   decoHeight = (fromIntegral $ myDecoHeight mySpacing)
--   , activeColor = myActiveColor
--   , activeTextColor = "#ffffff"
--   , activeBorderColor = "#000000"
--   , inactiveColor = myInactiveColor
--   , inactiveTextColor = myInactiveColor
--   , inactiveBorderColor = myInactiveColor
--   , fontName = "DejaVuSansMono-24"
--   }

-- mySpacing :: MySpacing
-- mySpacing | hostname == "rudiger" = MySpacing 2 2 0
--           | otherwise = MySpacing 2 2 0

mySpacing :: MySpacing
mySpacing = case hostname of
  "maladict" -> MySpacing 8 2 16
  _ -> MySpacing 4 0 0

myActiveColor = "#0059DD"
myInactiveColor = "#000000"
