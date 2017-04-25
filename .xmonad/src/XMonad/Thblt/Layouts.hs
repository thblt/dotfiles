{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module XMonad.Thblt.Layouts (layoutHook, GapsTransformer (GAPS)) where

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
import XMonad.Layout.Spacing (smartSpacingWithEdge)

data MySpacing = MySpacing {
  myGaps       :: Int,
  myBorderSize :: Int,
  myDecoHeight :: Int
  }

data GapsTransformer = GAPS deriving (Read, Show, Eq, Typeable)
instance Transformer GapsTransformer Window where
  transform _ x k = k (gaps[(U, 8), (D, 7), (R, 300), (L, 300)] x) (const x)

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
    --   , activeTextColor = myActiveColor
    --   , activeBorderColor = myActiveColor
    --   , inactiveColor = myInactiveColor
    --   , inactiveTextColor = myInactiveColor
    --   , inactiveBorderColor = myInactiveColor
    --   , fontName = "-*-clean-medium-r-*-*-12-*-*-*-*-*-*-*"
    --   }

-- mySpacing :: MySpacing
-- mySpacing | myHostName == "rudiger" = MySpacing 2 2 0
--           | otherwise = MySpacing 2 2 0

mySpacing = MySpacing 4 1 0
