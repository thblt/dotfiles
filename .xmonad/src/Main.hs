{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main where

import           Data.Int
import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Language.Haskell.TH
import           System.Posix.Unistd                 (SystemID (nodeName),
                                                      getSystemID)
import           XMonad
--import         XMonad.Actions.CycleWS              (nextWS, prevWS)
import           XMonad.Actions.Navigation2D
-- import           XMonad.Actions.MessageFeedback -- READ BELOW.
-- Note to self: this is broken.  The messages get correctly sent, but the view doesn't update.  You have to move focus or do something else.
import           XMonad.Actions.WindowBringer (bringMenu, gotoMenu)
import           XMonad.Hooks.EwmhDesktops (ewmh)
import           XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import           XMonad.Hooks.ManageDocks            (ToggleStruts (ToggleStruts),
                                                      avoidStruts,
                                                      docksEventHook,
                                                      manageDocks)
import           XMonad.Hooks.ManageHelpers (isDialog)
import           XMonad.Hooks.Place (placeHook, withGaps, smart)
import           XMonad.Hooks.SetWMName (setWMName)
import           XMonad.Layout.BinarySpacePartition  (ResizeDirectional (..),
                                                      Rotate (Rotate),
                                                      SelectMoveNode (..),
                                                      TreeBalance (..),
                                                      emptyBSP)
import           XMonad.Layout.BorderResize (borderResize)
import           XMonad.Layout.Fullscreen (fullscreenSupport)
import           XMonad.Layout.Gaps
import           XMonad.Layout.IfMax
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders (withBorder)
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.Spacing (smartSpacingWithEdge)
import qualified XMonad.StackSet as XSS

import           XMonad.Util.NamedScratchpad
import           XMonad.Util.NamedWindows (getName)
import           XMonad.Util.Run (spawnPipe)
import           XMonad.Util.WorkspaceCompare (getSortByIndex)

import qualified XMonad.Thblt.Keys as MyKeys

-- Computer-dependent settings.

[d| myHostName = $(stringE =<< runIO (fmap nodeName getSystemID) ) |]

workspacesKeys :: [KeySym]
workspacesKeys | myHostName == "anna"    = macAzertyKeys
               | myHostName == "rudiger" = pcAzertyBeKeys
               | otherwise               = pcAzertyKeys
  where
    pcAzertyKeys = [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0] -- From AzertyConfig
    pcAzertyBeKeys = [0x26,0xe9,0x22,0x27,0x28,0xa7,0xe8,0x21,0xe7,0xe0] -- From AzertyConfig
    macAzertyKeys = [0x26,0xe9,0x22,0x27,0x28,0xa7,0xe8,0x21,0xe7,0xe0] -- From AzertyConfig

-- XMonad.

myWorkspaces :: [ String ]
myWorkspaces = map show [ 1 .. 9 :: Int ]

myHiddenWorkspaces :: [ String ]
myHiddenWorkspaces = [ "NSP" ]

myActiveColor = "#007bFF"
myInactiveColor = "#888888"

data MySpacing = MySpacing {
  myGaps       :: Int,
  myBorderSize :: Int,
  myDecoHeight :: Int
  }

-- mySpacing :: MySpacing
-- mySpacing | myHostName == "rudiger" = MySpacing 2 2 0
--           | otherwise = MySpacing 2 2 0

mySpacing = MySpacing 2 2 0

data GapsTransformer = GAPS deriving (Read, Show, Eq, Typeable)
instance Transformer GapsTransformer Window where
  transform _ x k = k (gaps[(U, 8), (D, 7), (R, 300), (L, 300)] x) (const x)

myLayoutHook = avoidStruts $ mkToggle (FULL ?? GAPS ?? EOT) $
               ifMax 1 Full $
               borderResize
               . myDecoration
               . smartSpacingWithEdge (myGaps mySpacing)
               . withBorder (fromIntegral $ myBorderSize mySpacing) $
               (emptyBSP ||| (Mirror $ Tall 1 (3/100) (3/4)))
  where
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

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
    [
      ((modMask, 1), \w -> focus w >> mouseMoveWindow w
                                 >> windows XSS.shiftMaster)
--    , ((0, 10), const $ prevWS) -- Three-finger left swipe
--    , ((0, 11), const $ nextWS) -- Three finger right swipe
    ]


myScratchpads :: [NamedScratchpad]
myScratchpads = [
  NS "term" "urxvt -title urxvt_scratchpad_1545645 -e ~/.xmonad/scripts/tmux-attach-or-new scratch" (title =? "urxvt_scratchpad_1545645")
  (customFloating rect),

  NS "web" "surf" (className =? "Surf")
    (customFloating rect)
  ]
  where ratio = 12
        rect = XSS.RationalRect (1/ratio) (1/ratio) ((ratio-2)/ratio) ((ratio-2)/ratio)


{- And now to wrap it all up -}

main :: IO ()
main = do
--dbus <- connectSession
--getWellKnownName dbus

  launch . fullscreenSupport . withNavigation2DConfig def {
    defaultTiledNavigation = centerNavigation -- default lineNavigation is broken with BSP + smartSpacing
  } $ ewmh def {
      borderWidth = 0 -- Borders are added in the layout hook
    , focusedBorderColor = myActiveColor
    , normalBorderColor = myInactiveColor
    , clickJustFocuses = False
    , focusFollowsMouse = False
    --    , handleEventHook = fullscreenEventHook <+> docksEventHook
    , handleEventHook = docksEventHook
    , keys = MyKeys.keys workspacesKeys myScratchpads
    , mouseBindings = myMouseBindings
    , layoutHook = myLayoutHook
    , logHook = do
--        dbusLogger dbus
--        dynamicLogWithPP$ myPP dbus -- logPipe
        fadeInactiveLogHook 0.98
    , manageHook = composeAll
      [
        namedScratchpadManageHook myScratchpads
      , placeHook $ withGaps (16,0,16,0) (smart (0.5,0.5))
      , manageDocks
      , isDialog --> doFloat
      , className =? "Gloobus-preview"    --> doFloat
      , title =? "Invoking Emacs daemon…" --> doFloat
      , className =? "Xdialog"            --> doFloat
      , className =? "Yad"                --> doFloat
      , className =? "zenity"             --> doFloat
      , className =? "zbar"               --> doFloat
        ]
    , modMask = mod4Mask -- ``Windows'' key.
    , startupHook = setWMName "LG3D"
    , terminal = "urxvt"
    , workspaces = myWorkspaces
    }
