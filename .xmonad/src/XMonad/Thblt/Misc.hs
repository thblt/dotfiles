{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module XMonad.Thblt.Misc where


import           Data.Int
import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Language.Haskell.TH
import           System.Posix.Unistd                 (SystemID (nodeName),
                                                      getSystemID)
import           XMonad
--import         XMonad.Actions.CycleWS              (nextWS, prevWS)
-- import           XMonad.Actions.MessageFeedback -- READ BELOW.
-- Note to self: this is broken.  The messages get correctly sent, but the view doesn't update.  You have to move focus or do something else.
import           XMonad.Hooks.EwmhDesktops (ewmh)
import           XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import           XMonad.Hooks.ManageHelpers (isDialog)
import           XMonad.Hooks.Place (placeHook, withGaps, smart)
import           XMonad.Hooks.SetWMName (setWMName)

import           XMonad.Layout.Fullscreen (fullscreenSupport)
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoFrillsDecoration
import qualified XMonad.StackSet as XSS

import           XMonad.Util.NamedScratchpad
import           XMonad.Util.NamedWindows (getName)
import           XMonad.Util.Run (spawnPipe)
import           XMonad.Util.WorkspaceCompare (getSortByIndex)

-- Computer-dependent settings.

[d| hostname = $(stringE =<< runIO (fmap nodeName getSystemID) ) |]

workspacesKeys :: [KeySym]
workspacesKeys | hostname == "anna"    = macAzertyKeys
               | hostname == "rudiger" = pcAzertyBeKeys
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
