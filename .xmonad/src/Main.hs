{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main (main) where

import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Layout.Fullscreen
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (docksEventHook, manageDocks)
import XMonad.Hooks.ManageHelpers (isDialog)
import XMonad.Hooks.Place (placeHook, smart, withGaps)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Util.NamedScratchpad (namedScratchpadManageHook)
import XMonad.Thblt.Misc
import qualified XMonad.Thblt.Keys as XTK (keys)
import qualified XMonad.Thblt.Layouts as XTL (layoutHook, myActiveColor, myInactiveColor)

main :: IO ()
main = do
--dbus <- connectSession
--getWellKnownName dbus

  launch . fullscreenSupport . withNavigation2DConfig def {
    defaultTiledNavigation = centerNavigation -- default lineNavigation is broken with BSP + smartSpacing
  } $ ewmh def {
      borderWidth = 0 -- Borders are added in the layout hook
    , focusedBorderColor = XTL.myActiveColor
    , normalBorderColor = XTL.myInactiveColor
    , clickJustFocuses = False
    , focusFollowsMouse = False
    --    , handleEventHook = fullscreenEventHook <+> docksEventHook
    , handleEventHook = docksEventHook
    , keys = XTK.keys workspacesKeys myScratchpads
    , mouseBindings = myMouseBindings
    , layoutHook = XTL.layoutHook
    , logHook = do
--        dbusLogger dbus
--        dynamicLogWithPP$ myPP dbus -- logPipe
        fadeInactiveLogHook 0.95
    , manageHook = composeAll
      [
        namedScratchpadManageHook myScratchpads
      , placeHook $ withGaps (16,0,16,0) (smart (0.5,0.5))
      , manageDocks
      , isDialog --> doFloat
      , title =? "Invoking Emacs daemon…" --> doFloat
      , className =? "Gloobus-preview"    --> doFloat
      , className =? "Pinentry"           --> doFloat
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
