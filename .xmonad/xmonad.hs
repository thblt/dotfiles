{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

import           Data.Int
import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Graphics.X11.ExtraTypes.XF86
import           Language.Haskell.TH
import           System.Posix.Unistd (SystemID (nodeName),
                                       getSystemID)
import           XMonad
--import         XMonad.Actions.CycleWS              (nextWS, prevWS)
import           XMonad.Actions.Navigation2D
-- import           XMonad.Actions.MessageFeedback -- READ BELOW.
-- Note to self: this is broken.  The messages get correctly sent, but the view doesn't update.  You have to move focus or do something else.
import           XMonad.Util.EZConfig (mkKeymap)
import           XMonad.Actions.WindowBringer (bringMenu, gotoMenu)
import           XMonad.Hooks.EwmhDesktops (ewmh)
import           XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import           XMonad.Hooks.ManageDocks (ToggleStruts (ToggleStruts),
                                            avoidStruts,
                                            docksEventHook,
                                            manageDocks)
import           XMonad.Hooks.ManageHelpers (isDialog)
import           XMonad.Hooks.Place (placeHook, withGaps, smart)
import           XMonad.Hooks.SetWMName (setWMName)
import           XMonad.Layout.BinarySpacePartition (ResizeDirectional (..),
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

myScratchpads :: [NamedScratchpad]
myScratchpads = [
  NS "term" "urxvt -title urxvt_scratchpad_1545645 -e ~/.xmonad/tmux-attach-or-new scratch" (title =? "urxvt_scratchpad_1545645")
  (customFloating rect),

  NS "web" "surf" (className =? "Surf")
    (customFloating rect)
  ]
  where ratio = 16
        rect = XSS.RationalRect (1/ratio) (1/ratio) ((ratio-2)/ratio) ((ratio-2)/ratio)

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig { XMonad.modMask = modMask } =
  mkKeymap conf $
  [
    ("M-S-c"                     , kill),
    ("M-S-q"                     , spawn "/home/thblt/.xmonad/quit-xmonad.sh"),
    ("M-q"                       , restart "/home/thblt/.local/bin/xmonad" True), --spawn "/home/thblt/.xmonad/recompile-xmonad.sh"),
    ("M-<Esc>"                   , spawn "dm-tool lock"),
    ("M-S-<Esc>"                 , spawn "dm-tool switch-to-greeter"),

    ("M-<Space>"                 , sendMessage NextLayout),
    ("M-S-<Space>"               , setLayout $ XMonad.layoutHook conf), -- (Reset)

    ("M-h"                       , sendMessage Shrink),
    ("M-l"                       , sendMessage Expand),

    ("M-f f"                     , sendMessage $ Toggle FULL),
    ("M-f x"                     , sendMessage $ Toggle GAPS),
    ("M-f s"                     , sendMessage ToggleStruts),

    ("M-="                       , sendMessage $ IncMasterN 1),
    ("M--"                       , sendMessage $ IncMasterN (-1)),
    -- BSP-specific
    ("M-S-h"                     , sendMessage $ ExpandTowards L),
    ("M-S-j"                     , sendMessage $ ExpandTowards D),
    ("M-S-k"                     , sendMessage $ ExpandTowards U),
    ("M-S-l"                     , sendMessage $ ExpandTowards R),

    ("M-C-h"                     , sendMessage $ ShrinkFrom L),
    ("M-C-j"                     , sendMessage $ ShrinkFrom D),
    ("M-C-k"                     , sendMessage $ ShrinkFrom U),
    ("M-C-l"                     , sendMessage $ ShrinkFrom R),

    ("M-w"                       , sendMessage SelectNode),
    ("M-x"                       , sendMessage MoveNode),

    ("M-r"                       , sendMessage Rotate),

    ("M-<Tab>"                   , windows XSS.focusDown),
    ("M-S-<Tab>"                 , windows XSS.focusUp),

    ("M-<Return>"                , windows XSS.swapMaster),
    ("M-t"                       ,                 withFocused $ windows . XSS.sink),

    ("M-p"                       , spawn "~/.bin/dmenu-desktop --entry-type=name"),

    ("M-S-<Return>"              , spawn $ terminal conf),

    ("M-C-S-<Return>"            , spawn $ "~/.xmonad/emacsclient-with-feedback"),

    ("M-s"                       , namedScratchpadAction myScratchpads "term"),

    -- Volume
    ("<XF86AudioLowerVolume"     , spawn $ "amixer -c 0 set Master unmute ; amixer -c 0 set Master 2-; " ++ shNotifyVolume),
    ("<XF86AudioRaiseVolume"     , spawn $ "amixer -c 0 set Master unmute ; amixer -c 0 set Master 2+; " ++ shNotifyVolume),
    ("<XF86AudioMute>"           , spawn $ "amixer set Master toggle; " ++ shNotifyVolume),

    -- Brightness (monitor)
    ("<XF86MonBrightnessUp>"     , spawn "sudo anybrightness /sys/devices/pci0000:00/0000:00:02.0/backlight/acpi_video0 +1"),
    ("S-<XF86MonBrightnessUp>"   , spawn "sudo anybrightness /sys/devices/pci0000:00/0000:00:02.0/drm/card0/card0-eDP-1/intel_backlight/ +1"),
    ("<XF86MonBrightnessDown>"   , spawn "sudo anybrightness /sys/devices/pci0000:00/0000:00:02.0/backlight/acpi_video0 -1"),
    ("S-<XF86MonBrightnessDown>" , spawn "sudo anybrightness /sys/devices/pci0000:00/0000:00:02.0/drm/card0/card0-eDP-1/intel_backlight/ -1"),

    -- Brightness (keyboard backlight)
    ("<XF86KdbBrightnessUp>"     , spawn "sudo anybrightness /sys/devices/platform/applesmc.768/leds/smc::kbd_backlight +20%"),
    ("<XF86KdbBrightnessDown>"   ,spawn "sudo anybrightness /sys/devices/platform/applesmc.768/leds/smc::kbd_backlight -20%")
  ]
  where
    shNotifyVolume = "notify-send Volume `amixer get Master | tail -n 1  | awk '{print $6}'` -t 250 -h string:fgcolor:#ffffff -h string:bgcolor:#000000 -h int:value:`amixer get Master | tail -n 1 | awk '{print $4}' | sed 's/[^0-9]//g'`"



  -- workspace switching
extraKeys conf modMask = [((m .|. modMask, k), windows $ f i)
     | (i, k) <- zip (XMonad.workspaces conf) workspacesKeys
     , (f, m) <- [(XSS.greedyView, 0), (XSS.shift, shiftMask)]]
  ++
  -- mod-{a,z,e} %! Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{a,z,e} %! Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_a, xK_z, xK_e] [0..]
        , (f, m) <- [(XSS.view, 0), (XSS.shift, shiftMask)]]

-- @TODO REstore

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
    [
      ((modMask, 1), \w -> focus w >> mouseMoveWindow w
                                 >> windows XSS.shiftMaster)
--    , ((0, 10), const $ prevWS) -- Three-finger left swipe
--    , ((0, 11), const $ nextWS) -- Three finger right swipe
    ]

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
    , keys = myKeys
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
