module XMonad.Thblt.Keys (XMonad.Thblt.Keys.keys) where

import qualified Data.Map as M
import           Graphics.X11.ExtraTypes.XF86
import           XMonad
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.WindowBringer (bringMenu, gotoMenu)
import           XMonad.Hooks.ManageDocks            (ToggleStruts (ToggleStruts),
                                                      avoidStruts,
                                                      docksEventHook,
                                                      manageDocks)
import           XMonad.Layout.BinarySpacePartition  (ResizeDirectional (..),
                                                      Rotate (Rotate),
                                                      SelectMoveNode (..),
                                                      TreeBalance (..),
                                                      emptyBSP)
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import qualified XMonad.StackSet as XSS
import           XMonad.Util.EZConfig (mkKeymap)
import           XMonad.Util.NamedScratchpad (NamedScratchpad, namedScratchpadAction)

import XMonad.Thblt.Layouts

keys :: [KeySym] -> [NamedScratchpad] -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys workspaceKeys scratchpads = \conf -> M.union (baseKeys scratchpads conf) (extraKeys workspaceKeys conf)

baseKeys :: [NamedScratchpad] -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
baseKeys scratchpads conf@XConfig { XMonad.modMask = modMask } =
  mkKeymap conf $
  [
    ("M-S-c"                     , kill),
    ("M-S-q"                     , spawn "/home/thblt/.xmonad/scripts/stop"),
    ("M-q"                       , restart "/home/thblt/.xmonad/scripts/start" True),
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
    ("M-:"                       , sendMessage $ IncMasterN (-1)),

    -- Navigation2D
    ("M-<Up>"                    , windowGo U False),
    ("M-<Right>"                 , windowGo R False),
    ("M-<Down>"                  , windowGo D False),
    ("M-<Left>"                  , windowGo L False),

    ("M-S-<Up>"                  , windowSwap U False),
    ("M-S-<Right>"               , windowSwap R False),
    ("M-S-<Down>"                , windowSwap D False),
    ("M-S-<Left>"                , windowSwap L False),

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
    ("M-t"                       , withFocused $ windows . XSS.sink),

    ("M-p"                       , spawn "~/.bin/dmenu-desktop --entry-type=name"),
    ("M-S-<Return>"              , spawn $ terminal conf),
    ("M-C-S-<Return>"            , spawn $ "~/.xmonad/scripts/emacsclient-with-feedback"),
    ("M-s"                       , namedScratchpadAction scratchpads "term"),
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
    ("<XF86KdbBrightnessDown>"   , spawn "sudo anybrightness /sys/devices/platform/applesmc.768/leds/smc::kbd_backlight -20%")]
  where
    shNotifyVolume = "notify-send Volume `amixer get Master | tail -n 1  | awk '{print $6}'` -t 250 -h string:fgcolor:#ffffff -h string:bgcolor:#000000 -h int:value:`amixer get Master | tail -n 1 | awk '{print $4}' | sed 's/[^0-9]//g'`"

  -- workspace switching
extraKeys :: [KeySym] -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
extraKeys workspaceKeys conf@XConfig {XMonad.modMask = modMask} =
  M.fromList $
  [((m .|. modMask, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) workspaceKeys
  , (f, m) <- [(XSS.greedyView, 0), (XSS.shift, shiftMask)]]
  ++
  -- mod-{a,z,e} %! Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{a,z,e} %! Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip [xK_a, xK_z, xK_e] [0..]
  , (f, m) <- [(XSS.view, 0), (XSS.shift, shiftMask)]]
