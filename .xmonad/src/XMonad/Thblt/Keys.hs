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
import qualified XMonad.StackSet as XSS
import           XMonad.Util.NamedScratchpad (NamedScratchpad, namedScratchpadAction)

keys :: [KeySym] -> [NamedScratchpad] -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys workspacesKeys scratchpads conf@XConfig { XMonad.modMask = modMask } = M.fromList $
  [
    -- General
    ((modMask .|. shiftMask,                xK_c),                    kill)
  , ((modMask .|. shiftMask,                xK_q),                    spawn "/home/thblt/.xmonad/scripts/stop")
  , ((modMask,                              xK_q),                    restart "/home/thblt/.xmonad/scripts/start" True)
  , ((modMask,                              xK_Escape),               spawn "dm-tool lock")
  , ((modMask .|. shiftMask,                xK_Escape),               spawn "dm-tool switch-to-greeter")
  -- Layout management
  , ((modMask,                              xK_space),                sendMessage NextLayout)
  , ((modMask .|. shiftMask,                xK_space),                setLayout $ XMonad.layoutHook conf) -- Reset
  , ((modMask,                              xK_h),                    sendMessage Shrink)
  , ((modMask,                              xK_l),                    sendMessage Expand)
  , ((modMask .|. shiftMask,                xK_f),                    sendMessage ToggleStruts)
--  , ((modMask,                              xK_f),                    sendMessage $ Toggle FULL)
  , ((modMask,                              xK_equal),                sendMessage $ IncMasterN 1)
  , ((modMask,                              xK_minus),                sendMessage $ IncMasterN (-1))
  -- BSP-specific
  , ((modMask .|. shiftMask,                xK_h),                    sendMessage $ ExpandTowards L)
  , ((modMask .|. shiftMask,                xK_j),                    sendMessage $ ExpandTowards D)
  , ((modMask .|. shiftMask,                xK_k),                    sendMessage $ ExpandTowards U)
  , ((modMask .|. shiftMask,                xK_l),                    sendMessage $ ExpandTowards R)
  , ((modMask .|. controlMask,              xK_h),                    sendMessage $ ShrinkFrom L)
  , ((modMask .|. controlMask,              xK_j),                    sendMessage $ ShrinkFrom D)
  , ((modMask .|. controlMask,              xK_k),                    sendMessage $ ShrinkFrom U)
  , ((modMask .|. controlMask,              xK_l),                    sendMessage $ ShrinkFrom R)
  , ((modMask,                              xK_w),                    sendMessage $ SelectNode)
  , ((modMask,                              xK_x),                    sendMessage $ MoveNode)
  , ((modMask,                              xK_r),                    sendMessage Rotate)
  , ((modMask,                              xK_Tab),                  windows XSS.focusDown)
  , ((modMask .|. shiftMask,                xK_Tab),                  windows XSS.focusUp)
  , ((modMask,                              xK_Return),               windows XSS.swapMaster)
  , ((modMask,                              xK_t),                    withFocused $ windows . XSS.sink)
  , ((modMask,                              xK_p),                    spawn "~/.bin/dmenu-desktop --entry-type=name" )
  , ((modMask .|. shiftMask,                xK_p),                    spawn "dmenu_run")
  , ((modMask .|. shiftMask,                xK_Return),               spawn $ terminal conf)
  , ((modMask .|. controlMask .|. shiftMask,xK_Return),               spawn $ "~/.xmonad/scripts/emacsclient-with-feedback")
--, ((modMask,                              xK_s),                    scratchpadSpawnActionCustom $ terminal conf ++ " -name scratchpad -e ~/.xmonad/scripts/tmux-attach-or-new scratch")
  , ((modMask,                              xK_s),                    namedScratchpadAction scratchpads "term")
  , ((modMask .|. shiftMask,                xK_w),                    namedScratchpadAction scratchpads "web")
  , ((modMask,                              xK_g),                    gotoMenu)
  , ((modMask .|. shiftMask,                xK_g),                    bringMenu)
  , ((0,                                    xF86XK_AudioLowerVolume), spawn $ "amixer -c 0 set Master unmute ; amixer -c 0 set Master 2-; " ++ shNotifyVolume)
  , ((0,                                    xF86XK_AudioRaiseVolume), spawn $ "amixer -c 0 set Master unmute ; amixer -c 0 set Master 2+; " ++ shNotifyVolume)
  , ((0,                                    xF86XK_AudioMute),        spawn $ "amixer set Master toggle; " ++ shNotifyVolume )
  , ((0,                                    xF86XK_MonBrightnessUp),  spawn "sudo anybrightness /sys/devices/pci0000:00/0000:00:02.0/backlight/acpi_video0 +1")
  , ((0,                                    xF86XK_MonBrightnessDown),spawn "sudo anybrightness /sys/devices/pci0000:00/0000:00:02.0/backlight/acpi_video0 -1")
  , ((0 .|. shiftMask,                      xF86XK_MonBrightnessUp),  spawn "sudo anybrightness /sys/devices/pci0000:00/0000:00:02.0/drm/card0/card0-eDP-1/intel_backlight/ +1")
  , ((0 .|. shiftMask,                      xF86XK_MonBrightnessDown),spawn "sudo anybrightness /sys/devices/pci0000:00/0000:00:02.0/drm/card0/card0-eDP-1/intel_backlight/ -1")
  , ((0,                                    xF86XK_MonBrightnessDown),spawn "sudo anybrightness /sys/devices/pci0000:00/0000:00:02.0/backlight/acpi_video0 -1" )
  , ((0,                                    xF86XK_KbdBrightnessUp),  spawn "sudo anybrightness /sys/devices/platform/applesmc.768/leds/smc::kbd_backlight +20%")
  , ((0,                                    xF86XK_KbdBrightnessDown),spawn "sudo anybrightness /sys/devices/platform/applesmc.768/leds/smc::kbd_backlight -20%")
  , ((modMask,                              xK_Right),                windowGo R False)
  , ((modMask,                              xK_Left ),                windowGo L False)
  , ((modMask,                              xK_Up   ),                windowGo U False)
  , ((modMask,                              xK_Down ),                windowGo D False)
  , ((modMask,                              xK_l    ),                windowGo R False)
  , ((modMask,                              xK_h    ),                windowGo L False)
  , ((modMask,                              xK_k    ),                windowGo U False)
  , ((modMask,                              xK_j    ),                windowGo D False)
  , ((modMask .|. shiftMask,                xK_Right),                windowSwap R False)
  , ((modMask .|. shiftMask,                xK_Left ),                windowSwap L False)
  , ((modMask .|. shiftMask,                xK_Up   ),                windowSwap U False)
  , ((modMask .|. shiftMask,                xK_Down ),                windowSwap D False)
--  , ((modMask,                              xK_F7),                   sendMessage $ Toggle GAPS)

--  , ((modMask, xK_equal), sendMessage Balance)
--  , ((modMask, xK_d), sendMessage Equalize)
  ]
  ++
  -- workspace switching
  [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) workspacesKeys
        , (f, m) <- [(XSS.greedyView, 0), (XSS.shift, shiftMask)]]
  ++
  -- mod-{a,z,e} %! Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{a,z,e} %! Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_a, xK_z, xK_e] [0..]
        , (f, m) <- [(XSS.view, 0), (XSS.shift, shiftMask)]]
  where
    shNotifyVolume = "notify-send Volume `amixer get Master | tail -n 1  | awk '{print $6}'` -t 250 -h string:fgcolor:#ffffff -h string:bgcolor:#000000 -h int:value:`amixer get Master | tail -n 1 | awk '{print $4}' | sed 's/[^0-9]//g'`"
