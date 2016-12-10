{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

import           Data.Int
import qualified Data.Map                            as M
import           Data.Maybe                          (isJust)
import qualified DBus
import qualified DBus.Client                         as DBus
import           Graphics.X11.ExtraTypes.XF86
import           Language.Haskell.TH
import           System.Posix.Unistd                 (SystemID (nodeName),
                                                      getSystemID)
import           XMonad
--import         XMonad.Actions.CycleWS              (nextWS, prevWS)
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.WindowBringer        (bringMenu, gotoMenu)
import           XMonad.Hooks.EwmhDesktops           (ewmh)
import           XMonad.Hooks.FadeInactive           (fadeInactiveLogHook)
import           XMonad.Hooks.ManageDocks            (ToggleStruts (ToggleStruts),
                                                      avoidStruts,
                                                      docksEventHook,
                                                      manageDocks)
import           XMonad.Hooks.ManageHelpers          (isDialog)
import           XMonad.Hooks.Place                  (placeHook, withGaps, smart)
import           XMonad.Hooks.SetWMName              (setWMName)
import           XMonad.Layout.BinarySpacePartition  (ResizeDirectional (..),
                                                      Rotate (Rotate),
                                                      SelectMoveNode (..),
                                                      TreeBalance (..),
                                                      emptyBSP)
import           XMonad.Layout.BorderResize          (borderResize)
import           XMonad.Layout.Fullscreen            (fullscreenSupport)
import           XMonad.Layout.IfMax
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders             (withBorder)
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.Spacing               (smartSpacingWithEdge)
import qualified XMonad.StackSet                     as XSS
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.NamedWindows            (getName)
import           XMonad.Util.Run                     (spawnPipe)
import           XMonad.Util.WorkspaceCompare        (getSortByIndex)

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

myActiveColor = "#ff0000"
myInactiveColor = "#000000"

data MySpacing = MySpacing {
  myGaps       :: Int,
  myBorderSize :: Int,
  myDecoHeight :: Int
  }

mySpacing :: MySpacing
mySpacing | myHostName == "rudiger" = MySpacing 4 2 4
          | otherwise = MySpacing 1 1 0

myLayoutHook = avoidStruts $ mkToggle (FULL ?? EOT) $
               ifMax 1 Full $
               borderResize
               . myDecoration
               . smartSpacingWithEdge (myGaps mySpacing)
               . withBorder (fromIntegral $ myBorderSize mySpacing) $
               emptyBSP
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
    --   }


myScratchpads :: [NamedScratchpad]
myScratchpads = [
  NS "term" "urxvt -title urxvt_scratchpad_1545645 -e ~/.xmonad/tmux-attach-or-new scratch" (title =? "urxvt_scratchpad_1545645")
  (customFloating rect),

  NS "web" "surf" (className =? "Surf")
    (customFloating rect)
  ]
  where ratio = 12
        rect = XSS.RationalRect (1/ratio) (1/ratio) ((ratio-2)/ratio) ((ratio-2)/ratio)

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig { XMonad.modMask = modMask } = M.fromList $
  [
    -- General
    ((modMask .|. shiftMask,                xK_c),                    kill)
  , ((modMask .|. shiftMask,                xK_q),                    spawn "/home/thblt/.xmonad/quit-xmonad.sh")
  , ((modMask,                              xK_q),                    spawn "/home/thblt/.xmonad/recompile-xmonad.sh")
  , ((modMask,                              xK_Escape),               spawn "dm-tool lock")
  , ((modMask .|. shiftMask,                xK_Escape),               spawn "dm-tool switch-to-greeter")
  -- Layout management
  , ((modMask,                              xK_space),                sendMessage NextLayout)
  , ((modMask .|. shiftMask,                xK_space),                setLayout $ XMonad.layoutHook conf) -- Reset
  , ((modMask,                              xK_h),                    sendMessage Shrink)
  , ((modMask,                              xK_l),                    sendMessage Expand)
  , ((modMask .|. shiftMask,                xK_f),                    sendMessage ToggleStruts)
  , ((modMask,                              xK_f),                    sendMessage $ Toggle FULL)
--, ((modMask,                              xK_equal),                sendMessage $ IncMasterN 1)
--, ((modMask,                              xK_minus),                sendMessage $ IncMasterN (-1))
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
--, ((modMask,                              xK_p),                    spawn "synapse" )
  , ((modMask .|. shiftMask,                xK_p),                    spawn "dmenu_run")
  , ((modMask .|. shiftMask,                xK_Return),               spawn $ terminal conf)
  , ((modMask .|. controlMask .|. shiftMask,xK_Return),               spawn $ "emacsclient -ca ''")
--, ((modMask,                              xK_s),                    scratchpadSpawnActionCustom $ terminal conf ++ " -name scratchpad -e ~/.xmonad/tmux-attach-or-new scratch")
  , ((modMask,                              xK_s),                    namedScratchpadAction myScratchpads "term")
  , ((modMask,                              xK_w),                    namedScratchpadAction myScratchpads "web")
  , ((modMask,                              xK_g),                    gotoMenu)
  , ((modMask .|. shiftMask,                xK_g),                    bringMenu)
  , ((0,                                    xF86XK_AudioLowerVolume), spawn $ "amixer -c 0 set Master unmute ; amixer -c 0 set Master 2-; " ++ shNotifyVolume)
  , ((0,                                    xF86XK_AudioRaiseVolume), spawn $ "amixer -c 0 set Master unmute ; amixer -c 0 set Master 2+; " ++ shNotifyVolume)
  , ((0,                                    xF86XK_AudioMute),        spawn $ "amixer set Master toggle; " ++ shNotifyVolume )
  , ((0,                                    xF86XK_MonBrightnessUp),  spawn "sudo ~/.bin/anybrightness /sys/devices/pci0000:00/0000:00:02.0/backlight/acpi_video0 +1")
  , ((0,                                    xF86XK_MonBrightnessDown),spawn "sudo ~/.bin/anybrightness /sys/devices/pci0000:00/0000:00:02.0/backlight/acpi_video0 -1")
  , ((0 .|. shiftMask,                      xF86XK_MonBrightnessUp),  spawn "sudo ~/.bin/anybrightness /sys/devices/pci0000:00/0000:00:02.0/drm/card0/card0-eDP-1/intel_backlight/ +1")
  , ((0 .|. shiftMask,                      xF86XK_MonBrightnessDown),spawn "sudo ~/.bin/anybrightness /sys/devices/pci0000:00/0000:00:02.0/drm/card0/card0-eDP-1/intel_backlight/ -1")
  , ((0,                                    xF86XK_MonBrightnessDown),spawn "sudo ~/.bin/anybrightness /sys/devices/pci0000:00/0000:00:02.0/backlight/acpi_video0 -1" )
  , ((0,                                    xF86XK_KbdBrightnessUp),  spawn "sudo ~/.bin/anybrightness /sys/devices/platform/applesmc.768/leds/smc::kbd_backlight +20%")
  , ((0,                                    xF86XK_KbdBrightnessDown),spawn "sudo ~/.bin/anybrightness /sys/devices/platform/applesmc.768/leds/smc::kbd_backlight -20%")
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

  , ((modMask, xK_equal), sendMessage Balance)
  , ((modMask, xK_d), sendMessage Equalize)
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

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
    [
      ((modMask, 1), \w -> focus w >> mouseMoveWindow w
                                 >> windows XSS.shiftMaster)
--    , ((0, 10), const $ prevWS) -- Three-finger left swipe
--    , ((0, 11), const $ nextWS) -- Three finger right swipe
    ]


{- DBus interface -}

getWellKnownName :: DBus.Client -> IO ()
getWellKnownName dbus = do
  DBus.requestName dbus (DBus.busName_ "org.xmonad.Log")
                [DBus.nameAllowReplacement, DBus.nameReplaceExisting, DBus.nameDoNotQueue]
  return ()

dbusLogger :: DBus.Client -> X ()
dbusLogger dbus = do

  {- Note to future self: Would you need to get Xinerama to work (report
     screen for each workspace) look at the code of
     XMonad.Hooks.DynamicLog.pprWindowSetXinerama -}

  winset <- gets windowset
  sortws <- getSortByIndex
--  urgents <- readUrgents

  -- layout description
  let ld = description . XSS.layout . XSS.workspace . XSS.current $ winset

      -- workspace list
      all_ws = sortws $ XSS.workspaces winset
      -- this workspace
      this_ws = XSS.currentTag winset
      -- all visible
      vis_ws = fmap (XSS.tag . XSS.workspace) (XSS.visible winset)

      workspaces = fmap (\ws -> let id = getWsName ws in
                                  (id, -- Name
                                   0:: Int32, -- screen id (xinerama, unimplemented)
                                   id == this_ws, -- is current
                                   id `elem` vis_ws, -- is visible but not current (xinerama, untested)
                                   isJust (XSS.stack ws), -- contains windows
                                   id `elem` myHiddenWorkspaces) -- is hidden
                        ) all_ws


  -- window title
  title <- maybe (return "") (fmap show . getName) . XSS.peek $ winset

  let signal = (DBus.signal (DBus.objectPath_ "/org/xmonad/Log") (DBus.interfaceName_ "org.xmonad.Log") (DBus.memberName_ "Update")) {
        DBus.signalBody = [
            DBus.toVariant ld,
            DBus.toVariant workspaces,
            DBus.toVariant title
            ]
        }
  liftIO $ DBus.emit dbus signal
    where
      getWsName (XSS.Workspace t _ _) = t


{- And now to wrap it all up -}

main :: IO ()
main = do
--dbus <- connectSession
--getWellKnownName dbus

  xmonad . fullscreenSupport . withNavigation2DConfig def {
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
      , className =? "Gloobus-preview" --> doFloat
      , className =? "Xdialog"         --> doFloat
      , className =? "Yad"             --> doFloat
      , className =? "zenity"          --> doFloat
      , className =? "zbar"            --> doFloat
        ]
    , modMask = mod4Mask -- ``Windows'' key.
    , startupHook = setWMName "LG3D"
    , terminal = "urxvt"
    , workspaces = myWorkspaces
    }
