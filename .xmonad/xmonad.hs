{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, TypeSynonymInstances #-} 

import Data.Int (Int32)
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified DBus  
import qualified DBus.Client as DBus
import Graphics.X11.ExtraTypes.XF86
import Language.Haskell.TH
import System.Posix.Unistd (getSystemID, SystemID (nodeName) )
import XMonad
import XMonad.Actions.CycleWS (nextWS, prevWS)
import XMonad.Actions.Navigation2D
import XMonad.Actions.WindowBringer (bringMenu, gotoMenu)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts (ToggleStruts))
import XMonad.Hooks.ManageHelpers (isDialog)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.BinarySpacePartition (emptyBSP, ResizeDirectional(..), SelectMoveNode(..), Rotate(Rotate))
import XMonad.Layout.BorderResize (borderResize)
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Layout.IfMax
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoFrillsDecoration
import qualified XMonad.StackSet as XSS
import XMonad.Layout.Spacing (smartSpacingWithEdge)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionCustom)
import XMonad.Util.WorkspaceCompare (getSortByIndex)

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

myLayoutHook = avoidStruts $ mkToggle (FULL ?? EOT) $
               ifMax 1 Full $
               borderResize
               . myDecoration
               . smartSpacingWithEdge 4 $
               emptyBSP
  where
    myDecoration = noFrillsDeco shrinkText def {
      decoHeight = 4
      , activeColor = activeColor
      , activeTextColor = activeColor
      , activeBorderColor = activeColor
      , inactiveColor = inactiveColor
      , inactiveTextColor = inactiveColor
      , inactiveBorderColor = inactiveColor
      }
      where
        activeColor = "#ff0000"
        inactiveColor = "#aaaaaa"

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig { XMonad.modMask = modMask } = M.fromList $
  [
    -- Fundamentals
    ((modMask .|. shiftMask, xK_c), kill) -- Kill focused window
  , ((modMask .|. shiftMask, xK_q     ), spawn "/home/thblt/.xmonad/quit-xmonad.sh" ) -- Quit xmonad
  , ((modMask, xK_q     ), spawn $ unlines [
          "xmonad --recompile"
        , "if [ $? -eq 0 ]; then"
        , "    xmonad --restart"
        , "    notify-send -u low XMonad 'Recompiled and restarted.'"
        , "else"
        , "    notify-send -u critical \"XMonad recompilation failed\" \"\n$(cat ~/.xmonad/xmonad.errors)\""
        , "fi"
        ]
     )
  , ((modMask, xK_Escape)               , spawn "dm-tool lock")
  , ((modMask .|. shiftMask, xK_Escape) , spawn "dm-tool switch-to-greeter")  

    -- Layout management
  , ((modMask               , xK_space) , sendMessage NextLayout) -- Next layout
  , ((modMask .|. shiftMask , xK_space) , setLayout $ XMonad.layoutHook conf) -- Reset layouts
  , ((modMask                           , xK_h )    , sendMessage Shrink) -- %! Shrink the master area
  , ((modMask                           , xK_l )    , sendMessage Expand) -- %! Expand the master area

  , ((modMask .|. shiftMask             , xK_f )    , sendMessage ToggleStruts)
  , ((modMask                           , xK_f )    , sendMessage $ Toggle FULL)

  -- Window management within layout  
  , ((modMask .|. shiftMask,              xK_h ), sendMessage $ ExpandTowards L) -- BSP-Specific
  , ((modMask .|. shiftMask,              xK_j ), sendMessage $ ExpandTowards D) -- BSP-Specific
  , ((modMask .|. shiftMask,              xK_k ), sendMessage $ ExpandTowards U) -- BSP-Specific
  , ((modMask .|. shiftMask,              xK_l ), sendMessage $ ExpandTowards R) -- BSP-Specific
  , ((modMask .|. controlMask,            xK_h ), sendMessage $ ShrinkFrom L) -- BSP-Specific
  , ((modMask .|. controlMask,            xK_j ), sendMessage $ ShrinkFrom D) -- BSP-Specific
  , ((modMask .|. controlMask,            xK_k ), sendMessage $ ShrinkFrom U) -- BSP-Specific
  , ((modMask .|. controlMask,            xK_l ), sendMessage $ ShrinkFrom R) -- BSP-Specific
--}
  
  , ((modMask, xK_w               ), sendMessage $ SelectNode) -- BSP-Specific
  , ((modMask, xK_x               ), sendMessage $ MoveNode) -- BSP-Specific
  , ((modMask, xK_r), sendMessage Rotate) -- BSP-Specific
    
  , ((modMask, xK_Tab ), windows XSS.focusDown) -- Focus next in stack
  , ((modMask .|. shiftMask, xK_Tab ), windows XSS.focusUp ) -- Focus preview in stack 
  , ((modMask, xK_m ), windows XSS.focusMaster ) -- Focus master 
  , ((modMask, xK_Return), windows XSS.swapMaster) -- Swap current and master

    -- floating layer support
  , ((modMask, xK_t     ), withFocused $ windows . XSS.sink) -- %! Push window back into tiling
    
    -- Launchers
  , ((modMask, xK_p ), spawn "synapse" )
  , ((modMask .|. shiftMask, xK_p), spawn "dmenu_run" )
  , ((modMask .|. shiftMask, xK_Return ), spawn $ terminal conf)
  , ((modMask .|. controlMask .|. shiftMask, xK_Return ), spawn $ "emacsclient -ca ''")
  , ((modMask, xK_s ), scratchpadSpawnActionCustom $ terminal conf ++ " -name scratchpad -e ~/.xmonad/tmux-attach-or-new scratch")

    -- Misc actions
  , ((modMask, xK_g), gotoMenu) -- Go to window
  , ((modMask .|. shiftMask, xK_g), bringMenu) -- Bring window
    
  -- Media keys
  , ((0, xF86XK_AudioLowerVolume ), spawn $ "amixer set Master unmute ; amixer set Master 2-; " ++ shNotifyVolume )
  , ((0, xF86XK_AudioRaiseVolume ), spawn $ "amixer set Master unmute ; amixer set Master 2+; " ++ shNotifyVolume)
  , ((0, xF86XK_AudioMute ), spawn $ "amixer set Master toggle; " ++ shNotifyVolume )
  , ((0, xF86XK_MonBrightnessDown ), spawn "xbacklight -10" ) 
  , ((0, xF86XK_MonBrightnessUp ), spawn "xbacklight +10" )
  , ((0, xF86XK_KbdBrightnessUp), spawn "kbdlight up 10%")
  , ((0, xF86XK_KbdBrightnessDown), spawn "kbdlight down 10%")

    -- Directional navigation of windows
  , ((modMask,                 xK_Right), windowGo R False)
  , ((modMask,                 xK_Left ), windowGo L False)
  , ((modMask,                 xK_Up   ), windowGo U False)
  , ((modMask,                 xK_Down ), windowGo D False)
    
  , ((modMask,                 xK_l), windowGo R False)
  , ((modMask,                 xK_h ), windowGo L False)
  , ((modMask,                 xK_k   ), windowGo U False)
  , ((modMask,                 xK_j ), windowGo D False)

    -- Swap adjacent windows
  , ((modMask .|. shiftMask, xK_Right), windowSwap R False)
  , ((modMask .|. shiftMask, xK_Left ), windowSwap L False)
  , ((modMask .|. shiftMask, xK_Up   ), windowSwap U False)
  , ((modMask .|. shiftMask, xK_Down ), windowSwap D False)
    
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
  dbus <- DBus.connectSession
  getWellKnownName dbus

  xmonad . fullscreenSupport . withNavigation2DConfig def {
    defaultTiledNavigation = centerNavigation -- default lineNavigation is broken with BSP + smartSpacing
  } $ ewmh def {
      borderWidth = 0
    , focusedBorderColor = "#ff0000"
    , normalBorderColor = "#000000"
    , clickJustFocuses = False
    , focusFollowsMouse = False
    --    , handleEventHook = fullscreenEventHook <+> docksEventHook
    , handleEventHook = docksEventHook    
    , keys = myKeys
    , mouseBindings = myMouseBindings
    , layoutHook = myLayoutHook
    , logHook = do
        dbusLogger dbus
--        dynamicLogWithPP$ myPP dbus -- logPipe
        fadeInactiveLogHook 0.95
    , manageHook = composeAll 
      [
      manageDocks
      , isDialog --> doFloat
      , className =? "Gloobus-preview" --> doFloat
      , className =? "Xdialog"         --> doFloat
      , className =? "Yad"             --> doFloat
      , className =? "zenity"          --> doFloat            
      , className =? "zbar"            --> doFloat
      , className =? "Zeal"            --> doFloat 
      , scratchpadManageHook $ XSS.RationalRect 0.1 0.1 0.8 0.8
        ]
    , modMask = mod4Mask -- ``Windows'' key.
    , startupHook = setWMName "LG3D"
    , terminal = "urxvt"
    , workspaces = myWorkspaces
    }
