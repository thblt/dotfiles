{-
My XMonad configuration.

Note this is basically a modified copy of the default config. 
-}

{-# LANGUAGE MultiWayIf #-}

import XMonad
import XMonad.Actions.WindowBringer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as XSS
import XMonad.Util.Run (spawnPipe)

import Graphics.X11.ExtraTypes.XF86

import qualified Data.Map as M
import System.Exit (exitSuccess)
import System.IO

myWorkspaces :: [ String ]
myWorkspaces = map show [ 1 .. 9 :: Int ]

myLayout = avoidStruts $
  tall
  ||| ThreeCol 1 (3/100) (1/2)
  ||| Mirror tall
  ||| Grid
  ||| spiral (6/7)
  ||| Full
    where tall = Tall 1 (3/100) (1/2)

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList $
  [
  -- Fundamentals
    ((modMask .|. shiftMask, xK_c), kill) -- Kill focused window

  -- Doing stuff with workspaces
  , ((modMask, xK_g), gotoMenu)
  , ((modMask .|. shiftMask, xK_g), bringMenu)
    
  -- Layout transitions
  , ((modMask, xK_space), sendMessage NextLayout) -- Next layout
  , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf) -- Reset layouts
  , ((modMask, xK_h ), sendMessage Shrink) -- %! Shrink the master area
  , ((modMask, xK_l ), sendMessage Expand) -- %! Expand the master area
  , ((modMask, xK_f ), sendMessage ToggleStruts)
   
  -- increase or decrease number of windows in the master area
  , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
  , ((modMask              , xK_semicolon), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area
    
  -- Move between windows
  , ((modMask, xK_Tab ), windows XSS.focusDown) 
  , ((modMask .|. shiftMask, xK_Tab ), windows XSS.focusUp )
  -- , ((modMask, xK_j ), windows XSS.focusDown)
  -- , ((modMask, xK_k ), windows XSS.focusUp )
  , ((modMask, xK_m ), windows XSS.focusMaster )
  , ((modMask, xK_Return), windows XSS.swapMaster)
  , ((modMask .|. shiftMask, xK_j ), windows XSS.swapDown )
  , ((modMask .|. shiftMask, xK_k ), windows XSS.swapUp )
  -- floating layer support
  , ((modMask, xK_t     ), withFocused $ windows . XSS.sink) -- %! Push window back into tiling
    
    -- quit, or restart
  , ((modMask .|. shiftMask, xK_q     ), io exitSuccess) -- %! Quit xmonad
  , ((modMask, xK_q     ), spawn "xmonad --recompile && xmonad --restart; notify-send -i ~/.xmonad/notifIcon.png -u low XMonad 'Recompiled and restarted.'") -- %! Restart xmonad
    
  -- Launchers
  , ((modMask, xK_p ), spawn "synapse" )
  , ((modMask .|. shiftMask, xK_p), spawn "dmenu_run" )
  , ((modMask .|. shiftMask, xK_Return ), spawn "urxvt" )

  -- Media keys
  , ((0, xF86XK_AudioLowerVolume ), spawn "amixer set Master unmute ; amixer set Master 2-" )
  , ((0, xF86XK_AudioRaiseVolume ), spawn "amixer set Master unmute ; amixer set Master 2+" )
  , ((0, xF86XK_AudioMute ), spawn "amixer set Master toggle" )
  ]
  ++
  [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0]
        , (f, m) <- [(XSS.greedyView, 0), (XSS.shift, shiftMask)]]
  ++
  -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(XSS.view, 0), (XSS.shift, shiftMask)]]

prettyPrinter :: PP
prettyPrinter = def
  {
    ppHidden = \w -> " " ++ w ++ " "
  , ppCurrent = \w -> "<fc=#f5f4ef,#494947>\xe0b0 " ++ w ++ " </fc><fc=#494947,#f5f4ef>\xe0b0</fc>"
  , ppHiddenNoWindows = \w -> "<fc=#c4c3bf> " ++ w ++ " </fc>"                            
  , ppTitle = \t -> if
      | "" == t -> "∅"
      | otherwise -> "<fn=1><raw=" ++ (show $ length t) ++ ":" ++ t ++ "/></fn>",
    ppSep = "    "
  }

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad def
    {
      modMask = mod4Mask -- ``Windows'' key.
    , workspaces = myWorkspaces
    , keys = myKeys
    , layoutHook = myLayout
    , handleEventHook = docksEventHook
    , manageHook = composeAll
      [
        className =? "Gloobus-preview" --> doFloat
      ]
    , focusFollowsMouse = False
    , clickJustFocuses = False
    , borderWidth = 0
--  , normalBorderColor = "#000000"
--  , focusedBorderColor = "#000000"
    , logHook = dynamicLogWithPP prettyPrinter
      {
        ppOutput = hPutStrLn xmproc
      }
      >> fadeInactiveLogHook 0xeeeeeeee
    }
 
