{-
My XMonad configuration.

Note this is basically a modified copy of the default config. 
-}

import XMonad
import XMonad.Config.Azerty (azertyConfig)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as XSS
import XMonad.Util.Run (spawnPipe)

import qualified Data.Map as M
import System.Exit (exitWith, ExitCode (ExitSuccess))
import System.IO

myWorkspaces :: [ String ]
myWorkspaces = map show [ 1 .. 9 :: Int ]

myLayoutHook = avoidStruts (
  tall
  ||| ThreeCol 1 (3/100) (1/2)
  ||| Mirror tall
  ||| Grid
  ||| Full )
    where tall = Tall 1 (3/100) (1/2)

  
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
  -- Fundamentals
    (( modMask .|. shiftMask, xK_c), kill) -- Kill focused window

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
  , ((modMask, xK_j ), windows XSS.focusDown)
  , ((modMask, xK_k ), windows XSS.focusUp )
  , ((modMask, xK_m ), windows XSS.focusMaster )
  , ((modMask, xK_Return), windows XSS.swapMaster)
  , ((modMask .|. shiftMask, xK_j ), windows XSS.swapDown )
  , ((modMask .|. shiftMask, xK_k ), windows XSS.swapUp )
  -- floating layer support
  , ((modMask, xK_t     ), withFocused $ windows . XSS.sink) -- %! Push window back into tiling
    
    -- quit, or restart
  , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
  , ((modMask, xK_q     ), spawn "xmonad --recompile && xmonad --restart; notify-send -i ~/.xmonad/notifIcon.png -u low XMonad 'Recompiled and restarted.'") -- %! Restart xmonad
    
  -- Launchers
  , ((modMask, xK_p ), spawn "synapse" )
  , ((modMask .|. shiftMask, xK_p), spawn "dmenu_run" )
  , ((modMask .|. shiftMask, xK_Return ), spawn "terminator" )
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

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad def
    {
      modMask = mod4Mask -- ``Windows'' key.
    , workspaces = myWorkspaces
    , keys = myKeys
    , layoutHook = myLayoutHook
    , focusFollowsMouse = False
    , borderWidth = 0
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#ccff33"
    , logHook = dynamicLogWithPP xmobarPP
      {
        ppOutput = hPutStrLn xmproc
      }
      >> fadeInactiveLogHook 0xdddddddd
    }
 
