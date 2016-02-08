{-
My XMonad configuration.

Note this is basically a modified copy of the default config. 
-}

{-# LANGUAGE TemplateHaskell #-} 

import Language.Haskell.TH
import System.Posix.Unistd (getSystemID, SystemID (nodeName) )
import XMonad
import XMonad.Actions.WindowBringer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts (toggleLayouts, ToggleLayout (ToggleLayout, Toggle))
import qualified XMonad.StackSet as XSS
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad 
import Graphics.X11.ExtraTypes.XF86
import qualified Data.Map as M
import qualified Data.List as L
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)

[d| myHostName = $(stringE =<< runIO (fmap nodeName getSystemID) ) |]

myWorkspaces :: [ String ]
myWorkspaces = map show [ 1 .. 9 :: Int ]

workspacesKeys :: [KeySym]
workspacesKeys | myHostName == "anna" = macAzertyKeys
               | otherwise = pcAzertyKeys
  where
    pcAzertyKeys = [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0] -- From AzertyConfig
    macAzertyKeys = [0x26,0xe9,0x22,0x27,0x28,0xa7,0xe8,0x21,0xe7,0xe0] -- From AzertyConfig
        

hiddenWorkspaces :: [String]
hiddenWorkspaces = [ "NSP" ]

myLayout = avoidStruts . smartBorders $ toggleLayouts Full $
  tall
  ||| ThreeCol 1 (3/100) (1/2)
  ||| Mirror tall
  ||| Grid
  ||| spiral (6/7)
    where tall = Tall 1 (3/100) (1/2)

layoutNames :: [String] -- This is used by the pretty printer.
layoutNames = [
    "Tall"
  , "ThreeCol"
  , "Mirror Tall"
  , "Grid"
  , "Spiral"
  -- , "Full"
    ]
          
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList $
  [
    -- Fundamentals
    ((modMask .|. shiftMask, xK_c), kill) -- Kill focused window
  , ((modMask .|. shiftMask, xK_q     ), io exitSuccess) -- %! Quit xmonad
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

    -- Layout transitions
  , ((modMask, xK_space), sendMessage NextLayout) -- Next layout
  , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf) -- Reset layouts
  , ((modMask, xK_h ), sendMessage Shrink) -- %! Shrink the master area
  , ((modMask, xK_l ), sendMessage Expand) -- %! Expand the master area
  , ((modMask, xK_f ), sendMessage ToggleStruts)
  , ((modMask .|. shiftMask, xK_f ), sendMessage ToggleLayout)
   
    -- increase or decrease number of windows in the master area
  , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
  , ((modMask              , xK_semicolon), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area
    
    -- Move between windows
  , ((modMask, xK_Tab ), windows XSS.focusDown) 
  , ((modMask .|. shiftMask, xK_Tab ), windows XSS.focusUp )
  , ((modMask, xK_m ), windows XSS.focusMaster )
  , ((modMask, xK_Return), windows XSS.swapMaster)
  , ((modMask, xK_j ), windows XSS.swapDown )
  , ((modMask, xK_k ), windows XSS.swapUp )
    
    -- floating layer support
  , ((modMask, xK_t     ), withFocused $ windows . XSS.sink) -- %! Push window back into tiling
    
    -- Launchers
  , ((modMask, xK_p ), spawn "synapse" )
  , ((modMask .|. shiftMask, xK_p), spawn "dmenu_run" )
  , ((modMask .|. shiftMask, xK_Return ), spawn $ terminal conf)
  , ((modMask, xK_s ), scratchpadSpawnActionTerminal $ terminal conf)

    -- Bring/Goto
  , ((modMask, xK_g), gotoMenu)
  , ((modMask .|. shiftMask, xK_g), bringMenu)
    
  -- Media keys
  , ((0, xF86XK_AudioLowerVolume ), spawn $ "amixer set Master unmute ; amixer set Master 2-; " ++ shNotifyVolume )
  , ((0, xF86XK_AudioRaiseVolume ), spawn $ "amixer set Master unmute ; amixer set Master 2+; " ++ shNotifyVolume)
  , ((0, xF86XK_AudioMute ), spawn $ "amixer set Master toggle; " ++ shNotifyVolume )
  , ((0, xF86XK_MonBrightnessDown ), spawn "xbacklight -10" )
  , ((0, xF86XK_MonBrightnessUp ), spawn "xbacklight +10" )
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

{-
XMobar configuration.  - For consistency, xmobar isn't fully configured
through an external file, but also with command-line parameters. This
allows to keep things such as color scheme in a single place, and I
probably won't be using Xmobar outside of Xmonad.
-}

data Palette = Palette {
  bgColor :: String,
  fgColor :: String,
  activeColor :: String,
  inactiveColor ::  String,
  disabledColor :: String
  }

currentPalette :: Palette
currentPalette =
  Palette {
  bgColor = "#f5f4ef",
  fgColor = "#1f1d14",
  activeColor = "#000000",
  inactiveColor = "#999999",
  disabledColor = "#eeeeee" 
  }

pp_default :: String -> String
pp_default = xmobarColor (fgColor currentPalette) (bgColor currentPalette) 

pp_active :: String -> String
pp_active = xmobarColor (activeColor currentPalette) (bgColor currentPalette) 

pp_inactive :: String -> String
pp_inactive = xmobarColor (inactiveColor currentPalette) (bgColor currentPalette) 
  
pp_disabled :: String -> String
pp_disabled = xmobarColor (disabledColor currentPalette) (bgColor currentPalette) 

pp_font :: Int -> String -> String
pp_font f s = "<fn=" ++ show f ++ ">" ++ s ++ "</fn>"
  
pp_icon :: String -> String
pp_icon f = "<icon=" ++ f ++ ".xbm/>"
  
pp_unsafe :: String -> String
pp_unsafe "" = ""
pp_unsafe s = "<raw=" ++ (show $ length s) ++ ":" ++ s ++ "/>"

pp_surround :: String -> String -> String
pp_surround _ "" = ""
pp_surround a b = a ++ b ++ a

prettyPrinter :: PP
prettyPrinter = def
  {
    ppCurrent = \w -> handleHiddenWS w $ pp_active . pp_font 1
  , ppHidden = \w -> handleHiddenWS w $ pp_inactive
  , ppHiddenNoWindows = \w -> handleHiddenWS w $ pp_disabled 
  , ppTitle = pp_default . pp_unsafe 
  , ppSep = "        "
  , ppLayout = \a -> concat $ L.intersperse " " $ fmap (makeIcon a) layoutNames
  }
  where
    handleHiddenWS w f | w `elem` hiddenWorkspaces = ""
                       | otherwise = f w 
    makeIcon a l | a==l = pp_active $ pp_icon $ "layout_" ++ l
                       | otherwise = pp_inactive $ pp_icon $ "layout_" ++ l

{- And now to wrap it all up -}

main :: IO ()
main = do
  xmproc <- spawnPipe $ concat $ L.intersperse " " [ "xmobar"
                                                   , "-F", pp_surround "\"" $ fgColor currentPalette
                                                   , "-B", pp_surround "\"" $ bgColor currentPalette
                                                   , "~/.xmonad/xmobar.hs"
                                                 ]
  xmonad def
    {
      startupHook = setWMName "LG3D"
    , terminal = "urxvt"
    , modMask = mod4Mask -- ``Windows'' key.
    , workspaces = myWorkspaces
    , keys = myKeys
    , layoutHook = myLayout
    , handleEventHook = docksEventHook
    , manageHook = composeAll 
      [
        className =? "Gloobus-preview" --> doFloat
        , scratchpadManageHook $ XSS.RationalRect 0.1 0.1 0.8 0.8
      ]
    , focusFollowsMouse = False
    , clickJustFocuses = False
    , borderWidth = 0
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#ccff33"
    , logHook = dynamicLogWithPP prettyPrinter
      {
        ppOutput = hPutStrLn xmproc
      }
      >> fadeInactiveLogHook 0xeeeeeeee
    }
 
