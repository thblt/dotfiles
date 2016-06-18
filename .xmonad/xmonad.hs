{-
    This is a now a *heavily* modified version of the default configuration.
-}

{-# LANGUAGE TemplateHaskell #-} 

import qualified Data.List as L
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import Language.Haskell.TH
import System.IO (hPutStrLn)
import System.Posix.Unistd (getSystemID, SystemID (nodeName) )
import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Actions.WindowBringer (bringMenu, gotoMenu)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, PP (..), xmobarColor)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, ToggleStruts (ToggleStruts))
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.BinarySpacePartition (emptyBSP, ResizeDirectional(..), SelectMoveNode(..), Rotate(Rotate))
import XMonad.Layout.BorderResize (borderResize)
import XMonad.Layout.DwmStyle (dwmStyle, shrinkText)
import XMonad.Layout.Fullscreen (fullscreenEventHook, fullscreenManageHook, fullscreenFull, fullscreenSupport)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.ToggleLayouts (toggleLayouts, ToggleLayout (ToggleLayout))
import qualified XMonad.Layout.Renamed as XLR
import XMonad.Layout.Spacing (smartSpacing)
import qualified XMonad.StackSet as XSS
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionCustom)

import XMonad.Actions.MouseResize

-- Computer-dependent settings.

[d| myHostName = $(stringE =<< runIO (fmap nodeName getSystemID) ) |]

workspacesKeys :: [KeySym]
workspacesKeys | myHostName == "anna" = macAzertyKeys
               | otherwise = pcAzertyKeys
  where
    pcAzertyKeys = [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0] -- From AzertyConfig
    macAzertyKeys = [0x26,0xe9,0x22,0x27,0x28,0xa7,0xe8,0x21,0xe7,0xe0] -- From AzertyConfig

-- XMonad.

myWorkspaces :: [ String ]
myWorkspaces = map show [ 1 .. 9 :: Int ]

myLayoutHook = toggleLayouts (noBorders Full) $
               XLR.renamed [(XLR.Replace "BSP")] $
                             dwmStyle shrinkText def
                             . borderResize
--               . fullscreenFull                            
                             . smartSpacing 4
                             . avoidStruts
                             . smartBorders
                             $ emptyBSP
    
--  ||| Tall 1 (3/100) (1/2) 
--  ||| renamed "ThreeCol" (ThreeCol 1 (3/100) (1/2))
--  ||| Grid
--  where renamed n l = XLR.renamed [XLR.Replace n] l

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig { XMonad.modMask = modMask } = M.fromList $
  [
    -- Fundamentals
    ((modMask .|. shiftMask, xK_c), kill) -- Kill focused window
  , ((modMask .|. shiftMask, xK_q     ), spawn "~/.xmonad/quit-xmonad.sh" ) -- Quit xmonad
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

    -- Layout management
  , ((modMask               , xK_space) , sendMessage NextLayout) -- Next layout
  , ((modMask .|. shiftMask , xK_space) , setLayout $ XMonad.layoutHook conf) -- Reset layouts
  , ((modMask               , xK_h )    , sendMessage Shrink) -- %! Shrink the master area
  , ((modMask               , xK_l )    , sendMessage Expand) -- %! Expand the master area

  , ((modMask               , xK_f )    , sendMessage ToggleStruts)
  , ((modMask .|. shiftMask , xK_f )    , sendMessage $ ToggleLayout)
{-- Doesn't seem to work with smart spacing
, ((modMask                 , xK_b )  , sendMessage $ ModifySpacing $ (+) 4)
  , ((modMask .|. controlMask , xK_b )  , sendMessage $ ModifySpacing $ (-) 4)
--}
--  , ((modMask .|. shiftMask , xK_m )    , sendMessage $ Toggle MIRROR)

{-- I don't use these:
, ((modMask, xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
  , ((modMask, xK_semicolon), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area
--}

  -- Window management within layout  
  , ((modMask .|. shiftMask,              xK_h ), sendMessage $ ExpandTowards L) -- BSP-Specific
  , ((modMask .|. shiftMask,              xK_j ), sendMessage $ ExpandTowards D) -- BSP-Specific
  , ((modMask .|. shiftMask,              xK_k ), sendMessage $ ExpandTowards U) -- BSP-Specific
  , ((modMask .|. shiftMask,              xK_l ), sendMessage $ ExpandTowards R) -- BSP-Specific

  {-- I never used these:
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
  , ((modMask, xK_s ), scratchpadSpawnActionCustom $ terminal conf ++ " -name scratchpad -e tmux-attach-or-new scratch")

    -- Misc actions
  , ((modMask, xK_g), gotoMenu) -- Go to window
  , ((modMask .|. shiftMask, xK_g), bringMenu) -- Bring window
    
  -- Media keys
  , ((0, xF86XK_AudioLowerVolume ), spawn $ "amixer set Master unmute ; amixer set Master 2-; " ++ shNotifyVolume )
  , ((0, xF86XK_AudioRaiseVolume ), spawn $ "amixer set Master unmute ; amixer set Master 2+; " ++ shNotifyVolume)
  , ((0, xF86XK_AudioMute ), spawn $ "amixer set Master toggle; " ++ shNotifyVolume )
  , ((0, xF86XK_MonBrightnessDown ), spawn "xbacklight -10" )
  , ((0, xF86XK_MonBrightnessUp ), spawn "xbacklight +10" )

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

{-
XMobar configuration.  - For consistency, xmobar isn't fully configured
through an external file, but also with command-line parameters. This
allows to keep things such as color scheme in a single place, and I
probably won't be using Xmobar outside of Xmonad.
-}

data StatusPalette = StatusPalette {
  sbpBg :: String       -- Background color
  , sbpBorder :: String -- Border color
  , sbpFg :: String     -- Default foreground color
  , sbpAct :: String    -- Activement element foreground color
  , sbpInact ::  String -- 
  , sbpDis :: String
  , sbpAlpha :: Int
  }

currentPalette :: StatusPalette
currentPalette =
  StatusPalette { 
  sbpBg = "#000000"
  , sbpBorder = "#999999"
  , sbpFg = "#ffffff"
  , sbpAct = "#ccff33"
  , sbpInact = "#cccccc"
  , sbpDis = "#333333"
  , sbpAlpha = floor $ 255 * 0.80
  }

pp_default :: String -> String
pp_default = xmobarColor (sbpFg currentPalette) (sbpBg currentPalette) 

pp_active :: String -> String
pp_active = xmobarColor (sbpAct currentPalette) (sbpBg currentPalette) 

pp_inactive :: String -> String
pp_inactive = xmobarColor (sbpInact currentPalette) (sbpBg currentPalette) 
  
pp_disabled :: String -> String
pp_disabled = xmobarColor (sbpDis currentPalette) (sbpBg currentPalette) 

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
  , ppHiddenNoWindows = \w -> handleHiddenWS w $ const (pp_disabled "·")
  , ppTitle = pp_font 2 . pp_unsafe 
  , ppSep = " "
  , ppLayout = \a -> case a of
      "BSP" -> ""
      "Full" -> xmobarColor "red" (sbpBg currentPalette) "■"
      _ -> xmobarColor "red" (sbpBg currentPalette) a
  }
  where
    defaultLayout = "BSP" 
    handleHiddenWS w f | w `elem` hiddenWorkspaces = ""
                       | otherwise = f w
      where hiddenWorkspaces = [ "NSP" ]
--    makeIcon a l | a==l = pp_active $ pp_icon $ "layout_" ++ l
--                       | otherwise = pp_inactive $ pp_icon $ "layout_" ++ l

{- And now to wrap it all up -}


main :: IO ()
main = do
  xmproc <- spawnPipe $ concat $ L.intersperse " " [ "xmobar"
                                                   , "-A", show $ sbpAlpha currentPalette
                                                   , "-F", pp_surround "\"" $ sbpFg currentPalette
                                                   , "-B", pp_surround "\"" $ sbpBg currentPalette
                                                   , "~/.xmonad/xmobar.hs"
                                                 ]
  xmonad . fullscreenSupport . withNavigation2DConfig def {
    defaultTiledNavigation = centerNavigation -- default lineNavigation is broken with BSP + smartSpacing
  } $ ewmh def {
      borderWidth = 1
    , focusedBorderColor = "#000000"
    , normalBorderColor = "#000000"
    
    , clickJustFocuses = False
    , focusFollowsMouse = False
--    , handleEventHook = fullscreenEventHook <+> docksEventHook
    , handleEventHook = docksEventHook    
    , keys = myKeys
    , layoutHook = myLayoutHook
    , logHook = dynamicLogWithPP prettyPrinter
      {
        ppOutput = hPutStrLn xmproc
      }
--      >> fadeInactiveLogHook 0.9 
    , manageHook = composeAll 
      [
        className =? "Gloobus-preview" --> doFloat
      , scratchpadManageHook $ XSS.RationalRect 0.1 0.1 0.8 0.8
--      , fullscreenManageHook
        ]
    , modMask = mod4Mask -- ``Windows'' key.
    , startupHook = setWMName "LG3D"
    , terminal = "urxvt"
    , workspaces = myWorkspaces
    }
