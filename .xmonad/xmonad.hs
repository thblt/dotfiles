import XMonad

main :: IO ()
main = xmonad defaultConfig
        { modMask = mod4Mask      -- ``Windows'' key.
        , terminal = "terminator" -- Sarah Connor?
          
        }
