Config {
    font = "xft:DejaVu Sans Mono-8:Regular"
    , additionalFonts = [ "xft:DejaVu Sans Mono-8:Bold" 
                      , "xft:Deja Sans-8:Bold" ]
    , border = NoBorder -- BottomB
    , lowerOnStart = True
    , commands = [
		Run Battery        [ "--template" , "Batt: <acstatus>"
                             , "--Low"      , "10"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , "darkred"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkgreen"

                             , "--" -- battery specific options
                                       -- discharging status
                                       , "-o"	, "<left>% (<timeleft>)"
                                       -- AC "on" status
                                       , "-O"	, "<fc=#dAA520>Charging</fc>"
                                       -- charged status
                                       , "-i"	, "<fc=#006000>Charged</fc>"
                             ] 10
        , Run Date "%H:%M" "date" 10
        --Run DateZone "%A %_d/%B/%Y %H:%M" "fr_FR.UTF_8" "Europe/Paris" "date" 10
        , Run StdinReader
    ] 
    , sepChar = "%"
    , alignSep = "}{"
    , template = "  %StdinReader% }{ %battery%     <fn=1>%date%</fn> "
    , iconRoot = "/home/thblt/.xmonad/icons"
}
