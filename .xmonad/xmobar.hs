Config {
    font = "xft:DejaVu Sans Mono-12:book",
    additionalFonts = [ "xft:PowerlineSymbols-12:Medium" ],
    bgColor = "#000000",
    fgColor = "#ffffff",
    alpha = 180,
    position = Static { xpos = 0, ypos = 0, width = 2560, height = 16 },
    lowerOnStart = True,
    commands = [
        Run Weather "KPAO" ["-t","<tempF>F <skyCondition>","-L","64","-H","77","-n","#CEFFAC","-h","#FFB6B0","-l","#96CBFE"] 36000,
        Run Network "enp7s0f0" ["-t","Net: <rx>, <tx>","-H","200","-L","10","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10,
        Run Date "%a %b %_d %l:%M" "date" 10,
        Run StdinReader
    ], 
    sepChar = "%",
    alignSep = "}{",
    template = " %StdinReader% }{ %enp7s0f0%   <fc=#FFFFCC>%date%</fc>"
}
