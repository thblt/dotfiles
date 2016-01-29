Config {
    font = "xft:DejaVu Sans-10:book",
    additionalFonts = [ "xft:DejaVu Sans-10:Bold" ],
    bgColor = "#f5f4ef",
    fgColor = "#1f1d14",
    border = BottomB,
    borderColor = "#3f3b27",
    alpha = 255,
--    position = Static { xpos = 0, ypos = 0, width = 2560, height = 16 },
    lowerOnStart = True,
    commands = [
        Run Date "%H:%M" "date" 1,
        --Run DateZone "%A %_d/%B/%Y %H:%M" "fr_FR.UTF_8" "Europe/Paris" "date" 10,
        Run UnsafeStdinReader
    ], 
    sepChar = "%",
    alignSep = "}{",
    template = "  %UnsafeStdinReader% }{ <fn=1>%date%</fn> ",
    iconRoot = "/home/thblt/.xmonad/icons"
}
