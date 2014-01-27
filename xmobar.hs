Config { font = "xft:Ubuntu-10"
       -- , borderColor = "white"
       -- , border = BottomB
       , bgColor = "black"
       , fgColor = "grey"
       , position = Top
       , lowerOnStart = False
       , overrideRedirect = False
       , allDesktops = True
       , persistent = False
       , hideOnStart = False
       , commands = [ Run BatteryP ["BAT0"]
                                     ["-t", "<left>%",
                                      "-L", "10", "-H", "80", "-p", "3",
                                      "--", "-O", "<fc=green>On</fc> - ", "-i", "",
                                      "-L", "-15", "-H", "-5",
                                      "-l", "red", "-m", "blue", "-h", "green"]
                                      600
                    , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    , Run Locks
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "<fc=white>%date%</fc> <fc=red>=></fc> Bat:%battery% | <fc=red>%locks%</fc> }{ %StdinReader%"
       }
