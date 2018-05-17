Config { font = "xft:FuraCode Nerd Font-13"
       , borderColor = "#7f0000"
       , borderWidth = 2
       , border = BottomB
       , bgColor = "black"
       , fgColor = "white"
       , alpha = 128
       , position = Bottom
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
                    , Run Date "<fc=white,#7f0000> %H:%M </fc>" "date" 100
                    , Run MultiCpu ["-t", " <fc=#7f0000><autovbar></fc>"] 10
                    , Run Locks
                    , Run Memory ["-t", " <used>/<total> MB"] 10
                    , Run XMonadLog
                    , Run DynNetwork ["-t", " <rx>KB  <tx>KB"] 20
                    , Run CoreTemp ["-t", " <core0>·<core1>·<core2>·<core3> ºC",
                                    "-L", "40", "-H", "60",
                                    "-l", "lightblue,black", "-n", "gray90,black", "-h", "red,black"] 50
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%date%<fc=#7f0000,black></fc> %memory% <fc=#7f0000,black></fc> %coretemp% <fc=#7f0000,black></fc> %dynnetwork% <fc=#7f0000,black></fc> %locks% %multicpu% }{ %XMonadLog% "
       }
