Config { font = "xft:FantasqueSansMono Nerd Font-13"
       , borderColor = "#a9212e"
       , borderWidth = 2
       , border = BottomB
       , bgColor = "black"
       , fgColor = "white"
       , alpha = 255
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
                    , Run Date "<fc=white,#a9212e>%a %_d %b %H:%M </fc><fc=#a9212e,#404040> </fc>" "date" 100
                    , Run MultiCpu ["-t", " <fc=red><autovbar></fc>"] 10
                    , Run Locks
                    , Run Memory ["-t", "Mem: <used>/<total> MB"] 10
                    , Run XMonadLog
                    , Run DynNetwork ["-t", "<dev>:  <rx>KB  <tx>KB"] 20
                    , Run CoreTemp ["-t", " <core0>,<core1>,<core2>,<core3> ºC",
                                    "-L", "40", "-H", "60",
                                    "-l", "lightblue,#404040", "-n", "gray90,#404040", "-h", "red,#404040"] 50
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "<action=`rofi -combi-modi drun,run -show combi`>%date%</action><fc=white,#404040>%memory% <fc=#a9212e,#404040></fc> %coretemp% <fc=#a9212e,#404040></fc> %dynnetwork% </fc><fc=#404040></fc> <fc=red>%locks%</fc> <fc=#a9212e></fc> %multicpu% }{ %XMonadLog% "
       }
