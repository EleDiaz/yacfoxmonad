Config { font = "xft:FantasqueSansMono Nerd Font-13"
       , borderColor = "#a9212e"
       , borderWidth = 2
       , border = TopB
       , bgColor = "black"
       , fgColor = "white"
       , alpha = 255
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
                    , Run Date "<fc=white,#a9212e>%a %_d %b %H:%M </fc><fc=#a9212e,#404040> </fc>" "date" 100
                    , Run MultiCpu ["-t", " <fc=red><autovbar></fc>"] 10
                    , Run Locks
                    , Run Memory ["-t", "Mem: <used>/<total> MB"] 10
                    , Run XMonadLog
                    , Run DynNetwork ["-t", "<fc=white,#404040><dev>:  <rx>KB  <tx>KB </fc><fc=#404040></fc>"] 20
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%date%%dynnetwork% <fc=red>%locks%</fc> %memory% %multicpu% }{ %XMonadLog% "
       }
