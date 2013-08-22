Config { font = "-misc-fixed-*-*-*-*-13-*-*-*-*-*-*-*"
       , borderColor = "black"
       , border = TopB
       , bgColor = "black"
       , fgColor = "grey"
       , position = Top
       , lowerOnStart = False
       , overrideRedirect = False
       , allDesktops = True
       , persistent = False
       , hideOnStart = False
       , commands = [ Run Weather "EGPF" ["-t","<station>: <tempC>C","-L","18","-H","25","--normal","green","--high","red","--low","lightblue"] 36000
                    , Run Network "ppp0" ["-L","0","-H","32","--normal","green","--high","red"] 10
                    , Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
                    , Run CoreTemp ["-t", "Temp:<core0> ºC", "-L", "40", "-H", "60", "-l", "lightblue", "-n", "gray90", "-h", "red"] 50
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run CpuFreq ["-t", "Freq:<cpu0> <cpu1>", "-L", "0", "-H", "2", "-l", "lightblue", "-n","white", "-h", "red"] 50
                    , Run BatteryP ["BAT0"]
                                     ["-t", "<left>%",
                                      "-L", "10", "-H", "80", "-p", "3",
                                      "--", "-O", "<fc=green>On</fc> - ", "-i", "",
                                      "-L", "-15", "-H", "-5",
                                      "-l", "red", "-m", "blue", "-h", "green"]
                                      600
                    , Run Com "uname" ["-s","-r"] "" 36000
                    , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    , Run Locks
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "<fc=#ee9a00>%date%</fc> <fc=red>=></fc> %cpu% %coretemp% %cpufreq% | %memory% | %battery% <fc=red>%locks%</fc> }{ %StdinReader%"
       }
