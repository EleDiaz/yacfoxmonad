Config { font = "xft:Ubuntu-10"
       , borderColor = "black"
       , border = TopB
       , bgColor = "white"
       , fgColor = "black"
       , position = Bottom
       , lowerOnStart = False
       , overrideRedirect = False
       , allDesktops = True
       , persistent = False
       , hideOnStart = False
       , commands = [ Run Weather "EGPF" ["-t","<station>: <tempC>C","-L","18","-H","25","--normal","green","--high","red","--low","lightblue"] 36000
                    , Run Brightness ["-t", "Bright: <percent>"] 100
                    , Run Mpris2 "rhythmbox" ["-t", "<artist> - [<composer>] <title>"] 10
                    , Run Network ".0" ["-L","0","-H","32","--normal","green","--high","red"] 10
                    , Run Volume "default" "Master" [] 10
                    , Run MultiCpu ["-t", "Cpu: <total>% <total0>%", "-L","3","-H","50","--normal","blue","--high","red"] 10
                    , Run CoreTemp ["-t", "Temp:<core0><core1> C", "-L", "40", "-H", "60", "-l", "lightblue", "-n", "gray90", "-h", "red"] 50
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run NamedXPropertyLog "_XMONAD_LOG" "prop"
                    , Run Kbd []
                    , Run CpuFreq ["-t", "Freq:<cpu0> <cpu1>", "-L", "0", "-H", "2", "-l", "lightblue", "-n","blue", "-h", "red"] 50
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "<fc=red>=> </fc> %multicpu% | %cpufreq% | %memory% | %bright% | XP:%prop% | }{ %mpris2% <fc=red>||</fc> %default:Master% "
       }
