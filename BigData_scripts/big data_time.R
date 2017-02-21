
# PURPOSE: Plot Two Crowds data figure volume over time
 
#AUTHOR: Kelly Robinson
#CREATED: 2 February 2016
#LAST MODIFIED: 26 July 2016

#------------------------

library("ggplot2")
library(scales)
library(plyr)
library(dplyr)
library(RColorBrewer)

source("functions.R")

load("jedi_process.Robj")
load("coral_watch.Robj")
load("challenger_data.Robj")
load("globec_GB.Robj")
load("globec_NP.Robj")
load("globec_SO.Robj")
load("all_isiis.Robj")
load("cpr.Robj")
load("coml.Robj")
load("puffer.Robj")
load("SeaLifeBase.Robj")
load("bats_data.Robj")
load("puffer.Robj")
load("JGOFS_HOTS.Robj")


#assign program groups
gb$program <- "GLOBEC"
np.gb$program <- "GLOBEC"
so$program <- "GLOBEC"
isiis$program <- "ISIIS"
slb$program <- "SeaLife Base"
coml$program <- "Census of Marine Life"
cpr$program <- "Continous Plankton Recorder"
jedi$program <- "Jellyfish Database Initative"
p$program <- "Pufferfish Genome"
c$program <- "Coral Watch"
bats$program <- "JGOFS"
hots$program <- "JGOFS"
chal$program <- "Challenger Expedition"

#make one large data frame
d <- rbind(chal, cpr, gb, np.gb, so, coml, slb, jedi, c, p, bats, hots, isiis)
d <- na.omit(d)

#blue color group - GLOBEC
colfunc <- colorRampPalette(c("darkblue", "lightblue"))
colfunc(3)

#red color group - ISIIS
g.colfunc <- colorRampPalette(c("darkgreen", "white"))
gc <- g.colfunc(14)
g <- gc[c(seq(1,14,2))] #grab every other color in list

#get colors
colors <- as.data.frame(read.csv("data_set_colors.csv", header = T))
d <- join(d, colors)
d$color <- as.character(d$color)


#build main plot 
# ds <- d[d$year > 1900,] 
# x.min <- min(ds$year)
# x.max <- max(ds$year)+1
# x.breaks <- seq(x.min, x.max, (roundUpNice(x.max-x.min)/10))
# 
# y.min <- min(ds$ann.volume.CumSum)
# y.max <- max(ds$ann.volume.CumSum)
# y.breaks <- c(1,10,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9)

x.min <- min(d$year)
x.max <- max(d$year)+1
x.breaks <- seq(x.min, x.max, (roundUpNice(x.max-x.min)/20))

y.min <- min(d$ann.volume.CumSum)
y.max <- max(d$ann.volume.CumSum)
y.breaks <- c(1,10,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9)

v <- ggplot(NULL,aes(x=year, y=ann.volume.CumSum)) +
            #Challenger
            geom_line(data = d[d$project == "Challenger",], colour = "black", size = 1.2) +
            #geom_point(data = d[d$project == "Challenger",], colour = "black", pch=21, size = 5, stroke = 2, show.legend = F) +
            geom_point(data = d[d$project == "Challenger",], colour = "black", size = 4, show.legend = F) +
            # Censuse of Marine Life
            geom_line(data = d[d$project == "CoML",], colour = "darkgoldenrod1", size = 1.2) +
            #geom_point(data = d[d$project == "CoML",], colour = "black", pch=21, size = 5, stroke = 2, show.legend = F) +
            geom_point(data = d[d$project == "CoML",], size = 5, colour = "darkgoldenrod1", show.legend = F) +
            # CPR
            geom_line(data = d[d$project == "CPR",], colour = "firebrick", size = 1.2) +
            #geom_point(data = d[d$project == "CPR",], colour = "black", pch=21, size = 5, show.legend = F) +
            geom_point(data = d[d$project == "CPR",], colour = "firebrick", size = 4, show.legend = F) +
            # 1. ISIIS-SoFL
            geom_line(data = d[d$project == "ISIIS-SoFL",], colour = "#006400", size = 1.2) +
            #geom_point(data = d[d$project == "ISIIS-SoFL",], colour = "black", pch=21, size = 5, show.legend = F) +
            geom_point(data = d[d$project == "ISIIS-SoFL",], colour = "#006400", size = 4, show.legend = F) +
            # 2. ISIIS-SCB-jellyfish
            #geom_point(data = d[d$project == "ISIIS-SCB-jellyfish",], colour = "black", pch=21, size = 5, show.legend = F) +
            geom_point(data = d[d$project == "ISIIS-SCB-jellyfish",], colour = "#247A24", size = 4, show.legend = F) +
            #4. ISIIS-nGOM
            #geom_point(data = d[d$project == "ISIIS-nGOM",], colour = "black", pch=21, size = 5, show.legend = F) +
            geom_point(data = d[d$project == "ISIIS-nGOM",], colour = "#489048", size = 4, show.legend = F) +
            #4. ISIIS-SCB
           # geom_point(data = d[d$project == "ISIIS-SCB",], colour = "black", pch=21, size = 5, show.legend = F) +
            geom_point(data = d[d$project == "ISIIS-SCB",], colour = "#6DA66D", size = 4, show.legend = F) +
            #5. ISIIS-Monterey Bay
            #geom_point(data = d[d$project == "ISIIS-MontereyBay",], colour = "black", pch=21, size = 5, show.legend = F) +
            geom_point(data = d[d$project == "ISIIS-MontereyBay",], colour = "#91BC91", size = 4, show.legend = F) +
            #5. ISIIS-Stellwagen
            #geom_point(data = d[d$project == "ISIIS-Stellwagen",], colour = "black", pch=21, size = 5, show.legend = F) +
            geom_point(data = d[d$project == "ISIIS-Stellwagen",], colour = "#B6D2B6", size = 4, show.legend = F) +
            # Seabase Life
            geom_line(data = d[d$project == "SeaLB",], colour = "slategrey", size = 1.2) +
            #geom_point(data = d[d$project == "SeaLB",], colour = "black", pch=21, size = 5, show.legend = F) +
            geom_point(data = d[d$project == "SeaLB",], colour = "slategrey", size = 4, show.legend = F) +
            # JEDI
            geom_line(data = d[d$project == "JEDI",], colour = "lightseagreen", size = 1.2) +
            #geom_point(data = d[d$project == "JEDI",], colour = "black", pch=21, size = 5, show.legend = F) +
            geom_point(data = d[d$project == "JEDI",], colour = "lightseagreen", size = 4, show.legend = F) +
            # Coral Watch
            geom_line(data = d[d$project == "Coral_Watch",], colour = "darkorange2", size = 1.2) +
            #geom_point(data = d[d$project == "Coral_Watch",], colour = "black", pch=21, size = 5, show.legend = F) +
            geom_point(data = d[d$project == "Coral_Watch",], colour = "darkorange2", size = 4, show.legend = F) +
            # BATS
            geom_line(data = d[d$project == "BATS",], colour = "deeppink4", size = 1.2) +
            #geom_point(data = d[d$project == "BATS",], colour = "black", pch=21, size = 5, show.legend = F) +
            geom_point(data = d[d$project == "BATS",], colour = "deeppink4", size = 4, show.legend = F) +
            # HOTS
            geom_line(data = d[d$project == "HOTS",], colour = "deeppink", size = 1.2) +
           # geom_point(data = d[d$project == "HOTS",], colour = "black", pch=21, size = 5, show.legend = F) +
            geom_point(data = d[d$project == "HOTS",], colour = "deeppink", size = 4, show.legend = F) +
            # 1. Globec_GB
            #geom_point(data = d[d$project == "Globec_GB",], colour = "darkblue", pch=21, size = 5, show.legend = F) +
            geom_point(data = d[d$project == "Globec_GB",], colour = "darkblue", size = 4, show.legend = F) +
            geom_line(data = d[d$project == "Globec_GB",], colour = "darkblue", size = 1.2) +
            # 2. Globec_SO
            geom_line(data = d[d$project == "Globec_SO",], colour = "dodgerblue3", size = 1.2) +
            #geom_point(data = d[d$project == "Globec_SO",], colour = "black", pch=21, size = 5, show.legend = F) +
            geom_point(data = d[d$project == "Globec_SO",], colour = "dodgerblue3", size = 4, show.legend = F) + 
            # 4. Globec_NP
            #geom_point(data = d[d$project == "Globec_NP",], colour = "black", pch=21, size = 5, show.legend = F) +
            geom_point(data = d[d$project == "Globec_NP",], colour = "skyblue2", size = 4, show.legend = F) + 
           # Puffer Fish Genome
            #geom_point(data = d[d$project == "Pufferfish genome",], colour = "black", pch=21, size = 5, show.legend = F) +
            geom_point(data = d[d$project == "Pufferfish genome",], colour = "yellow1", size = 4, show.legend = F) +
            
            scale_x_continuous(name = NULL, expand=c(0.1,0.1), limits = c(x.min, x.max), breaks = x.breaks) +
            scale_y_continuous(name = "Cumulative annual volume", expand=c(0.1,0.1), trans = "log10", limits = c(1,1e9), breaks = y.breaks, labels = trans_format('log10', math_format(10^.x))) +
            theme_bw(base_size = 30) + theme(axis.title = element_text(face = "bold", size = 30), legend.title = element_blank(), legend.key.size = unit(2.25, "lines")) +
            theme(axis.line.y = element_line(colour = "black", size = 1), axis.line.x = element_line(colour = "black", size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks = element_line(size = 1)) +
            guides(colour = F) + theme(legend.position = "right")

plot(v)

#print image file to a directory
png(file = "VolCum_201702_noguide.png", width = 16, height = 10, units = "in", res = 300)
plot(v)
dev.off()   


# build Challenger plot
# dc <- d[d$year < 1900,] 
# x.min <- min(dc$year)
# x.max <- max(dc$year)
# x.breaks <- seq(x.min, x.max, (roundUpNice(x.max-x.min)/2))
# 
# y.min <- min(d$ann.volume.CumSum)
# y.max <- max(d$ann.volume.CumSum)
# y.breaks <- c(1,10,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9)
# 
# c <- ggplot(NULL,aes(x=year, y=ann.volume.CumSum)) +
#   stat_smooth(data = dc[dc$project == "Challenger",], method = "loess", fullrange = F, level = 0.95, se = F, colour = "black", size = 1.2, alpha = 0.1) +
#   geom_point(data = dc[dc$project == "Challenger",], colour = "black", size = 6, show.legend = F) +
#   scale_x_continuous(expand=c(0.1,0.1), limits = c(x.min, x.max), breaks = x.breaks) +
#   scale_y_continuous(name = "Cumulative Volume", expand=c(0.1,0.1), trans = "log10", limits = c(1,1e9), breaks = y.breaks, labels = trans_format('log10', math_format(10^.x))) +
#   theme_bw(base_size = 15)
# 
# #print image file to a directory
# png(file = "chal_volume_variety_201607_noguide.png", width = 3, height = 12, units = "in", res = 300)
# plot(c)
# dev.off()   

# Main plot using stat_smooth
# v <- ggplot(NULL,aes(x=year, y=ann.volume.CumSum)) +
#   #Challenger
#   stat_smooth(data = d[d$project == "Challenger",], method = "loess", fullrange = F, level = 0.95, se = F, colour = "black", size = 1.2, alpha = 0.1) +
#   geom_point(data = d[d$project == "Challenger",], colour = "black", size = 5, show.legend = F) +
#   # CPR
#   geom_point(data = d[d$project == "CPR",], colour = "springgreen4", size = 5, show.legend = F) +
#   stat_smooth(data = d[d$project == "CPR",], method = "loess", fullrange = F, level = 0.95, se = F, colour = "springgreen4", size = 1.2, alpha = 0.1) +
#   # 1. Globec_GB
#   stat_smooth(data = d[d$project == "Globec_GB",], method = "loess", fullrange = F, level = 0.95, se = F, colour = "darkblue", size = 1.2, alpha = 0.1) +
#   geom_point(data = d[d$project == "Globec_GB",], colour = "darkblue", size = 5, show.legend = F) +
#   # 2. Globec_SO
#   stat_smooth(data = d[d$project == "GLOBEC_SO",], method = "lm", fullrange = F, se = F, colour = "lightblue", size = 1.2, alpha = 0.1) +
#   geom_point(data = d[d$project == "GLOBEC_SO",], colour = "lightblue", size = 5, show.legend = F) + 
#   # 3. Globec_NP
#   geom_point(data = d[d$project == "Globec_NP",], colour = "deepskyblue3", size = 5, show.legend = F) + 
#   # 1. ISIIS-SoFL
#   stat_smooth(data = d[d$project == "ISIIS-SoFL",], method = "lm", fullrange = F, level = 0.95, se = F, colour = "firebrick", size = 1.2, alpha = 0.1) +
#   geom_point(data = d[d$project == "ISIIS-SoFL",], colour = "firebrick", size = 5, show.legend = F) +
#   # 2. ISIIS-SCB-jellyfish
#   geom_point(data = d[d$project == "ISIIS-SCB-jellyfish",], colour = "#BD4444", size = 5, show.legend = F) +
#   #3. ISIIS-nGOM
#   geom_point(data = d[d$project == "ISIIS-nGOM",], colour = "#C96666", size = 5, show.legend = F) +
#   #4. ISIIS-SCB
#   geom_point(data = d[d$project == "ISIIS-SCB",], colour = "#D58888", size = 5, show.legend = F) +
#   #5. ISIIS-Monterey Bay
#   geom_point(data = d[d$project == "ISIIS-MontereyBay",], colour = "#E1AAAA", size = 5, show.legend = F) +
#   #5. ISIIS-Stellwagen
#   geom_point(data = d[d$project == "ISIIS-Stellwagen",], colour = "mistyrose", size = 5, show.legend = F) +
#   # Censuse of Marine Life
#   geom_point(data = d[d$project == "CoML",], colour = "darkgoldenrod1", size = 5, show.legend = F) +
#   stat_smooth(data = d[d$project == "CoML",], method = "loess", fullrange = F, level = 0.95, se = F, colour = "darkgoldenrod1", size = 1.2, alpha = 0.1) +
#   # Seabase Life
#   geom_point(data = d[d$project == "SeaLB",], colour = "grey54", size = 5, show.legend = F) +
#   stat_smooth(data = d[d$project == "SeaLB",], method = "loess", fullrange = F, level = 0.95, se = F, colour = "grey54", size = 1.2, alpha = 0.1) +
#   # JEDI
#   geom_point(data = d[d$project == "JEDI",], colour = "darkolivegreen4", size = 5, show.legend = F) +
#   stat_smooth(data = d[d$project == "JEDI",], method = "lm", fullrange = F, level = 0.95, se = F, colour = "darkolivegreen4", size = 1.2, alpha = 0.1) +
#   # Coral Watch
#   geom_point(data = d[d$project == "Coral_Watch",], colour = "darkorange2", size = 5, show.legend = F) +
#   stat_smooth(data = d[d$project == "Coral_Watch",], method = "loess", fullrange = F, level = 0.95, se = F, colour = "darkorange2", size = 1.2, alpha = 0.1) +
#   # BATS
#   geom_point(data = d[d$project == "BATS",], colour = "deeppink4", size = 5, show.legend = F) +
#   stat_smooth(data = d[d$project == "BATS",], method = "loess", fullrange = F, level = 0.95, se = F, colour = "deeppink4", size = 1.2, alpha = 0.1) +
#   # HOTS
#   geom_point(data = d[d$project == "HOTS",], colour = "deeppink", size = 5, show.legend = F) +
#   stat_smooth(data = d[d$project == "HOTS",], method = "loess", fullrange = F, level = 0.95, se = F, colour = "deeppink", size = 1.2, alpha = 0.1) +
#   # Puffer Fish Genome
#   geom_point(data = d[d$project == "Pufferfish genome",], colour = "yellow", size = 5, show.legend = F) +
#   
#   #scale_colour_manual(values = c("green2","darkorchid3", "black", "deepskyblue3", "forestgreen","blue4","deeppink3" , "firebrick", "tan4","darkgoldenrod1" , "grey48")) +
#   scale_x_continuous(expand=c(0.1,0.1), limits = c(x.min, x.max), breaks = x.breaks) +
#   scale_y_continuous(name = "Cumulative Volume", expand=c(0.1,0.1), trans = "log10", limits = c(1,1e9), breaks = y.breaks, labels = trans_format('log10', math_format(10^.x))) +
#   theme_bw(base_size = 15)


