
#PURPOSE: Plot Two Crowds data bubble plot data figure

#AUTHOR: Kelly Robinson
#CREATED: 2 February 2016
#LAST MODIFIED: 26 July 2016

#------------------------


library("ggplot2")

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


# assign program groups
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

#big data (volume*variety)
library(plyr)
library(dplyr)
library(scales)
library(RColorBrewer)

d$veracity.variety <- d$variety*d$veracity

# vol.var <- ddply(d, .(project, program), summarise, max.vv = max(veracity.variety), max.volume = max(volume), max.big.data = max(big.data))
# vol.var$log10BD.notweight <- log(x = vol.var$max.big.data, base = 10)
# vol.var <- arrange(.data = vol.var, project)

#Account for time it took to collect data
# vol.var <- ddply(d, .(project, program), summarise, duration = length(year), max.vv = max(veracity.variety), max.volume = max(volume), max.big.data.Cum = max(big.data.CumSum))
# vol.var$BBD <- vol.var$max.big.data.Cum/vol.var$duration
# vol.var$log10BD <- log(x = vol.var$BBD, base = 10)
# vol.var <- arrange(.data = vol.var, project)

#Account for time it took to collect data
vol.var <- ddply(d, .(project, program), summarise, duration = length(year), max.vv = max(veracity.variety), max.volume = max(volume), max.big.data.Cum = max(big.data.CumSum))
vol.var$log10BD <- log(x = vol.var$max.big.data.Cum, base = 10)/vol.var$duration

#get colors
colors <- as.data.frame(read.csv("data_set_colors.csv", header = T))
vol.var <- join(vol.var, colors, "project")
vol.var$color <- as.character(vol.var$color)

#green color group - ISIIS
g.colfunc <- colorRampPalette(c("darkgreen", "white"))
gc <- g.colfunc(8)
g <- gc[c(seq(1,8,1))] #grab every other color in list

#get legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

vd <- ggplot(data = vol.var, aes(x=max.vv, y=max.volume, color = as.factor(project))) + geom_point(show.legend = T, size = 5) +
  scale_colour_manual(values = vol.var$color) +
  #scale_size(range = c(1,40), breaks = seq(1,40,1)) +
  #scale_x_continuous(name = "Maximum Variety x Veracity", expand=c(0.1,0.1), trans = "log10", breaks = c(1,10,100,1000,10000,100000), labels = trans_format('log10', math_format(10^.x))) +
  #scale_y_continuous(name = "Maximum Annual Volume", expand=c(0.1,0.1), trans = "log10", limits = c(1e3,1e7), breaks = c(1,10,1e2,1e3,1e4,1e5,1e6,1e7,1e8), labels = trans_format('log10', math_format(10^.x))) +
  theme_bw(base_size = 20) + theme(legend.box = "horizontal", legend.key = element_blank(), axis.title = element_text(face = "bold", size = 20), legend.title = element_blank(), legend.key.size = unit(2.25, "lines")) +
  guides(colour = guide_legend(fill=guide_legend(ncol = 2))) + theme(legend.position = "top")
         
v.legend <- g_legend(vd)


vv <- ggplot(data = vol.var, aes(x=max.vv, y=max.volume)) + geom_point(show.legend = T, aes(size = log10BD, color = as.factor(project))) +
      scale_colour_manual(values = vol.var$color, guide = F) +
      scale_size(range = c(1,40), breaks = seq(1,40,1)) +
      scale_x_continuous(name = "Maximum variety x veracity", expand=c(0.1,0.1), trans = "log10", breaks = c(1,10,100,1000,10000,100000), labels = trans_format('log10', math_format(10^.x))) +
      scale_y_continuous(name = "Maximum annual volume", expand=c(0.1,0.1), trans = "log10", limits = c(1e3,1e7), breaks = c(1,10,1e2,1e3,1e4,1e5,1e6,1e7,1e8), labels = trans_format('log10', math_format(10^.x))) +
      theme_bw(base_size = 30) + theme(legend.box = "vertical", legend.key = element_blank(), axis.title = element_text(face = "bold", size = 30), legend.title = element_blank(), legend.key.size = unit(2.25, "lines")) +
      theme(axis.line.y = element_line(colour = "black", size = 1), axis.line.x = element_line(colour = "black", size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks = element_line(size = 1)) +
      guides(colour = F) + theme(legend.position = "right")
            
plot(vv)

library(gridExtra)
g <- grid.arrange(vv, v.legend, ncol = 1)

plot(g)
            
#print image file to a directory
png(file = "big_data_201608_noguide.png", width = 16, height = 10, units = "in", res = 300)
plot(vv)
dev.off()                                  

