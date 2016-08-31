
# PURPOSE: Estimate data generation rates

#AUTHOR: Kelly Robinson
#CREATED: 27 July 2016
#LAST MODIFIED: 27 July 2016

#------------------------

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

rates <- ddply(.data = d, .variables = c("project"), function(z){
  
  x <- z$year
  y <- z$ann.volume.CumSum
  
  m <- lm(y ~ x, data = z)
  
  #get slope
  R2 <- summary(m)$r.squared
  slope <- summary(m)$coefficients[2]
  sig <- summary(m)$coefficients[8]
 
  project <- unique(z$project)
  
  min.year <- min(x)
  max.year <- max(x)
  
  rates <- data.frame(project, min.year, max.year, max(y), R2, slope, sig)
  
  return(rates)
  
}, .progress = "text")

#get colors
colors <- as.data.frame(read.csv("data_set_colors.csv", header = T))
rates <- join(rates, colors, "project")
rates$color <- NULL
  
write.csv(rates, "data_set_rates.csv")

