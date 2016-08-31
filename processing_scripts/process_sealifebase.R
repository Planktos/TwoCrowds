


library(rfishbase)

year <- seq(2007,2015,1)

total.volume <- nrow(sealifebase)

volume <- total.volume/length(year)

unique.taxon < unique(sealifebase$SpecCode)
veracity <- length(unique.taxon)/length(year)

variety <- 16
project <- "SeaLB"

slb <- data.frame(year, volume, veracity, variety, project)

slb$volume<-as.numeric(slb$volume)
slb$big.data <- slb$volume*slb$variety*slb$veracity
slb$big.data.CumSum <- cumsum(slb$big.data)
slb$ann.volume.CumSum <- cumsum(slb$volume)

save(slb, file = "SeaLifeBase.Robj")
