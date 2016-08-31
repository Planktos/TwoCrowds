

#ISIIS
isiis <- read.csv(file = "data_sets/ISIIS/isiis_projects_all.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
isiis <- isiis[,c("year","volume","variety","veracity","project")]
isiis$volume<-as.numeric(isiis$volume)
isiis$big.data <- isiis$volume*isiis$variety*isiis$veracity
isiis$big.data.CumSum <- cumsum(isiis$big.data)
isiis$ann.volume.CumSum <- cumsum(isiis$volume)

save(isiis, file = "all_isiis.Robj")