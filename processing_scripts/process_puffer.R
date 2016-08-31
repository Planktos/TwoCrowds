

puffer <- read.table(file = "data_sets/puffer/puffer.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
p <- puffer[,c("year","volume","variety","veracity","project")]
p$volume<-as.numeric(p$volume)
p$big.data <- p$volume*p$variety*p$veracity
p$big.data.CumSum <- cumsum(p$big.data)
p$ann.volume.CumSum <- cumsum(p$volume)

save(p, file = "puffer.Robj")
