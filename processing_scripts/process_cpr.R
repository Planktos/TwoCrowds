
cpr <- read.table(file = "data_sets/CPR/cpr_data_20170211.csv", sep = ",", header = F, stringsAsFactors = FALSE, skip = 1)

colnames(cpr) <- c("year","volume","variety","veracity","project")

cpr$volume<-as.numeric(cpr$volume)
cpr$big.data <- cpr$volume*cpr$variety*cpr$veracity
cpr$big.data.CumSum <- cumsum(cpr$big.data)
cpr$ann.volume.CumSum <- cumsum(cpr$volume)

save(cpr, file = "cpr.Robj")