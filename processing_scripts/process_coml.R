
#Census of Marine Life

coml <- read.table(file = "data_sets/CoML/CoML.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
coml <- coml[,c("year","volume","variety","veracity","project")]
coml$volume<-as.numeric(coml$volume)
coml$big.data <- coml$volume*coml$variety*coml$veracity
coml$big.data.CumSum <- cumsum(coml$big.data)
coml$ann.volume.CumSum <- cumsum(coml$volume)

save(coml, file = "coml.Robj")
