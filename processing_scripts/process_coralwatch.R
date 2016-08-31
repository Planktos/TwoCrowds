#CoralWatch

library("data.table")
library("dplyr")
library("stringr")

c <- as.data.frame(fread(input = "data_sets/CoralWatch/CoralWatch_surveys.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE))

d <- str_split_fixed(c$Date, pattern = " ", n = 3)
c$year <- str_sub(d[,3])

volume <- c %>% group_by(year) %>% summarise(volume = sum(Records))

c <- as.data.frame(volume)

c$variety <- 1 #colour code

c$veracity <- 4 # coral type

c$volume<-as.numeric(c$volume)
c$big.data <- c$volume*c$variety*c$veracity
c$big.data.CumSum <- cumsum(c$big.data)
c$ann.volume.CumSum <- cumsum(c$volume)
c$project <- "Coral_Watch"
c$year <- as.numeric(c$year)

#save data.frame
save(c, file = "coral_watch.Robj")

