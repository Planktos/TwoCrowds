library(dplyr)
#PURPOSE - Calculate big data metrics for the Challenge Expedition biological data

c <- read.csv(file = "data_sets/Challenger/challenger_year.csv", skip = 3, header = T, stringsAsFactors = F)
c <- as.data.frame(c)
c$X <- NULL

#per station volume and variety
c$variety <- 1
c$veracity <- c$total.species 
c$volume <- ifelse(test = is.na(c$no.specimens), yes = (mean(c$no.specimens, na.rm=T)), no = c$no.specimens)
vars <- c("volume", "veracity", "variety", "year")
bio <- c[vars]

chal <- bio %>% group_by(year) %>% summarise(volume = sum(volume), veracity = sum(veracity))

chal$variety <- 1

#calculate big data 
chal$big.data <- chal$volume*chal$variety*chal$veracity
chal$big.data.CumSum <- cumsum(chal$big.data)
chal$ann.volume.CumSum <- cumsum(chal$volume)
chal$project <- "Challenger"

chal.lm <- lm(formula = big.data.CumSum ~ year, data = chal)
summary(chal.lm)

#save frame as R fileschas
save(chal, file = "challenger_data.Robj")

