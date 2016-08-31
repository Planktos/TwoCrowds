#JEDI

library("data.table")
library("stringr")
library("dplyr")

j <- as.data.frame(fread(input = "data_sets/JEDI/JeDI.csv", sep = ",", header = TRUE, na.strings = "-9999", stringsAsFactors = FALSE))

vars <- c("taxon", "rank_genus", "rank_species", "density", "density_integrated", "biovolume", "biovolume_integrated","weight_wet", "weight_dry", "presence_absence", "year")
bio <- j[vars]

#remove spaces in field values
b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
bio <- as.data.frame(b)

#change factors to numbers
taxon <- as.character(bio$taxon)
genus <- as.character(bio$rank_genus)
species <- as.character(bio$rank_species)
PA <- as.character(bio$presence_absence)

factors <- bio[,c(4:9,11)]
asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
b <- factorsNumeric(factors)

#add names back in
bio <- cbind(taxon, genus, species, PA, b)

#set nd to NAs
bio[bio == -9999] <- NA
bio[bio == "-9999.0"] <- NA

bio$gs <- ifelse(is.na(bio$species), str_c(bio$genus,"_sp"), str_c(bio$genus,"_",bio$species))
bio$tax <- ifelse(test = is.na(bio$gs), bio$taxon, bio$gs)
bio$gs <- NULL
bio$taxon <- NULL
bio$genus <- NULL
bio$species <- NULL
names(bio)[names(bio) == "tax"] <- "taxon"

# get number of metrics collected for species
m.vars <- c("density", "density_integrated","biovolume","biovolume_integrated","weight_dry","weight_wet","PA")
metrics <- bio[m.vars]
m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
mm <- melt(data = m)

colnames(mm) <- c("no.measurements")
bio <- cbind(bio, mm)

# Volume

#get the total number of measurements made for each species for each year
bio.cast <- dcast(data = bio, year ~ taxon, sum, value.var = "no.measurements")

value_vars <- names(bio.cast[,c(2:length(bio.cast))])
bio.m <- melt(data = bio.cast, id.vars = c("year"), measure.vars = value_vars, na.rm = T,
              variable.name = "taxon", value.name = "total.no.measurements")

by.taxon.volume <- bio.m %>% group_by(year,taxon) %>% summarise(total.no.measurements = sum(total.no.measurements))

# total number of measurements
volume <- by.taxon.volume %>% group_by(year) %>% summarise(volume = sum(total.no.measurements))

# Veracity - number species with measurements for a given year 
s <- subset(by.taxon.volume, total.no.measurements > 0)
veracity <- s %>% group_by(year) %>% summarise(veracity = length(taxon))

# Variety - number of measurement types
variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))

#Final data frame
vv <- plyr::join(volume,variety)
jedi <- plyr::join(vv, veracity)

#add data set name
jedi$big.data <- jedi$volume*jedi$variety*jedi$veracity
jedi$big.data.CumSum <- cumsum(jedi$big.data)
jedi$ann.volume.CumSum <- cumsum(jedi$volume)
jedi$project <- "JEDI"

save(jedi, file = "jedi_process.Robj")

