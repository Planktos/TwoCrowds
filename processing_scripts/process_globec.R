#PURPOSE: Pre-process GLOBEC data for Tale of Two Crowds data figures

#AUTHOR: Kelly Robinson
#CREATED: 2 February 2016
#LAST MODIFIED: 26 July 2016



library("dplyr")
library("reshape2")
library(stringr)

#GLOBEC
 
#Georges Bank ----
 
#1. Bongo Vols ----
  glb.gb.bongo <- read.table(file = "data_sets/GLOBEC/GeorgesBank/bongovols/bongovols.txt", sep = "\t", header = TRUE, na.strings = "-9999", stringsAsFactors = FALSE)
  glb.gb.bongo <- as.data.frame(glb.gb.bongo)
  
  #subset biological fields and metrics
  bio <- glb.gb.bongo[,c(2,15)]
  
  #remove spaces from field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #make NAs zeros
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  
  #change factors to numbers
  factors <- bio[,c(1:2)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  bio <- factorsNumeric(factors)
  
  #get the annual total number of observations
  bongo <- bio %>% group_by(year) %>% summarise(volume = n())
  colnames(recs_year.bongo) <- c("year", "volume")
  
  #Variety (dimensions)
  #variety (number of fraction x number of metrics)
  bongo$variety <- 1
  
  #veracity
  bongo$veracity <- 1
  
  #final data frame
  bongo$var <- "bongo.disply.vols"

#2. chloro_bottle----
  glb.gb.chl <- read.table(file = "data_sets/GLOBEC/GeorgesBank/chloro_bottle/chloro_bottle.txt", sep = "\t", header = TRUE, na.strings = "-9999", stringsAsFactors = FALSE)
  glb.gb.chl <- as.data.frame(glb.gb.chl)
  
  #subset biological fields and metrics
  bio <- glb.gb.chl[,c(2,13:15)]
  
  #remove spaces from field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #make nd into NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  
  #change factors to numbers
  fraction <- as.character(bio$fraction)
  factors <- bio[,c(1,3)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  #add fraction labels back in
  bio <- cbind(fraction, b)
  bio$fraction<-as.character(bio$fraction)
  
  bio$no_metrics <- 1
  
  #get the total number of measurments made for each fraction
  bio.cast <- dcast(data = bio, year + no_metrics ~ fraction, length, value.var = "chl_a")
  
  #get the annual total number of observations for each fraction
  volume <- apply(bio.cast[3:6], MARGIN = 1, FUN = function(x) sum(x))
  vol <- melt(volume)
  colnames(vol) <- c("volume")
  bio.cast$volume <- volume
  
#Variety (dimensions)
    #get number of unique parameters for each year
    v <- apply(bio.cast[3:6], MARGIN = 1, FUN = function(x) length(x[(x!=0)]))
    vm <- melt(v)
    colnames(vm) <- c("no_fractions")
    
    bio.cast <- cbind(bio.cast, vm)
    
    #variety (number of fraction x number of metrics)
    bio.cast$variety <- bio.cast$no_fractions * bio.cast$no_metrics

#final data frame
  chl_bottle <- bio.cast[ ,c("year", "variety", "volume")]
  chl.bottle <- chl_bottle %>% group_by(year) %>% summarise(volume = sum(volume), variety = max(variety))
  
  #Veracity
  chl.bottle$veracity <- 1
  
  chl.bottle$var <- "chloro.bottle"

  

#3. fish eggs_rs ----
  
  glb.gb.fishegg <- read.table(file = "data_sets/GLOBEC/GeorgesBank/fisheggs_rs/fisheggs_rs.txt", fill = T, sep = "\t", header = TRUE, na.strings = "-9999", stringsAsFactors = FALSE)
  glb.gb.fishegg <- as.data.frame(glb.gb.fishegg)
  
  #subset biological fields and metrics
  bio <- glb.gb.fishegg[,c(2,16:26)]
  rm(glb.gb.fishegg)
  
  #remove spaces from field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  taxon <- as.character(bio$taxon)
  
  factors <- bio[,c(1,3:12)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  #add taxon or common names back in
  bio <- cbind(taxon, b)
  bio$taxon<-as.character(bio$taxon)
  bio <- subset(bio, taxon != "NO_EGGS_TAKEN")
  
#Volume  
  
  # get number of measurements done for species, excluding NAs
  metrics <- bio[,c(3:12)]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  
  colnames(mm) <- c("no_measurements")
  
  bio <- cbind(bio, mm)
  
  #get the total number of measurements made for each species for each year
  bio.cast <- dcast(data = bio, year ~ taxon, sum, value.var = "no_measurements")
  
  value_vars <- names(bio.cast[,c(2:length(bio.cast))])
  bio.m <- melt(data = bio.cast, id.vars = c("year"), measure.vars = value_vars, na.rm = T,
                variable.name = "taxon", value.name = "total.no.measurements")
  
  by.taxon.volume <- bio.m %>% group_by(year,taxon) %>% summarise(total.no.measurements = sum(total.no.measurements))
  
  # total number of measurements
  volume <- by.taxon.volume %>% group_by(year) %>% summarise(volume = sum(total.no.measurements))

# Veracity - number species with measurements for a given year 
  s <- subset(by.taxon.volume, total.no.measurements>0)
  veracity <- s %>% group_by(year) %>% summarise(veracity = length(taxon))

# Variety - number of measurement types
  variety <- 2 #total num_caught & num at given stage
  
#Final data frame
  # group by year
  fish_egg <- plyr::join(volume, veracity)
  fish_egg$variety <- variety

  #add data set name
  fish_egg$var <- "fish_eggs_rs"

#4. fish larvae_bss -----
  glb.gb.fishlbon <- read.table(file = "data_sets/GLOBEC/GeorgesBank/fishlarvae_Bon_bss/fishlarvae_Bon_bss.txt", fill = T, sep = "\t", header = TRUE, na.strings = "-9999", stringsAsFactors = FALSE)
  glb.gb.fishlbon <- as.data.frame(glb.gb.fishlbon)
  glb.gb.fishlbon$common_name <- gsub(x = glb.gb.fishlbon$common_name, pattern = " *", replacement = "")
  
  vars <- c("common_name", "length", "prot_total", "RNA", "DNA","rings", "sagitta_diam", "sagitta_chk_diam", "lapillus_diam", "growth", "RNA_DNA", "prot_predicted", "year")
  bio <- glb.gb.fishlbon[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
    
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  name <- as.character(bio$common_name)
  factors <- bio[2:13]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  #add names back in
  bio <- cbind(name, b)
  bio$name<-as.character(bio$name)
  
  # get number of metrics collected for species
  m.vars <- c("length", "prot_total", "RNA", "DNA","rings", "sagitta_diam", "sagitta_chk_diam", "lapillus_diam", "growth", "RNA_DNA","prot_predicted")
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  
  colnames(mm) <- c("no.measurements")
  bio <- cbind(bio, mm)

 # Volume
  
  #get the total number of measurements made for each species for each year
  bio.cast <- dcast(data = bio, year ~ name, sum, value.var = "no.measurements")
  
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
  fish_bon <- plyr::join(vv, veracity)

  #add data set name
  fish_bon$var <- "fish.bongo.larvae"
             

#5. fish larvae_pro -----
  glb.gb.fishlpro <- read.table(file = "data_sets/GLOBEC/GeorgesBank/fish_larvae_Bon_pro/fishlarvae_Bon_pro.txt", fill = T, sep = "\t", header = TRUE, na.strings = "-9999", stringsAsFactors = FALSE)
  glb.gb.fishlpro <- as.data.frame(glb.gb.fishlpro)

  vars <- c("common_name", "length", "prot_total", "RNA", "DNA","rings", "sagitta_diam", "sagitta_chk_diam", "lapillus_diam", "growth", "RNA_DNA","prot_predicted", "year")
  bio <- glb.gb.fishlpro[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)

  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  name <- as.character(bio$common_name)
  factors <- bio[2:13]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  #add names back in
  bio <- cbind(name, b)
  bio$name<-as.character(bio$name)
  
  # get number of metrics collected for species
  m.vars <- c("length", "prot_total", "RNA", "DNA","rings", "sagitta_diam", "sagitta_chk_diam", "lapillus_diam", "growth", "RNA_DNA","prot_predicted")
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
# Volume
  
  #get the total number of measurements made for each species for each year
  bio.cast <- dcast(data = bio, year ~ name, sum, value.var = "no.measurements")
  
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
  fish_pro <- plyr::join(vv, veracity)
  
  #add data set name
  fish_pro$var <- "fish.pro.larvae"
  

#6. fish larvae_M1 -----
  glb.gb.fishlM1 <- read.table(file = "data_sets/GLOBEC/GeorgesBank/fishlarvae_M1/fishlarvae_M1.txt", fill = T, sep = "\t", header = TRUE, na.strings = "-9999", stringsAsFactors = FALSE)
  glb.gb.fishlM1 <- as.data.frame(glb.gb.fishlM1)
  
  vars <- c("year", "common_name", "length", "size_class", "prot_total", "RNA", "DNA","rings", "sagitta_diam", "sagitta_chk_diam", "lapillus_diam", "growth", "RNA_DNA", "prot_predicted")
  bio <- glb.gb.fishlM1[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  name <- as.character(bio$common_name)
  factors <- bio[,c(1,3:14)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  #add names back in
  bio <- cbind(name, b)
  bio$name<-as.character(bio$name)
  
  # get number of metrics collected for species
  m.vars <- c("length", "size_class", "prot_total", "RNA", "DNA","rings", "sagitta_diam", "sagitta_chk_diam", "lapillus_diam", "growth", "RNA_DNA","prot_predicted")
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
# Volume
  
  #get the total number of measurements made for each species for each year
  bio.cast <- dcast(data = bio, year ~ name, sum, value.var = "no.measurements")
  
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
  
# Final data frame
  vv <- plyr::join(volume,variety)
  fishlM1 <- plyr::join(vv, veracity)

#add data set name
  fishlM1$var <- "fish.larvae.M1"

#7. moc10_abund ----
  glb.gb.moc10a <- read.table(file = "data_sets/GLOBEC/GeorgesBank/moc10_abund/moc10_abund.txt", fill = T, sep = "\t", header = TRUE, na.strings = "-9999", stringsAsFactors = FALSE)
  glb.gb.moc10a <- as.data.frame(glb.gb.moc10a)
  glb.gb.moc10a$species <- gsub(x = glb.gb.moc10a$species, pattern = " *", replacement = "")
  
  vars <- c("species", "abund", "year")
  bio <- glb.gb.moc10a[vars]
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  species <- as.character(bio$species)
  factors <- bio[2:3]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  #add names back in
  bio <- cbind(species, b)
  bio$species<-as.character(bio$species)
  
  # get number of metrics collected for species
  m.vars <- c("abund")
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  
# Volume
  
  #get the total number of valid observations for each data species during each year
  bio.cast <- dcast(data = bio, year ~ species, sum, value.var = "no.measurements")

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
  
# Final data frame
  vv <- plyr::join(volume,variety)
  moc10a <- plyr::join(vv, veracity)
  
  # add data set name
  moc10a$var <- "moc10.abund"
  

#8. moc10_length ----
  glb.gb.moc10l <- read.table(file = "data_sets/GLOBEC/GeorgesBank/moc10_lengths/moc10data_lengths_rs.txt", fill = T, sep = "\t", header = TRUE, na.strings = "-9999", stringsAsFactors = FALSE)
  glb.gb.moc10l <- as.data.frame(glb.gb.moc10l)
  glb.gb.moc10l$species <- gsub(x = glb.gb.moc10l$species, pattern = " *", replacement = "")
  
  vars <- c("species", "length", "year")
  bio <- glb.gb.moc10l[vars]
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  species <- as.character(bio$species)
  factors <- bio[2:3]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  #add names back in
  bio <- cbind(species, b)
  bio$species<-as.character(bio$species)
  
  
  # get number of metrics collected for species
  m.vars <- c("length")
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)

# Volume
  
  #get the total number of valid observations for each data species during each year
  bio.cast <- dcast(data = bio, year ~ species, sum, value.var = "no.measurements")
  
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
  
  # Final data frame
  vv <- plyr::join(volume,variety)
  moc10l <- plyr::join(vv, veracity)
  
  # add data set name
  moc10l$var <- "moc10.length"
 
  
  
  
#9. nut_phyto ----
  glb.gb.nut.phyto <- read.table(file = "data_sets/GLOBEC/GeorgesBank/nut_phyto/nut_phyto.txt", fill = T, sep = "\t", header = TRUE, na.strings = "-9999", stringsAsFactors = FALSE)
  glb.gb.nut.phyto <- as.data.frame(glb.gb.nut.phyto)
  
  vars <- c("year", "flvolt", "chl_a", "phaeo")
  bio <- glb.gb.nut.phyto[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  factors <- bio
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  bio <- factorsNumeric(factors)
  
  value_vars <- names(bio[,c(3:length(bio))])
  bio.m <- melt(bio, id.vars = "year", measure.vars = value_vars, na.rm = T)
  
  #get number of metrics collected
  m.vars <- c("chl_a","phaeo") #excluding flvolts because chla and phaeo are derived from flvolts
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # total number of measurements
  volume <- bio %>% group_by(year) %>% summarise(volume = sum(no.measurements))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  # Final data frame
  vv <- plyr::join(volume,variety)
  nut.phyto <- plyr::join(vv, veracity)
  
  # Veracity - number species with measurements for a given year 
  nut.phyto$veracity <- 1
  
  # add data set name
  nut.phyto$var <- "nut.phyto"
  
  
  
  
  
  
#10. fish larvaeB_rs ----
  glb.gb.fishlB <- read.table(file = "data_sets/GLOBEC/GeorgesBank/fishlarvaeB_rs/fishlarvaeB_rs.txt", fill = T, sep = "\t", header = TRUE, na.strings = "-9999", stringsAsFactors = FALSE)
  glb.gb.fishlB <- as.data.frame(glb.gb.fishlB)
  
  vars <- c("year", "taxon", "num_caught", "num_measure", "fish_len", "num_fish_len")
  bio <- glb.gb.fishlB[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  taxon <- as.character(bio$taxon)
  factors <- bio[,c(1,3:6)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  #add names back in
  bio <- cbind(taxon, b)
  bio$taxon<-as.character(bio$taxon)
  
  # get number of metrics collected for species
  m.vars <- c("num_caught", "num_measure", "fish_len", "num_fish_len")
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  #get the total number of valid observations for each data species during each year
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
  
  # Final data frame
  vv <- plyr::join(volume,variety)
  fishlB <- plyr::join(vv, veracity)
  
  # add data set name
  fishlB$var <- "fish.larvae.B"
  

#11. fish larvaeM_rs -----
  glb.gb.fishlM <- read.table(file = "data_sets/GLOBEC/GeorgesBank/fishlarvaeM_rs/fishlarvaeM_rs.txt", fill = T, sep = "\t", header = TRUE, na.strings = "-9999", stringsAsFactors = FALSE)
  glb.gb.fishlM <- as.data.frame(glb.gb.fishlM)
  
  vars <- c("year", "taxon", "num_caught", "num_measure", "fish_len", "num_fish_len")
  bio <- glb.gb.fishlM[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  taxon <- as.character(bio$taxon)
  factors <- bio[,c(1,3:6)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  #add names back in
  bio <- cbind(taxon, b)
  bio$taxon<-as.character(bio$taxon)
  
  # get number of metrics collected for species
  m.vars <- c("num_caught", "num_measure", "fish_len", "num_fish_len")
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  #get the total number of valid observations for each data species during each year
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
  
  # Final data frame
  vv <- plyr::join(volume,variety)
  fishlM <- plyr::join(vv, veracity)
  
  # add data set name
  fishlM$var <- "fish.larvae.M"
  
  

#12. VPR -----
  glb.gb.vpr <- read.table(file = "data_sets/GLOBEC/GeorgesBank/vpr/vpr_cashjian.txt", fill = T, sep = "\t", header = TRUE, na.strings = "-9999", stringsAsFactors = FALSE)
  glb.gb.vpr <- as.data.frame(glb.gb.vpr)
  
  vars <- c(2,15,18:47)
  bio <- glb.gb.vpr[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  factors <- bio
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  bio <- factorsNumeric(factors)
  
  value_vars <- names(bio[,c(3:length(bio)-1)])
  bio.m <- melt(data = bio, id.vars = c("year"), measure.vars = value_vars, na.rm = T, variable.name = "taxon")
  
  bio.m$no.measurements <- 1
  
  by.taxon.volume <- bio.m %>% group_by(year,taxon) %>% summarise(total.no.measurements = sum(no.measurements))
  
  # total number of measurements
  volume <- by.taxon.volume %>% group_by(year) %>% summarise(volume = sum(total.no.measurements))
  
  # Veracity - number species with measurements for a given year 
  s <- subset(by.taxon.volume, total.no.measurements > 0)
  veracity <- s %>% group_by(year) %>% summarise(veracity = length(taxon))

  # Variety - number of measurement types
  variety <-  bio.m[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  # Final data frame
  vv <- plyr::join(volume,variety)
  vpr <- plyr::join(vv, veracity)
  
  # add data set name
  vpr$var <- "vpr"
  

#13. zoo_cb_bongo ----
  glb.gb.zoo.cb.b <- read.table(file = "data_sets/GLOBEC/GeorgesBank/zoo_cb_bongo/zoo_cb_meter_bongo.txt", fill = T, sep = "\t", header = TRUE, na.strings = "-9999", stringsAsFactors = FALSE)
  glb.gb.zoo.cb.b <- as.data.frame(glb.gb.zoo.cb.b)
  glb.gb.zoo.cb.b$year <- glb.gb.zoo.cb.b$year_cruise_end
  
  vars <- c(33,35:37)
  bio <- glb.gb.zoo.cb.b[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  taxon <- bio$taxon
  stage <- bio$stage
  factors <- bio[3:4]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  #add names back in
  bio <- cbind(taxon, stage, b)
  bio$taxon <- as.character(bio$taxon)
  bio$stage <- as.character(bio$stage)
  
  # get number of metrics collected for species
  m.vars <- c("count")
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  #get the total number of measurements made for each species for each year
  bio.cast <- dcast(data = bio, year ~ taxon, sum, value.var = "no.measurements")
  
  value_vars <- names(bio.cast[,c(2:length(bio.cast))])
  bio.m <- melt(data = bio.cast, id.vars = c("year"), measure.vars = value_vars, na.rm = T, variable.name = "taxon", value.name = "total.no.measurements")
  
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
  zoo.cb.b <- plyr::join(vv, veracity)
  
  #add data set name
  zoo.cb.b$var <- "zoo.cb.b"
  
  
  
  
#14. zoo_cb_pump ------
  
  glb.gb.zoo.cb.p <- read.table(file = "data_sets/GLOBEC/GeorgesBank/zoo_cb_pump/zoo_cb_meter_pump.txt", fill = T, sep = "\t", header = TRUE, na.strings = "-9999", stringsAsFactors = FALSE)
  glb.gb.zoo.cb.p <- as.data.frame(glb.gb.zoo.cb.p)
  glb.gb.zoo.cb.p$year <- glb.gb.zoo.cb.p$year_cruise_end
  
  vars <- c(33,35:37)
  bio <- glb.gb.zoo.cb.p[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  taxon <- bio$taxon
  stage <- bio$stage
  factors <- bio[3:4]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  #add names back in
  bio <- cbind(taxon, stage, b)
  bio$taxon <- as.character(bio$taxon)
  bio$stage <- as.character(bio$stage)
  
  # get number of metrics collected for species
  m.vars <- c("count")
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
  
  # total number of measurements
  volume <- bio.m %>% group_by(year) %>% summarise(volume = sum(total.no.measurements))
  
  # Veracity - number species with measurements for a given year 
  s <- subset(bio.m, total.no.measurements > 0)
  veracity <- s %>% group_by(year) %>% summarise(veracity = length(taxon))
  
  # Variety - number of measurement types
  bio.c <- dcast(bio, year + taxon ~ stage, sum, value.var = "no.measurements")
  
  m.types <- bio.c[,c(3:length(bio.c))]
  types <- apply(bio.c, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  types.m <- melt(data = types)
  colnames(types.m) <- c("no.measurement.types")
  
  bio.c <- cbind(bio.c, types.m)
  
  variety <-  bio.c[,c("year","no.measurement.types")] %>% group_by(year) %>% summarise(variety = max(no.measurement.types))
  
  #Final data frame
  vv <- plyr::join(volume,variety)
  zoo.cb.p <- plyr::join(vv, veracity)
  
  #add data set name
  zoo.cb.p$var <- "zoo.cb.p"

#15. zoo_m3 -----

  glb.gb.zoo.m3 <- read.table(file = "data_sets/GLOBEC/GeorgesBank/zoo_cubic_m/zoo_cubic_meter.txt", fill = T, sep = "\t", header = TRUE, na.strings = "-9999", stringsAsFactors = FALSE)
  glb.gb.zoo.m3 <- as.data.frame(glb.gb.zoo.m3)
  glb.gb.zoo.m3$year <- glb.gb.zoo.m3$year_cruise_end
  
  vars <- c(33,35:37)
  bio <- glb.gb.zoo.m3[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  taxon <- bio$taxon
  stage <- bio$stage
  factors <- bio[3:4]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  #add names back in
  bio <- cbind(taxon, stage, b)
  bio$taxon <- as.character(bio$taxon)
  bio$stage <- as.character(bio$stage)
  
  # get number of number of measurements collected for each species
  m.vars <- c("count")
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
  
  # total number of measurements
  volume <- plyr::ddply(.data = bio.m, .variables = "year", summarise, volume = sum(total.no.measurements))
  
  # Veracity - number species with measurements for a given year 
  s <- subset(bio.m, total.no.measurements > 0)
  veracity <- s %>% group_by(year) %>% summarise(veracity = length(taxon))
  
  # Variety - number of measurement types
  bio.c <- dcast(bio, year + taxon ~ stage, sum, value.var = "no.measurements")
  
  m.types <- bio.c[,c(3:length(bio.c))]
  types <- apply(bio.c, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  types.m <- melt(data = types)
  colnames(types.m) <- c("no.measurement.types")
  
  bio.c <- cbind(bio.c, types.m)
  
  variety <-  bio.c[,c("year","no.measurement.types")] %>% group_by(year) %>% summarise(variety = max(no.measurement.types))
  
  #Final data frame
  vv <- plyr::join(volume,variety)
  zoo.m3 <- plyr::join(vv, veracity)
  
  #add data set name
  zoo.m3$var <- "zoo.m3"

#16 zoo_pump ----
  
  glb.gb.zoo.p <- read.table(file = "data_sets/GLOBEC/GeorgesBank/zoo_pump/zoo_pump.txt", fill = T, sep = "\t", header = TRUE, na.strings = "-9999", stringsAsFactors = FALSE)
  glb.gb.zoo.p <- as.data.frame(glb.gb.zoo.p)
  
  vars <- c(2,12:16)
  bio <- glb.gb.zoo.p[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  taxon <- bio$taxon
  stage <- bio$stage
  sex <- bio$sex
  
  factors <- bio[,c(1,4,6)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  #add names back in
  bio <- cbind(taxon, stage, sex, b)
  bio$taxon <- as.character(bio$taxon)
  bio$stage <- as.character(bio$stage)
  bio$sex <- as.character(bio$sex)
  
  # get number of number of measurements collected for each species
  m.vars <- c("stage","stage_num", "sex","num_per_m3")
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
  
  # total number of measurements
  volume <- plyr::ddply(.data = bio.m, .variables = "year", summarise, volume = sum(total.no.measurements))
  
  # Veracity - number species with measurements for a given year
  s <- subset(bio.m, total.no.measurements > 0)
  veracity <- s %>% group_by(year) %>% summarise(veracity = length(taxon))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  #Final data frame
  vv <- plyr::join(volume,variety)
  zoo.p <- plyr::join(vv, veracity)
  
  #add data set name
  zoo.p$var <- "zoo.p"
                                   

# Combine into one data frame ----
                                   
d <- rbind(fish_egg, chl.bottle, bongo, fish_bon, fish_pro, fishlM1, fishlM, fishlB,
           moc10a, moc10l, nut.phyto, vpr, zoo.cb.b, zoo.cb.p, zoo.m3, zoo.p)
                                   
gb <- d %>% group_by(year) %>% summarise(volume = sum(volume), veracity = sum(veracity), variety = sum(variety))                                   

gb$big.data <- gb$volume*gb$variety*gb$veracity
gb$big.data.CumSum <- cumsum(gb$big.data)
gb$ann.volume.CumSum <- cumsum(gb$volume)
gb$project <- "Globec_GB"
save(gb, file = "globec_GB.Robj")
                                                 
# dm <- dcast(d, year ~ var, value.var = "n")
#                                                
# #Variety (dimensions)
# #get number of unique parameters for each year
#  v <- apply(dm, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
#                                                 
#  vm <- melt(v)
#                                                  
#  colnames(vm) <- c("variety")
#  
#  vm$variety <- vm$variety - 1 #remove count of year as a biological dimension
#                                                  
#  dm <- cbind(dm, vm)
#                                              
#                                                     
#  #Velocity
#  #make NAs zeros
# dm$chloro_bottle <- ifelse(is.na(dm$chloro_bottle), 0, dm$chloro_bottle)
#                                                 
# dm$bongovols <- ifelse(is.na(dm$bongovols), 0, dm$bongovols)
# dm$fish_eggs <- ifelse(is.na(dm$fish_eggs), 0, dm$fish_eggs)
# dm$fish_larvae_bss <- ifelse(is.na(dm$fish_larvae_bss), 0, dm$fish_larvae_bss)
# dm$fish_larvae_pro <- ifelse(is.na(dm$fish_larvae_pro), 0, dm$fish_larvae_pro)
# dm$fish_larvae_M1 <- ifelse(is.na(dm$fish_larvae_M1), 0, dm$fish_larvae_M1)
# dm$fish_larvaeB <- ifelse(is.na(dm$fish_larvaeB), 0, dm$fish_larvaeB)
# dm$fish_larvaeM <- ifelse(is.na(dm$fish_larvaeM), 0, dm$fish_larvaeM)
# dm$moc10a <- ifelse(is.na(dm$moc10a), 0, dm$moc10a)
# dm$moc10l <- ifelse(is.na(dm$moc10l), 0, dm$moc10l)
# dm$nut.phyto <- ifelse(is.na(dm$nut.phyto), 0, dm$nut.phyto)
# dm$vpr <- ifelse(is.na(dm$vpr), 0, dm$vpr)
# dm$zoo.cb.b <- ifelse(is.na(dm$zoo.cb.b), 0, dm$zoo.cb.b)
#  
#                                                       dm$zoo.cb.p <- ifelse(is.na(dm$zoo.cb.p), 0, dm$zoo.cb.p)
#                                              
#                                                       dm$zoo.m3 <- ifelse(is.na(dm$zoo.m3), 0, dm$zoo.m3)
#                                                  
#                                                       dm$zoo.p <- ifelse(is.na(dm$zoo.p), 0, dm$zoo.p)
#                                                 
#                                                       
#                                                         
#                                                    #get number of total observations per year
#                                                     dm$ann.volume <- dm$bongovols + dm$chloro_bottle + dm$fish_eggs + dm$fish_larvae_bss + dm$fish_larvae_pro +
#                                                    + dm$fish_larvae_M1 + dm$fish_larvaeB + dm$fish_larvaeM + dm$moc10a + dm$moc10l + dm$nut.phyto +
#                                                    +dm$vpr + dm$zoo.cb.b + dm$zoo.cb.p + dm$zoo.m3 + dm$zoo.p
#                                                         
#                                                     g <- dm[c("year", "variety", "ann.volume")]
#                                                     g$ann.volume.cum <- cumsum(g$ann.volume)
#                                                     g$big.data <- g$ann.volume*g$variety
#                                                     g$big.data.cum <- cumsum(g$big.data)
#                                                     g$log10BD <- log10(g$big.data.cum)
#                                                     
#                                                     #plot data
#                                                       globec <- ggplot(data = g, aes(x=year, y=log(x = log10BD, base = 10))) + geom_point() + stat_smooth(method = "loess")
#                                                     plot(globec)
#                                                     
#                                                     #save data frame
#                                                     g$project <- "GLOBEC_GB"
#                                                   save(g, file = "globec_GB.R")
#                                                   
                                                    
# NE PACIFIC -------------
                                                    
 # birds ----------
   glb.np.bird <- read.table(file = "data_sets/GLOBEC/NEPacific/birds/birds.txt", fill = T, sep = "\t", header = TRUE, na.strings = "nd", stringsAsFactors = FALSE)
   glb.np.bird <- as.data.frame(glb.np.bird)
   
   vars <- c(1,7,15:16)
   bio <- glb.np.bird[vars]
   
   #remove spaces in field values
   b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
   bio <- as.data.frame(b)
   
   #set nd to NAs
   b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
   bio <- as.data.frame(b)
   
   #change factors to numbers
   species <- bio$species
  
   factors <- bio[,c(1,3:4)]
   asNumeric <- function(x) as.numeric(as.character(x))
   factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
   b <- factorsNumeric(factors)
   
   #add names back in
   bio <- cbind(species, b)
   bio$species <- as.character(bio$species)
  
   # get number of number of measurements collected for each species
   m.vars <- c("flight_dir","behav_code")
   metrics <- bio[m.vars]
   m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
   mm <- melt(data = m)
   colnames(mm) <- c("no.measurements")
   
   bio <- cbind(bio, mm)
   
   # Volume
   
   #get the total number of measurements made for each species for each year
   bio.cast <- dcast(data = bio, year ~ species, sum, value.var = "no.measurements")
   
   value_vars <- names(bio.cast[,c(2:length(bio.cast))])
   bio.m <- melt(data = bio.cast, id.vars = c("year"), measure.vars = value_vars, na.rm = T, 
                 variable.name = "taxon", value.name = "total.no.measurements")
   
   # total number of measurements
   volume <- plyr::ddply(.data = bio.m, .variables = "year", summarise, volume = sum(total.no.measurements))
   
   # Veracity - number species with measurements for a given year
   s <- subset(bio.m, total.no.measurements > 0)
   veracity <- s %>% group_by(year) %>% summarise(veracity = length(taxon))
   
   # Variety - number of measurement types
   variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
   
   #Final data frame
   vv <- plyr::join(volume,variety)
   birds <- plyr::join(vv, veracity)
   
   #add data set name
   birds$var <- "birds"
 
 
#cetaceans -----
 
  glb.np.cet <- read.table(file = "data_sets/GLOBEC/NEPacific/cetaceans/cetaceans.txt", fill = T, sep = "\t", header = TRUE, na.strings = "nd", stringsAsFactors = FALSE)
  glb.np.cet <- as.data.frame(glb.np.cet)
  
  vars <- c(3,5)
  bio <- glb.np.cet[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  factors <- bio[,c(1)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- asNumeric(factors)
  
  #add names back in
  bio <- cbind(bio, b)
  bio$year <- NULL
  names(bio)[names(bio) == "b"] <- "year"
  
  
  # get number of number of measurements collected for each species
  m.vars <- c("species")
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  #get the total number of measurements made for each species for each year
  bio.cast <- dcast(data = bio, year ~ species, sum, value.var = "no.measurements")
  
  value_vars <- names(bio.cast[,c(2:length(bio.cast))])
  bio.m <- melt(data = bio.cast, id.vars = c("year"), measure.vars = value_vars, na.rm = T, 
                variable.name = "taxon", value.name = "total.no.measurements")
  
  # total number of measurements
  volume <- plyr::ddply(.data = bio.m, .variables = "year", summarise, volume = sum(total.no.measurements))
  
  # Veracity - number species with measurements for a given year
  s <- subset(bio.m, total.no.measurements > 0)
  veracity <- s %>% group_by(year) %>% summarise(veracity = length(taxon))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  #Final data frame
  vv <- plyr::join(volume,variety)
  np.cet <- plyr::join(vv, veracity)
  np.cet$var <- "np.cetaceans"
  
                                                      
# Combine into one data frame
  np <- rbind(birds, np.cet)
  np.gb <- np %>% group_by(year) %>% summarise(volume = sum(volume), veracity = sum(veracity), variety = sum(variety))                                   
  
  np.gb$big.data <- np.gb$volume*np.gb$variety*np.gb$veracity
  np.gb$big.data.CumSum <- cumsum(np.gb$big.data)
  np.gb$ann.volume.CumSum <- cumsum(np.gb$volume)
  np.gb$project <- "Globec_NP"
  save(np.gb, file = "globec_NP.Robj")


# SOUTHERN OCEAN ---------------
                                                      
# bacteria_brine -----
  
   so.bact <- read.table(file = "data_sets/GLOBEC/SouthOce/bacteria_brine/bacteria_brine.txt", fill = T, sep = "\t", header = TRUE, na.strings = "nd", stringsAsFactors = FALSE)
   so.bact <- as.data.frame(so.bact)
   
   vars <- c(2,12)
   bio <- so.bact[vars]
   rm(so.bact)
   
   #remove spaces in field values
   b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
   bio <- as.data.frame(b)
   
   #set nd to NAs
   b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
   bio <- as.data.frame(b)
   
   #change factors to numbers
   year <- bio[,c(1)]
   asNumeric <- function(x) as.numeric(as.character(x))
   factorsNumeric <- function(d) modifyList(year, lapply(year[, sapply(year, is.factor)], asNumeric))
   years <- asNumeric(year)
   
   chl <- bio[,c(2)]
   asNumeric <- function(x) as.numeric(as.character(x))
   factorsNumeric <- function(d) modifyList(chl, lapply(chl[, sapply(chl, is.factor)], asNumeric))
   chls <- asNumeric(chl)
   
   bio <- as.data.frame(cbind(years, chls))
   
  # get number of number of measurements collected for each species
   bio$no.measurements <- 1
   names(bio)[names(bio) == "years"] <- "year"
   
  # Volume
   
   # total number of measurements
   volume <- plyr::ddply(.data = bio, .variables = "year", summarise, volume = sum(no.measurements))
   
  # Variety - number of measurement types
   variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
   
   # Final data frame
   so.bacteria <- plyr::join(volume,variety)
   
   # Veracity - number species with measurements for a given year
   so.bacteria$veracity <- 1
   so.bacteria$var <- "bacteria_brine"
                                                        
  
   
# bacteria_ice -----
   
  so.bact.ice <- read.table(file = "data_sets/GLOBEC/SouthOce/bacteria_ice/bacteria_ice.txt", fill = T, sep = "\t", header = TRUE, na.strings = "nd", stringsAsFactors = FALSE)
  so.bact.ice <- as.data.frame(so.bact.ice)
  
  vars <- c(2,13:15)
  bio <- so.bact.ice[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  factors <- bio[,c(1:4)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  bio <- factorsNumeric(factors)
  
  # get number of number of measurements collected for each species
  m.vars <- c(2:4)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  # total number of measurements
  volume <- plyr::ddply(.data = bio, .variables = "year", summarise, volume = sum(no.measurements))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  # Final data frame
  so.bacteria.ice <- plyr::join(volume,variety)
  
  # Veracity - number species with measurements for a given year
  so.bacteria.ice$veracity <- 1
  so.bacteria.ice$var <- "so.bacteria.ice"

                                                
# bacteria_wc ------
  
  so.bact.wc <- read.table(file = "data_sets/GLOBEC/SouthOce/bacteria_wc/bacteria.txt", fill = T, sep = "\t", header = TRUE, na.strings = "nd", stringsAsFactors = FALSE)
  so.bact.wc <- as.data.frame(so.bact.wc)
  
  vars <- c(2,10:12)
  bio <- so.bact.wc[vars]
  rm(so.bact.wc)
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  factors <- bio[,c(1:4)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  bio <- factorsNumeric(factors)
  
  # get number of number of measurements collected for each species
  m.vars <- c(2:4)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  # total number of measurements
  volume <- plyr::ddply(.data = bio, .variables = "year", summarise, volume = sum(no.measurements))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  # Final data frame
  so.bacteria_wc <- plyr::join(volume,variety)
  
  # Veracity - number species with measurements for a given year
  so.bacteria_wc$veracity <- 1
  so.bacteria_wc$var <- "bacteria_wc"


# bird --------
  
  so.birdo <- read.table(file = "data_sets/GLOBEC/SouthOce/bird/bird_observ.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.birdo <- as.data.frame(so.birdo)
  
  vars <- c(2,9,11:12)
  bio <- so.birdo[vars]
  rm(so.birdo)
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  species <- bio$species
  
  factors <- bio[,c(1,3:4)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  bio <- cbind(species, b)
  
  # get number of number of measurements collected for each species
  m.vars <- c(3:4)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  #get the total number of measurements made for each species for each year
  bio.cast <- dcast(data = bio, year ~ species, sum, value.var = "no.measurements")
  
  value_vars <- names(bio.cast[,c(2:length(bio.cast))])
  bio.m <- melt(data = bio.cast, id.vars = c("year"), measure.vars = value_vars, na.rm = T, 
                variable.name = "taxon", value.name = "total.no.measurements")
  
  # total number of measurements
  volume <- plyr::ddply(.data = bio, .variables = "year", summarise, volume = sum(no.measurements))
  
  # Veracity - number species with measurements for a given year
  s <- subset(bio.m, total.no.measurements > 0)
  veracity <- s %>% group_by(year) %>% summarise(veracity = length(taxon))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  # Final data frame
  vv <- plyr::join(volume,variety)
  so.bird <- plyr::join(vv,veracity)
  
  # Veracity - number species with measurements for a given year
  so.bird$var <- "so.bird"

                                                      
#chloro-ctd ------
  
  so.chlf.ctd <- read.table(file = "data_sets/GLOBEC/SouthOce/chloro_ctd/fullchlorodaly.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.chlf.ctd <- as.data.frame(so.chlf.ctd)
  
  vars <- c(2,11:12)
  bio <- so.chlf.ctd[vars]
  rm(so.chlf.ctd)
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  factors <- bio[,c(1:3)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  bio <- factorsNumeric(factors)
  
  # get number of number of measurements collected for each species
  m.vars <- c(2:3)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  # total number of measurements
  volume <- plyr::ddply(.data = bio, .variables = "year", summarise, volume = sum(no.measurements))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  # Final data frame
  so.chl.ctd <- plyr::join(volume,variety)
  
  so.chl.ctd$veracity <- 1
  
  # Veracity - number species with measurements for a given year
  so.chl.ctd$var <- "chl.ctd"


#chloro-full seawater -----
  
  so.chl.f <- read.table(file = "data_sets/GLOBEC/SouthOce/chloro_full/fullchloro.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.chl.f <- as.data.frame(so.chl.f)
  
  vars <- c(2,14:15)
  bio <- so.chl.f[vars]
  rm(so.chl.f)
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  factors <- bio[,c(1:3)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  bio <- factorsNumeric(factors)
  
  # get number of number of measurements collected for each species
  m.vars <- c(2:3)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  # total number of measurements
  volume <- plyr::ddply(.data = bio, .variables = "year", summarise, volume = sum(no.measurements))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  # Final data frame
  so.chl.full <- plyr::join(volume,variety)
  
  so.chl.full$veracity <- 1
  
  # Veracity - number species with measurements for a given year
  so.chl.full$var <- "chl.full"

                                                      
# chloro-underway -----
  
  so.chl.udw <- read.table(file = "data_sets/GLOBEC/SouthOce/chloro_udwy/uwchloro.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.chl.udw <- as.data.frame(so.chl.udw)
  
  vars <- c(2,6:7)
  bio <- so.chl.udw[vars]
  rm(so.chl.udw)
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  factors <- bio[,c(1:3)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  bio <- factorsNumeric(factors)
  
  # get number of number of measurements collected for each species
  m.vars <- c(2:3)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  # total number of measurements
  volume <- plyr::ddply(.data = bio, .variables = "year", summarise, volume = sum(no.measurements))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  # Final data frame
  so.chl.udwy <- plyr::join(volume,variety)
  
  so.chl.udwy$veracity <- 1
  
  # Veracity - number species with measurements for a given year
  so.chl.udwy$var <- "chl.udwy"

                                                      
# fish-moc10 ------
  
  so.fisha.moc <- read.table(file = "data_sets/GLOBEC/SouthOce/fish_moc10/fish_abund.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.fisha.moc <- as.data.frame(so.fisha.moc)
  
  vars <- c(2,18,22:24)
  bio <- so.fisha.moc[vars]
  rm(so.fisha.moc)
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  species <- bio$species
  
  factors <- bio[,c(1,3:5)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  bio <- cbind(species, b)
  
  # get number of number of measurements collected for each species
  m.vars <- c(3:5)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
    
  #get the total number of valid observations for each data species during each year
  bio.cast <- dcast(data = bio, year ~ species, sum, value.var = "no.measurements")
  
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
  
  # Final data frame
  vv <- plyr::join(volume,variety)
  so.fish.moc <- plyr::join(vv, veracity)
  
  # add data set name
  so.fish.moc$var <- "so.fish.moc"

 
# krill-abundance -----
 
  so.krilla <- read.table(file = "data_sets/GLOBEC/SouthOce/krill_abund/krill.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.krilla <- as.data.frame(so.krilla)
  
  vars <- c(2,19:20,22:32)
  bio <- so.krilla[vars]
  rm(so.krilla)
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  species <- bio$taxon
  stage <- bio$stage
  
  factors <- bio[,c(1,4:13)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  bio <- cbind(species,stage, b)
  
  # get number of number of measurements collected for each species
  m.vars <- c(2,4:13)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
    #get the total number of valid observations for each data species during each year
    bio.cast <- dcast(data = bio, year ~ species, sum, value.var = "no.measurements")
    
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
  
  # Final data frame
  vv <- plyr::join(volume,variety)
  so.krill <- plyr::join(vv, veracity)
  
  # add data set name
  so.krill$var <- "so.krill"
  

# krill-condition ------
  
  so.krill.cf <- read.table(file = "data_sets/GLOBEC/SouthOce/krill_larval_cf/larval_krill_cf.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.krill.cf <- as.data.frame(so.krill.cf)
  
  vars <- c(2,9:14)
  bio <- so.krill.cf[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  factors <- bio[,c(1:7)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  bio <- factorsNumeric(factors)
  
  # get number of number of measurements collected for each species
  m.vars <- c(2:7)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
    # total number of measurements
    volume <- bio %>% group_by(year) %>% summarise(volume = sum(no.measurements))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  # Final data frame
  so.krill.l.cf <- plyr::join(volume,variety)
  so.krill.l.cf$veracity <- 1
  so.krill.l.cf$var <- "so.krill.l.cf"


# krill-growth ----
  
  so.krill.gr <- read.table(file = "data_sets/GLOBEC/SouthOce/krill_larval_gr/larval_krill_gr.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.krill.gr <- as.data.frame(so.krill.gr)
  
  vars <- c(2,10:12)
  bio <- so.krill.gr[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  stage <- bio$stage
  
  factors <- bio[,c(1,3:4)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  bio <- cbind(b, stage)
  
  # get number of number of measurements collected for each species
  m.vars <- c(2:4)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  # total number of measurements
  volume <- bio %>% group_by(year) %>% summarise(volume = sum(no.measurements))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  # Final data frame
  so.krill.l.gr <- plyr::join(volume,variety)
  so.krill.l.gr$veracity <- 1
  so.krill.l.gr$var <- "so.krill.l.gr"
                           
                               
#krill-pigment -----
    
  so.krill.pig <- read.table(file = "data_sets/GLOBEC/SouthOce/krill_larval_pig/larval_krill_pig.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.krill.pig <- as.data.frame(so.krill.pig)
  
  vars <- c(2,11:13)
  bio <- so.krill.pig[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  factors <- bio[,c(1:4)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  bio <- factorsNumeric(factors)
  
  # get number of number of measurements collected for each species
  m.vars <- c(2:4)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  # total number of measurements
  volume <- bio %>% group_by(year) %>% summarise(volume = sum(no.measurements))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  # Final data frame
  so.krill.l.pig <- plyr::join(volume,variety)
  so.krill.l.pig$veracity <- 1
  so.krill.l.pig$var <- "so.krill.l.pig"

                                                      
# OPC -----
  
  so.opc <- read.table(file = "data_sets/GLOBEC/SouthOce/opc_moc/opc_moc1.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.opc <- as.data.frame(so.opc)
  
  vars <- c(2,12:13)
  bio <- so.opc[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  factors <- bio[,c(1:3)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  bio <- factorsNumeric(factors)
  
  # get number of number of measurements collected for each species
  m.vars <- c(2:3)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  # total number of measurements
  volume <- bio %>% group_by(year) %>% summarise(volume = sum(no.measurements))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  # Final data frame
  so.opc <- plyr::join(volume,variety)
  so.opc$veracity <- 1
  so.opc$var <- "so.opc"
  

                                                      
# plankton_biovol ----
  
  so.plankton <- read.table(file = "data_sets/GLOBEC/SouthOce/plankton_biovol/mocness_biovols_rs.txt", fill = T, sep = ",", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.plankton <- as.data.frame(so.plankton)
  
  vars <- c(2,20:25)
  bio <- so.plankton[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  factors <- bio[,c(1:7)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  bio <- factorsNumeric(factors)
  
  # get number of number of measurements collected for each species
  m.vars <- c(2:7)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  # total number of measurements
  volume <- bio %>% group_by(year) %>% summarise(volume = sum(no.measurements))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  # Final data frame
  so.plankton.biovol <- plyr::join(volume,variety)
  so.plankton.biovol$veracity <- 1
  so.plankton.biovol$var <- "plankton.biovol"

                                                      
# POC_PON ----
  
  so.fchn <- read.table(file = "data_sets/GLOBEC/SouthOce/poc_pon/fullchn.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.fchn <- as.data.frame(so.fchn)
  
  vars <- c(2,14:15)
  bio <- so.fchn[vars]
  rm(so.fchn)
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  factors <- bio[,c(1:3)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  bio <- factorsNumeric(factors)
  
  # get number of number of measurements collected for each species
  m.vars <- c(2:3)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  # total number of measurements
  volume <- bio %>% group_by(year) %>% summarise(volume = sum(no.measurements))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  # Final data frame
  so.chn <- plyr::join(volume,variety)
  so.chn$veracity <- 1
  so.chn$var <- "chn"

                                                      
# seal-location ----
  
  so.seals.loc <- read.table(file = "data_sets/GLOBEC/SouthOce/seal_loc/seals_loc_test.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.seals.loc <- as.data.frame(so.seals.loc)
  
  vars <- c(1,6) # using time_gmt as unique identifier for seal location
  bio <- so.seals.loc[vars]
  rm(so.seals.loc)
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  factors <- bio[,c(1:2)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  bio <- factorsNumeric(factors)
  
  # get number of number of measurements collected for each species
  m.vars <- c(2)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  # total number of measurements
  volume <- bio %>% group_by(year) %>% summarise(volume = sum(no.measurements))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  # Final data frame
  so.seal.loc <- plyr::join(volume,variety)
  so.seal.loc$veracity <- 1
  so.seal.loc$var <- "seal.loc"
  
                                                     
# seal-morph -----
  
  so.seal.m <- read.table(file = "data_sets/GLOBEC/SouthOce/seal_morph/seals_morphs.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.seal.m <- as.data.frame(so.seal.m)
  
  vars <- c(1,6,8:41)
  bio <- so.seal.m[vars]
  rm(so.seal.m)
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  species <- bio$species
  ageclass <- bio$ageclass
  sex <- bio$sex
  
  factors <- bio[,c(1,5:36)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  bio <- cbind(b, species, ageclass, sex)
  
  # get number of number of measurements collected for each species
  m.vars <- c(2:36)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  #get the total number of measurements made for each species for each year
  bio.cast <- dcast(data = bio, year ~ species, sum, value.var = "no.measurements")
  
  value_vars <- names(bio.cast[,c(2:length(bio.cast))])
  bio.m <- melt(data = bio.cast, id.vars = c("year"), measure.vars = value_vars, na.rm = T, 
                variable.name = "taxon", value.name = "total.no.measurements")
  
  # total number of measurements
  volume <- plyr::ddply(.data = bio.m, .variables = "year", summarise, volume = sum(total.no.measurements))
  
  # Veracity - number species with measurements for a given year
  s <- subset(bio.m, total.no.measurements > 0)
  veracity <- s %>% group_by(year) %>% summarise(veracity = length(taxon))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  #Final data frame
  vv <- plyr::join(volume,variety)
  so.seal.morph <- plyr::join(vv, veracity)
  so.seal.morph$var <- "seal.morph"


                                                      
# seal-phys -----

  so.seals.phys <- read.table(file = "data_sets/GLOBEC/SouthOce/seal_phys/seals_phys.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.seals.phys <- as.data.frame(so.seals.phys)
  
  vars <- c(1,5:15)
  bio <- so.seals.phys[vars]
  rm(so.seals.phys)
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  factors <- bio[,c(1:12)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  bio <- factorsNumeric(factors)
  
  # get number of number of measurements collected 
  m.vars <- c(2:12)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  # total number of measurements
  volume <- plyr::ddply(.data = bio, .variables = "year", summarise, volume = sum(no.measurements))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  #Final data frame
  so.seal.phys <- plyr::join(volume,variety)
  so.seal.phys$veracity <- 1
  so.seal.phys$var <- "seal.phys"
                                                     

  
# seal-predmass ----
  
  so.seal.pred <- read.table(file = "data_sets/GLOBEC/SouthOce/seal_predmass/seals_predmass.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.seal.pred <- as.data.frame(so.seal.pred)
  
  vars <- c(2,4:11)
  bio <- so.seal.pred[vars]
  rm(so.seal.pred)
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  ageclass <- bio$ageclass
  sex <- bio$sex
  age_by_mass <- bio$age_by_mass
  
  factors <- bio[,c(1,4:8)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  bio <- cbind(b, ageclass, sex, age_by_mass)
  
  # get number of number of measurements collected 
  m.vars <- c(2:9)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  # total number of measurements
  volume <- plyr::ddply(.data = bio, .variables = "year", summarise, volume = sum(no.measurements))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  #Final data frame
  so.seal.predmass <- plyr::join(volume,variety)
  so.seal.predmass$veracity <- 1
  so.seal.predmass$var <- "seal.predmass"
                                                        

# VPR ----
  
  so.vpr <- read.table(file = "data_sets/GLOBEC/SouthOce/vpr_ashjian_orig/vpr_cashjian.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.vpr <- as.data.frame(so.vpr)
  
  vars <- c(2,17:25)
  bio <- so.vpr[vars]
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  factors <- bio[,c(1:10)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  bio <- factorsNumeric(factors)
  
  # get number of number of measurements collected 
  m.vars <- c(2:10)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  # total number of measurements
  volume <- plyr::ddply(.data = bio, .variables = "year", summarise, volume = sum(no.measurements))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  #Final data frame
  so.vpr <- plyr::join(volume,variety)
  so.vpr$veracity <- 1
  so.vpr$var <- "vpr"

                                                      
  
# whale-acoustic ----
  
  sono.w <- read.table(file = "data_sets/GLOBEC/SouthOce/whale_acoustic/sonobuoy_whale.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  sono.w <- as.data.frame(sono.w)
  
  vars <- c(2,10:15,17)
  bio <- sono.w[vars]
  rm(sono.w)
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  Mn <- bio$Mn
  Bb <- bio$Bb
  Bp <- bio$Bp
  Bm <- bio$Bm
  Odt <- bio$Odt
  Seal <- bio$Seal
  
  factors <- bio[,c(1,8)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  bio <- cbind(b, Mn, Bb, Bp, Bm, Odt, Seal)
  
  # get number of number of measurements collected 
  m.vars <- c(2:8)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x){
        x <- x[!is.na(x)]
        x <- x[x != "-"]
        return(length(x))
  })
  
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  # total number of measurements
  volume <- plyr::ddply(.data = bio, .variables = "year", summarise, volume = sum(no.measurements))
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no.measurements")] %>% group_by(year) %>% summarise(variety = max(no.measurements))
  
  #Final data frame
  so.whale <- plyr::join(volume,variety)
  so.whale$veracity <- 1
  so.whale$var <- "whale"

                                                       
  
# zoolankton-abund-lmg ----
  
  so.zooa.lmg <- read.table(file = "data_sets/GLOBEC/SouthOce/zoo_abund_lmg/zooabund_lmg.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.zooa.lmg <- as.data.frame(so.zooa.lmg)
  
  vars <- c(2,17:21)
  bio <- so.zooa.lmg[vars]
  rm(so.zooa.lmg)
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  taxon <- bio$taxon
  stage <- bio$stage
  
  factors <- bio[,c(1,4:6)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  bio <- cbind(taxon,stage, b)
  
  # get number of number of measurements collected for each species
  m.vars <- c(2,4:6)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  #get the total number of valid observations for each data species during each year
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
  
  # Final data frame
  vv <- plyr::join(volume,variety)
  so.zoo.lmg <- plyr::join(vv, veracity)
  
  # add data set name
  so.zoo.lmg$var <- "zoo.lmg"

# zoolankton-abund-nbp -----
  
  so.zoo.np <- read.table(file = "data_sets/GLOBEC/SouthOce/zoo_abund_nbp/zooabund_nbp.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.zoo.np <- as.data.frame(so.zoo.np)
  
  vars <- c(2,19,21:23)
  bio <- so.zoo.np[vars]
  rm(so.zoo.np)
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  taxon <- bio$taxon
  stage <- bio$stage
  
  factors <- bio[,c(1:2,5)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  bio <- cbind(taxon,stage, b)
  
  # get number of number of measurements collected for each species
  m.vars <- c(2,4:5)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  #get the total number of valid observations for each data species during each year
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
  
  # Final data frame
  vv <- plyr::join(volume,variety)
  so.zoo.npb <- plyr::join(vv, veracity)
  # add data set name
  so.zoo.npb$var <- "zoo.npb"
                               
                           
  
  
# zoolankton-biomass ------
  
  so.zoo.bio <- read.table(file = "data_sets/GLOBEC/SouthOce/zoo_biomass/zoo_moc.txt", fill = T, sep = "\t", header = TRUE, quote = "", na.strings = "nd", stringsAsFactors = FALSE)
  so.zoo.bio <- as.data.frame(so.zoo.bio)
  
  vars <- c(2,11:15)
  bio <- so.zoo.bio[vars]
  rm(so.zoo.bio)
  
  #remove spaces in field values
  b <- apply(bio, MARGIN = 2, FUN = function(x) gsub(x = x, pattern = " *", replacement = ""))
  bio <- as.data.frame(b)
  
  #set nd to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x == "nd", NA, x))
  bio <- as.data.frame(b)
  
  #change factors to numbers
  taxon <- bio$taxon
  
  factors <- bio[,c(1:3,5:6)]
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(factors, lapply(factors[, sapply(factors, is.factor)], asNumeric))
  b <- factorsNumeric(factors)
  
  bio <- cbind(taxon,b)
  
  # get number of number of measurements collected for each species
  m.vars <- c(3:6)
  metrics <- bio[m.vars]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  colnames(mm) <- c("no.measurements")
  
  bio <- cbind(bio, mm)
  
  # Volume
  
  #get the total number of valid observations for each data species during each year
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
  
  # Final data frame
  vv <- plyr::join(volume,variety)
  so.zoo.biomass <- plyr::join(vv, veracity)
  so.zoo.biomass$var <- "zoo.biomass" # add data set name

                                                      
                                                        
  
# Combine into one data frame ----------
s <- rbind(so.bacteria, so.bacteria.ice, so.bacteria_wc, so.bird, so.chl.ctd, so.chl.full, so.chl.udwy, so.fish.moc, so.krill, so.krill.l.cf,
           so.krill.l.gr, so.krill.l.pig, so.opc, so.plankton.biovol, so.chn, so.seal.loc, so.seal.morph, so.seal.phys, so.seal.predmass,
           so.vpr, so.whale, so.zoo.lmg, so.zoo.npb, so.zoo.biomass)
#                                                       
#                                                         project.start <- min(s$year)
#                                                       project.end <- max(s$year)
#                                                       
#                                                         sm <- dcast(s, year ~ var, value.var = "n")
#                                                       
#                                                         # Variety (dimensions)
#                                                         #get number of unique sarameters for each year
#                                                         sv <- apply(sm, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
#                                                       svm <- melt(sv)
#                                                       colnames(svm) <- c("variety")
#                                                       svm$variety <- svm$variety - 1 #remove count of year as a biological dimension
#                                                       sm <- cbind(sm, svm)
#                                                       
#                                                         # Velocity
#                                                         #make NAs zeros
#                                                         sm$bacteria_brine <- ifelse(is.na(sm$bacteria_brine), 0, sm$bacteria_brine)
#                                                       sm$bacteria_ice <- ifelse(is.na(sm$bacteria_ice), 0, sm$bacteria_ice)
#                                                       sm$bacteria_wc <- ifelse(is.na(sm$bacteria_wc), 0, sm$bacteria_wc)
#                                                       sm$bird <- ifelse(is.na(sm$bird), 0, sm$bird)
#                                                       sm$chl.ctd <- ifelse(is.na(sm$chl.ctd), 0, sm$chl.ctd)
#                                                       sm$chl.full <- ifelse(is.na(sm$chl.full), 0, sm$chl.full)
#                                                       sm$chl.udwy <- ifelse(is.na(sm$chl.udwy), 0, sm$chl.udwy)
#                                                       sm$fish.moc <- ifelse(is.na(sm$fish.moc), 0, sm$fish.moc)
#                                                       sm$krill <- ifelse(is.na(sm$krill), 0, sm$krill)
#                                                       sm$krill.l.cf <- ifelse(is.na(sm$krill.l.cf), 0, sm$krill.l.cf)
#                                                       sm$krill.l.pig <- ifelse(is.na(sm$krill.l.pig), 0, sm$krill.l.pig)
#                                                       sm$opc <- ifelse(is.na(sm$opc), 0, sm$opc)
#                                                       sm$plankton.biovol <- ifelse(is.na(sm$plankton.biovol), 0, sm$plankton.biovol)
#                                                       sm$chn <- ifelse(is.na(sm$chn), 0, sm$chn)
#                                                       sm$seal.loc <- ifelse(is.na(sm$seal.loc), 0, sm$seal.loc)
#                                                       sm$seal.morph <- ifelse(is.na(sm$seal.morph), 0, sm$seal.morph)
#                                                       sm$seal.phys <- ifelse(is.na(sm$seal.phys), 0, sm$seal.phys)
#                                                       sm$seal.predmass <- ifelse(is.na(sm$seal.predmass), 0, sm$seal.predmass)
#                                                       sm$vpr <- ifelse(is.na(sm$vpr), 0, sm$vpr)
#                                                       sm$whale <- ifelse(is.na(sm$whale), 0, sm$whale)
#                                                       sm$zoo.lmg <- ifelse(is.na(sm$zoo.lmg), 0, sm$zoo.lmg)
#                                                       sm$zoo.np <- ifelse(is.na(sm$zoo.np), 0, sm$zoo.np)
#                                                       sm$zoo.biomass <- ifelse(is.na(sm$zoo.biomass), 0, sm$zoo.biomass)
#                                                       
#                                                         #get number of total observations per year
#                                                         sm$ann.volume <- sm$bacteria_brine + sm$bacteria_ice + sm$bacteria_wc + sm$bird + sm$chl.ctd + sm$chl.full + sm$chl.udwy + 
#                                                         +sm$fish.moc + sm$krill + sm$krill.l.cf + sm$krill.l.pig + sm$opc+sm$plankton.biovol + sm$chn + sm$seal.loc + 
#                                                         +sm$seal.morph + sm$seal.phys + sm$seal.predmass + sm$vpr + sm$whale + sm$zoo.lmg + sm$zoo.np + sm$zoo.biomass
#                                                       
#   
  
so <- s %>% group_by(year) %>% summarise(volume = sum(volume), veracity = sum(veracity), variety = sum(variety))                                   
  
so$big.data <- so$volume*so$variety*so$veracity
so$big.data.CumSum <- cumsum(so$big.data)
so$ann.volume.CumSum <- cumsum(so$volume)
so$project <- "Globec_SO"
save(so, file = "globec_SO.Robj")                                                     

                                                      
                                                       