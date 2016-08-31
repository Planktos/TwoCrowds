#PURPOSE: Pre-process JGOFS_HOTS data for inclusion in 'volume variety' figure created using 'big_data_time.R'

#AUTHOR: Kelly Robinson
#CREATED: 2 February 2016
#LAST MODIFIED: 26 July 2016

library(stringr)
library(plyr)
library(dplyr)
library(reshape2)

#HOTS - bottle data ------

  #read in data
  hb <- read.table(file = "data_sets/JGOFS_HOTS/bottle_data.txt", sep = ",", header = T, stringsAsFactors = FALSE)
  hb <- as.data.frame(hb)
  hb$X <- NULL
  
  #get year
  hb$date <- as.character(hb$date)
  hb$year <- as.numeric(str_sub(hb$date, -2, -1))
  hb$year <- ifelse(hb$year<0,NA,hb$year)
  hb$year <- hb$year + 1900
  
  ##subset biological data fields
  vars <- names(hb)[5:39]
  bio <- hb[vars]
  
  #set negative values to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x<0, NA, x)) #change -9 no data values to NAs
  bio <- as.data.frame(b)
  
  
#Volume  
  
  # get number of measurements done for species, excluding NAs
  metrics <- bio[,c(3:34)]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  
  colnames(mm) <- c("no_measurements")
  
  bio <- cbind(bio, mm)
  
  #get the total number of measurements made for each species for each year
  volume <- bio %>% group_by(year) %>% summarise(volume = sum(no_measurements))
  
# Veracity - number species with measurements for a given year 
  s <- bio[,c(3,26:29,35)] #bacteria species
  
  taxa <- s[,c(1:5)]
  t <- apply(taxa, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  tt <- melt(data = t)
  
  colnames(tt) <- c("no_taxa_measured")
  
  s <- cbind(s, tt)
  
  veracity <- s %>% group_by(year) %>% summarise(veracity = sum(no_taxa_measured))
  
# Variety - number of measurement types made on taxa. These ares just abundance estiamtes so value assigned is '1'.
  var <- bio[,c(4:25,30:35)]
  
  type <- var[,c(1:27)]
  t <- apply(type, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  types <- melt(data = t)
  
  colnames(types) <- c("no_types_measured")
  
  var <- cbind(var, types)
  
  variety <- var %>% group_by(year) %>% summarise(variety = max(no_types_measured))
  
  #Final data frame
  bottle <- plyr::join(volume, veracity)
  bottle <- plyr::join(bottle, variety)
  bottle$var <- "bottle"
  
 
#HOTS - macrozoo data ------
  
  #read in data
  hz <- read.table(file = "data_sets/JGOFS_HOTS/macrozoo.txt", sep = ",", header = T, stringsAsFactors = FALSE)
  hz <- as.data.frame(hz)
  hz$X <- NULL
  
  #get year
  hz$date <- as.character(hz$date)
  hz$year <- as.numeric(str_sub(hz$date, -2, -1))
  hz$year <- ifelse(hz$year<0,NA,hz$year)
  hz$year <- ifelse(hz$year>16,hz$year+1900,hz$year+2000)
  
  #subset biological data fields
  vars <- names(hz)[7:14]
  bio <- hz[vars]
  
  #set negative values to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x<0, NA, x)) #change -9 no data values to NAs
  bio <- as.data.frame(b)
  
  
# Volume  
  
  # get number of measurements done for species, excluding NAs
  metrics <- bio[,c(1:7)]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  
  colnames(mm) <- c("no_measurements")
  
  bio <- cbind(bio, mm)
  
  #get the total number of measurements made for each species for each year
  volume <- bio %>% group_by(year) %>% summarise(volume = sum(no_measurements))
  
# Veracity - number species with measurements for a given year 
  veracity <- 1
  
# Variety - number of measurement types made on taxa. These ares just abundance estiamtes so value assigned is '1'.
  var <- bio[,c(1:8)]
  type <- var[,c(1:7)]
  t <- apply(type, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  types <- melt(data = t)
  
  colnames(types) <- c("no_types_measured")
  
  var <- cbind(var, types)
  
  variety <- var %>% group_by(year) %>% summarise(variety = max(no_types_measured))
  
  #Final data frame
  zoopl <- plyr::join(volume, variety)
  zoopl$veracity <- veracity
  zoopl$var <- "zoopl"
  
  
#HOTS - primary production ------
  
  #read in data
  hp <- read.table(file = "data_sets/JGOFS_HOTS/primary_prod.txt", sep = ",", header = T, colClasses = c(date = "character"), stringsAsFactors = FALSE)
  hp <- as.data.frame(hp)
  hp$X <- NULL
  
  #get year
  date <- gsub(x = hp$date, pattern = " *", replacement = "")
  hp$year <- as.numeric(str_sub(date, 1, 2))
  hp$year <- ifelse(hp$year<0,NA,hp$year)
  hp$year <- ifelse(hp$year>16,hp$year+1900,hp$year+2000)
  
  #subset biological data fields
  vars <- names(hp)[7:13]
  bio <- hp[vars]
  
  #set negative values to NAs
  b <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x<0, NA, x)) #change -9 no data values to NAs
  bio <- as.data.frame(b)
  
# Volume  
  
  # get number of measurements done for species, excluding NAs
  metrics <- bio[,c(1:6)]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  
  colnames(mm) <- c("no_measurements")
  
  bio <- cbind(bio, mm)
  
  #get the total number of measurements made for each species for each year
  volume <- bio %>% group_by(year) %>% summarise(volume = sum(no_measurements))
  
  value_vars <- names(bio[, c(1,3:6)])
  bio.m <- melt(data = bio, id.vars = c("year"), measure.vars = value_vars, na.rm = T, variable.name = "taxon")
  
  bio.m$taxa_valid <- ifelse(is.na(bio.m$value), 0, 1)
  
# Veracity - number species with measurements for a given year 
  #get the total number of measurements made for each species for each year
  by.taxon.volume <- bio.m %>% group_by(year,taxon) %>% summarise(total.no.measurements = sum(taxa_valid))
  s <- subset(by.taxon.volume, total.no.measurements > 0)
  veracity <- s %>% group_by(year) %>% summarise(veracity = length(taxon))
  
  
# Variety - number of measurement types made on taxa. These ares just abundance estiamtes so value assigned is '1'.
  variety <- bio %>% group_by(year) %>% summarise(variety = max(no_measurements))
  
  #Final data frame
  primary <- plyr::join(volume, veracity)
  primary <- plyr::join(primary, variety)
  primary$var <- "primary.prod"
  
 
#COLLATE data sets ----

  #rbind together
  all.bio <- rbind(bottle, zoopl, primary)
  
  hots <- all.bio %>% group_by(year) %>% summarise(volume = sum(volume), veracity = sum(veracity), variety = sum(variety))                                   
  
  hots$big.data <- hots$volume*hots$variety*hots$veracity
  hots$big.data.CumSum <- cumsum(hots$big.data)
  hots$ann.volume.CumSum <- cumsum(hots$volume)
  hots$project <- "HOTS"
  save(hots, file = "JGOFS_HOTS.Robj")                                                     
  
