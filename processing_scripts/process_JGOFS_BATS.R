
#PURPOSE: Pre-process JGOFS_BATS data for inclusion in 'volume variety' figure created using 'big_data_time.R'

#AUTHOR: Kelly Robinson
#CREATED: 2 February 2016
#LAST MODIFIED: 26 July 2016

library(stringr)
library(plyr)
library(dplyr)
library(reshape2)

#BATS - bottle data ------

#get headers
  bb.header <- read.table(file = "data_sets/JGOFS_BATS/bats_bottle.txt", sep = ",", skip = 58, nrows = 1, header = F, stringsAsFactors = FALSE)
  bb.header <- as.list(bb.header[1,])
  bb.header <- gsub(x = bb.header, pattern = " *", replacement = "")
  bb.header <- bb.header[1:(length(bb.header)-1)]
 
#read in data
  bats.bottle <- read.table(file = "data_sets/JGOFS_BATS/bats_bottle.txt", sep = ",", skip = 59, header = F,  stringsAsFactors = FALSE)
  pattern = "  *"
  t <- str_split_fixed(bats.bottle$V1, pattern = pattern, n = (length(bb.header)+1))
  t <- as.data.frame(t)
  t$V1 <- NULL
  
#assign headers
  colnames(t) <- bb.header
  
#get year
  t$decy <- as.numeric(levels(t$decy))[t$decy]
  t$year <- floor(x = t$decy)

#convert data to correct types
  t$POC <- as.numeric(levels(t$POC))[t$POC]
  t$PON <- as.numeric(levels(t$PON))[t$PON]
  t$TOC <- as.numeric(levels(t$TOC))[t$TOC]
  t$TON <- as.numeric(levels(t$TON))[t$TON]
  t$Bact <- as.numeric(levels(t$Bact))[t$Bact]
  t$POP <- as.numeric(levels(t$POP))[t$POP]
  t$BSi <- as.numeric(levels(t$BSi))[t$BSi]
  t$Pro <- as.numeric(levels(t$Pro))[t$Pro]
  t$Syn <- as.numeric(levels(t$Syn))[t$Syn]
  t$Piceu <- as.numeric(levels(t$Piceu))[t$Piceu]
  t$Naneu <- as.numeric(levels(t$Naneu))[t$Naneu]

##subset biological data fields
  vars <- c("POC", "PON", "TOC", "TON", "Bact", "POP", "BSi", "Pro", "Syn", "Piceu", "Naneu", "year")
  bio <- t[vars]

bio <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x < 0, NA, x)) #set negative values to NAs
bio <- as.data.frame(bio)

# Volume  
  
  # get number of measurements done for species, excluding NAs
  metrics <- bio[,c(1:11)]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  
  colnames(mm) <- c("no_measurements")
  
  bio <- cbind(bio, mm)
  
  #get the total number of measurements made for each species for each year
  volume <- bio %>% group_by(year) %>% summarise(volume = sum(no_measurements))
  
# 
# #melt to get the total number of valid observations for each data type
#   bio.vars <- c("POC", "PON", "TOC", "TON", "Bact", "POP", "BSi", "Pro", "Syn", "Piceu", "Naneu")
#   bio.b <- melt(data = bio, id.vars = "year", measure.vars = bio.vars, variable.name = "type")
#   bio.b$value <- as.numeric(bio.b$value)
  
# Veracity - number species with measurements for a given year 
  s <- bio[,c("Bact", "Pro", "Syn", "Piceu", "Naneu", "year")] 
  
  taxa <- s[,c(1:4)]
  t <- apply(taxa, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  tt <- melt(data = t)
  
  colnames(tt) <- c("no_taxa_measured")
  
  s <- cbind(s, tt)

  veracity <- s %>% group_by(year) %>% summarise(veracity = sum(no_taxa_measured))
  
  # Variety - number of measurement types made on taxa. These ares just abundance estiamtes so value assigned is '1'.
  variety <- 1
  
  #Final data frame
  # group by year
  bottle <- plyr::join(volume, veracity)
  bottle$variety <- variety
  
  #add data set name
  bottle$var <- "bottle"
  

#BATS - pigment ----
  
  #get headers
  bp.header <- read.table(file = "data_sets/JGOFS_BATS/bats_pigments.txt", sep = ",", skip = 50, nrows = 1, header = F, stringsAsFactors = FALSE)
  bp.header <- as.list(bp.header[1,])
  bp.header <- gsub(x = bp.header, pattern = " *", replacement = "")
  bp.header <- bp.header[1:(length(bp.header)-1)]
  
  #read in data
  bp <- read.table(file = "data_sets/JGOFS_BATS/bats_pigments.txt", sep = ",", skip = 51, header = F,  stringsAsFactors = FALSE)
  pattern = "  *"
  t <- str_split_fixed(bp$V1, pattern = pattern, n = (length(bp.header)+1))
  t <- as.data.frame(t)
  t$V1 <- NULL
  
  #assign headers
  colnames(t) <- bp.header
  
  #get year
  t$decy <- as.numeric(levels(t$decy))[t$decy]
  t$year <- floor(x = t$decy)
  
  #convert data to correct types
  t$p1 <- as.numeric(levels(t$p1))[t$p1]
  t$p2 <- as.numeric(levels(t$p2))[t$p2]
  t$p3 <- as.numeric(levels(t$p3))[t$p3]
  t$p4 <- as.numeric(levels(t$p4))[t$p4]
  t$p5 <- as.numeric(levels(t$p5))[t$p5]
  t$p6 <- as.numeric(levels(t$p6))[t$p6]
  t$p7 <- as.numeric(levels(t$p7))[t$p7]
  t$p8 <- as.numeric(levels(t$p8))[t$p8]
  t$p9 <- as.numeric(levels(t$p9))[t$p9]
  t$p10 <- as.numeric(levels(t$p10))[t$p10]
  t$p11 <- as.numeric(levels(t$p11))[t$p11]
  t$p12 <- as.numeric(levels(t$p12))[t$p12]
  t$p13 <- as.numeric(levels(t$p13))[t$p13]
  t$p14 <- as.numeric(levels(t$p14))[t$p14]
  t$p15 <- as.numeric(levels(t$p15))[t$p15]
  t$Chl <- as.numeric(levels(t$Chl))[t$Chl]
  t$Phae <- as.numeric(levels(t$Phae))[t$Phae]
  t$p18 <- as.numeric(levels(t$p18))[t$p18]
  t$p19 <- as.numeric(levels(t$p19))[t$p19]
  t$p20 <- as.numeric(levels(t$p20))[t$p20]
  t$p21 <- as.numeric(levels(t$p21))[t$p21]
  
  ##subset biological data fields
  vars <- c(bp.header[8:28], "year")
  bio <- t[vars]
  
  #set negative values to NAs
  bio <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x < 0, NA, x)) #set negative values to NAs
  bio <- as.data.frame(bio)
  
  
# Volume  
  
  # get number of measurements done for species, excluding NAs
  metrics <- bio[,c(1:20)]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  
  colnames(mm) <- c("no_measurements")
  
  bio <- cbind(bio, mm)
  
  #get the total number of measurements made for each species for each year
  volume <- bio %>% group_by(year) %>% summarise(volume = sum(no_measurements))
  
  
# Veracity - number species with measurements for a given year. Veracity = 1 because measureing phytoplankton pigments 
  veracity <- 1
  
  # Variety - number of measurement types made on taxa. These ares just abundance estiamtes so value assigned is '1'.
  variety <- 1
  
  #Final data frame
  # group by year
  pigment <- as.data.frame(volume)
  pigment$veracity <- veracity
  pigment$variety <- variety
  pigment$var <- "pigment"
  
  
#BATS - production ------
  
  #get headers
  bpro.header <- read.table(file = "data_sets/JGOFS_BATS/bats_production.txt", sep = ",", skip = 37, nrows = 1, header = F, stringsAsFactors = FALSE)
  bpro.header <- as.list(bpro.header[1,])
  bpro.header <- gsub(x = bpro.header, pattern = " *", replacement = "")
  bpro.header <- bpro.header[1:(length(bpro.header)-1)]
  
  #read in data
  bats.pro <- read.table(file = "data_sets/JGOFS_BATS/bats_production.txt", sep = ",", skip = 38, header = F,  stringsAsFactors = FALSE)
  pattern = "  *"
  t <- str_split_fixed(bats.pro$V1, pattern = pattern, n = (length(bpro.header)+1))
  t <- as.data.frame(t)
  t$V1 <- NULL
  
  #assign headers
  colnames(t) <- bpro.header
  
  #get year
  t$decy <- as.numeric(levels(t$decy))[t$decy]
  t$year <- floor(x = t$decy)
  
  #convert data to correct types
  t$lt1 <- as.numeric(levels(t$lt1))[t$lt1]
  t$lt2 <- as.numeric(levels(t$lt2))[t$lt2]
  t$lt3<- as.numeric(levels(t$lt3))[t$lt3]
  t$dark <- as.numeric(levels(t$dark))[t$dark]
  t$t0 <- as.numeric(levels(t$t0))[t$t0]
  t$pp <- as.numeric(levels(t$pp))[t$pp]
  t$thy1 <- as.numeric(levels(t$thy1))[t$thy1]
  t$thy2 <- as.numeric(levels(t$thy2))[t$thy2]
  t$thy3 <- as.numeric(levels(t$thy3))[t$thy3]
  t$thy <- as.numeric(levels(t$thy))[t$thy]
  
  ##subset biological data fields
  vars <- c(bpro.header[9:18], "year")
  bio <- t[vars]
  
  #set negative values to NAs
  bio <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x < 0, NA, x)) #set negative values to NAs
  bio <- as.data.frame(bio)
  
# Volume  
  
  # get number of measurements done for species, excluding NAs
  metrics <- bio[,c(1:10)]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  
  colnames(mm) <- c("no_measurements")
  
  bio <- cbind(bio, mm)
  
  #get the total number of measurements made for each year
  volume <- bio %>% group_by(year) %>% summarise(volume = sum(no_measurements))
  
  
  # Veracity - number species with measurements for a given year. Veracity = 1 because measureing bacteria production 
  veracity <- 1
  
  # Variety - number of measurement types made on taxa..
  variety <- 10
  
  #Final data frame
  # group by year
  bact.prod <- as.data.frame(volume)
  bact.prod$veracity <- veracity
  bact.prod$variety <- variety
  bact.prod$var <- "bact.prod"
  

#BATS - zooplankton ------
    
  #get headers
  bzoo.header <- read.csv(file = "data_sets/JGOFS_BATS/bats_zoo_header_translator.csv", header = T, stringsAsFactors = FALSE)
  bzoo.header <- as.list(bzoo.header[,3])
  
  #read in data
  bats.zoo <- read.table(file = "data_sets/JGOFS_BATS/bats_zooplankton.txt", sep = "\t", skip = 36, header = F,  stringsAsFactors = FALSE)
  t <- as.data.frame(bats.zoo)
  
  #assign headers
  colnames(t) <- bzoo.header
  
  #get year
  t$year <- as.numeric(str_sub(t$date, 1, 4))
  
  ##subset biological data fields
  vars <- c(bzoo.header[14:23], "year")
  vars <- unlist(vars)
  bio <- t[vars]
  
  #set nd to NAs
  bio <- apply(bio, MARGIN = 2, FUN = function(x) ifelse(x < 0, NA, x)) #set negative values to NAs
  bio <- as.data.frame(bio)
  
#Volume  
  
  # get number of measurements done for species, excluding NAs
  metrics <- bio[,c(1:10)]
  m <- apply(metrics, MARGIN = 1, FUN = function(x) length(x[!is.na(x)]))
  mm <- melt(data = m)
  
  colnames(mm) <- c("no_measurements")
  
  bio <- cbind(bio, mm)
  
  # total number of measurements
  volume <- bio %>% group_by(year) %>% summarise(volume = sum(no_measurements))
  
  # Veracity - number species with measurements for a given year 
  veracity <- 1 #all measurements are for zooplankton
  
  # Variety - number of measurement types
  variety <-  bio[,c("year","no_measurements")] %>% group_by(year) %>% summarise(variety = max(no_measurements))
  
#Final data frame
  # group by year
  zoopl <- plyr::join(volume, variety)
  zoopl$veracity <- veracity
  zoopl$var <- "zoopl"
  

# COLLATE DATA SETS ----
  bats <- rbind(bottle, pigment, bact.prod, zoopl)
  
  bats <- bats %>% group_by(year) %>% summarise(volume = sum(volume), veracity = sum(veracity), variety = sum(variety))        

  #calculate big data
  bats$volume<-as.numeric(bats$volume)
  bats$big.data <- bats$volume*bats$variety*bats$veracity
  bats$big.data.CumSum <- cumsum(bats$big.data)
  bats$ann.volume.CumSum <- cumsum(bats$volume)
  bats$project <- "BATS"
  
#save frame as R files
  save(bats, file = "bats_data.Robj")
  