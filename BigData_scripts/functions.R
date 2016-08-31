# functions

## Detect up and down casts in a depth yo
detect.casts <- function(depth, order=200) {
  # smoothing the depth profile using a moving average and find the turning points
  
  # smooth depths
  library("pastecs")
  depth_avg <- decaverage(-depth, times=3, weights=c(seq(1, order), order+1, seq(order, 1, -1)))
  # plot(depth_avg)
  depth_avg <- as.numeric(pastecs::extract(depth_avg, component="filtered"))
  
  # detect turning points
  TP <- suppressWarnings(turnpoints(depth_avg))
  
  # set cast numbers (different for up and down casts)
  cast <- cumsum(TP$peaks | TP$pits) + 1
  
  # detect which are up and which are down casts:
  # if the first turning point is a peak, then the first cast (and all odd casts) are upcasts
  if ( TP$firstispeak ) {
    # these are the types for
    #              even  & odd   cast numbers
    castTypes <- c("down", "up")
  } else {
    castTypes <- c("up", "down")
  }
  down.up <- castTypes[cast %% 2 + 1]
  
  return(data.frame(cast, down.up))
}

  # smoothing the depth profile using a moving average and find the turning points

# Interpolate a slice of data for which the x-axis is a distance in nautical miles
interp.dist <- function(x, y, z, anisotropy=1000, x.step=500, y.step=2.5, smooth=FALSE, theta=0.2, ...) {
  #
  # Interpolate data over a distance coordinate
  #
  # x   vector of distance *IN KILOMETERS*
  # y   vector of depth in m
  # z   vector of measured variable
  # anisotropy  anisotropy ratio between x and y
  # x/y.step    interpolation grid steps in m
  # smooth      boolean, wether to smooth the first interpolation using fields::image.smooth
  # x/y.step.smooth   interpolation grid step for the smoothing
  # grid.smooth intepolation grid for the smoothing, overrides x/y.step.smooth
  # theta       bandwidth for the kernel smoother in fields::image.smooth
  
  library("akima")
  library("reshape2")
  
  # correct x-axis for anisotropy between horizontal and vertical
  x <- x <- x*1852/anisotropy #if x unit is nautical miles
  
  # interpolate
  i <- interp(x=x, y=y, z=z, xo=seq(0, max(x), by=x.step/anisotropy), yo=seq(0, max(y), by=y.step), ...)
  
  # smooth
  if ( smooth ) {
    library("fields")
    i <- image.smooth(i, grid=list(x=i$x, y=i$y), theta=theta)
  }
  
  # extract a data.frame
  out <- melt(i$z, varnames=c("x","y"))
  out$x <- i$x[out$x] * anisotropy/1852
  out$y <- i$y[out$y]
  
  return(out)
}


# Compute the straight line distance (km) from the starting point of a lat,lon trajectory
dist.from.start.nm <- function(lat, lon) {
  library("oce")
  geodDist(lat1=lat, lon1=lon, lat2=na.omit(lat)[1], lon2=na.omit(lon)[1])
}

# Compute the straight line distance (km) from the starting point of a lat,lon trajectory
dist.from.start.km <- function(lat, lon) {
  library("oce")
  geodDist(lat1=lat, lon1=lon, lat2=na.omit(lat)[1], lon2=na.omit(lon)[1])/1.852
}

# Compute the distance from rsmas
dist.from.rsmas <- function(lat, lon) {
  # TODO should compensate for the length of cable put out and the angle of the cable (i.e. depth of ISIIS)
  library("oce")
  geodDist(lat, lon, lat2=25.731384, lon2=-80.1621017) / 1.852
}

# function reformat the lat and long in decimal degrees
to.dec.gps <- function(y) {
  # split in degree, minute, second
  pieces <- str_split_fixed(x, "° |'", 3)
  # extract orientation (S/N and E/W)
  orientation <- str_sub(pieces[,3], -1)
  # remove orientation to only keep numbers
  pieces[,3] <- str_replace(pieces[,3], "[NSEW]", "")
  # convert to decimal degrees
  dec <- as.numeric(pieces[,1]) + as.numeric(pieces[,2]) / 60 + as.numeric(pieces[,3]) / 3600
  # orient the coordinate
  ifelse(orientation %in% c("S", "W"), -dec, dec)
  
  return(dec)
}

# KR: Modified from "to.dec.gps" because JL original wasn't quite working with structure of 2014 physical data files. This may not be robust for data
# collected in regions with single digit lat and longitude coordinates
to.dec.2014 <- function(x) {
  # split in degree, minute, second
  #pieces <- str_split_fixed(x, "°|'",2) #Can't make R split at the degree symbol
  deg <- substr(x, 1, 2)
  min <- substr(x, 5, 6)
  sec <- substr(x, 8, 12)
  # extract orientation (S/N and E/W)
  orientation <- substr(x, 13,13) #Changed from pieces[,3] to sec
  # remove orientation to only keep numbers
  #pieces[,2] <- str_replace(pieces[,3], pattern = "[NSEW]", replacement = "")
  # convert to decimal degrees
  dec <- as.numeric(deg) + as.numeric(min) / 60 + as.numeric(sec) / 3600 #replaced 'pieces' with 'deg', 'min', and 'sec'
  # orient the coordinate
  ifelse(orientation %in% c("S", "W"), -dec, dec)
  
  return(dec)
}

to.dec.2015 <- function(x) {
  # split in degree, minute, second
  pieces <- str_split_fixed(x, "°|'| ",3) #Can't make R split at the degree symbol
  deg <- substr(pieces[,1], 1, 2)
  min <- pieces[,2]
  # extract orientation (S/N and E/W)
  orientation <-substrRight(pieces[,3], 1)
  # remove orientation to only keep numbers
  sec <- str_replace(pieces[,3], pattern = "[NSEW]", replacement = "")
  # convert to decimal degrees
  dec <- as.numeric(deg) + as.numeric(min) / 60 + as.numeric(sec) / 3600 #replaced 'pieces' with 'deg', 'min', and 'sec'
  # orient the coordinate
  ifelse(orientation %in% c("S", "W"), -dec, dec)
  
  return(dec)
}

read.gps <- function(file) {
  library(stringr)
  
  options(digits.secs=3)  # allow split seconds
  
  # read table
  t <- read.table(file, header=F, skip=2, sep=",")
  # split the first column
  tmp <- colsplit(t$V1, pattern="\t", names=c("date", "time", "model"))
  # bind together
  t <- cbind(tmp, t[2:ncol(t)])
  t <- t[,names(t) %in% c("date", "time", "V3", "V4", "V5", "V6")]
  
  names(t) <- c("date", "time", "lat", "N-S", "long", "E-W")
  
  t$latdeg <- str_sub(t$lat,1,2)
  t$latminsec <- str_sub(t$lat,3,-1)
  t$londeg <- str_sub(t$long,1,2)
  t$lonminsec <- str_sub(t$long,3,-1)
  
  # convert to degrees, take into account the N/S/E/W orientation
  t$lat <- as.numeric(t$latdeg) + as.numeric(t$latminsec)/60
  t$lat <- ifelse(t$"N-S" == "S", -t$lat, t$lat)
  t$long <- as.numeric(t$londeg) + as.numeric(t$lonminsec)/60
  t$long <- ifelse(t$"E-W" == "W", -t$long, t$long)
  
  # combine date and time and convert dateTime to POSIX
  t$dateTime <- str_c(t$date, " ",t$time)
  
  t$dateTime <- as.POSIXct(t$dateTime, format="%m/%d/%Y %H:%M:%OS", tz="America/New_York")
  
  # convert from GMT to local time
  t$dateTime <- t$dateTime - 4*3600
  # keep only relevant columns
  t <- t[,names(t) %in% c("dateTime", "lat", "long")]
  
  return(t)  
  
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

read.isiis <- function(file) {
  library("stringr")
  
  options(digits.secs=3)  # allow split seconds #changed from 2 to 3
  
  # read the data
  d <- read.delim(file, skip=10, fileEncoding="ISO-8859-1", encoding="UTF-8", stringsAsFactors=FALSE)
  
  # clean names
  # remove double dots
  names(d) <- str_replace_all(names(d), fixed(".."), ".")
  names(d) <- str_replace_all(names(d), fixed(".."), ".")
  # remove dots at end of names
  names(d) <- str_replace(names(d), "\\.$", "")
  
  # extract date from file name
  year <- str_sub(file, -16, -13)
  month <- str_sub(file, -12, -11)
  day <- str_sub(file, -10, -9)
  
  # compute date and time
  d$dateTimeMsec <- as.POSIXct(str_c(year, "-", month, "-", day, " ", d$Time), tz="America/New_York")
  # detect midnight shifts
  midnightShift <- which(diff(d$dateTimeMsec) < 0)
  if (length(midnightShift) > 0) {
    d$dateTimeMsec[midnightShift:nrow(d)] <- d$dateTimeMsec[midnightShift:nrow(d)] + 24 * 3600
  }
  
  # keep important columns
  d <- d[,c("dateTimeMsec", "Pressure.dbar", "Depth.m", "Temp.C", "Salinity.PPT", "Fluoro.volts", "Oxygen.ml.l", "Irrandiance.UE.cm", "Lat.decimals", "Long.decimals")]
  
  return(d)
} #original "lib_process" code from Jessica Luo to process ISIIS physical data

oxy.recalib <- function(SBE43.serial, calib.date, oxygen, press, temp, sal){
    # Eqaution to convert SBE43 oxygen sensor voltage to dissolved oxygen (ml/l) may be incorrect in ISIIS software for ISIIS v1, v2, and v3. This function back-calculates
    # the raw voltage from the incorrect equation and then derives the correct concentration values using the correct equation.
    
    #1) get SBE43 coefficents for the input calibration date
    oc <- read.csv("oxygen.calibration.tests/SBE43_calibration_coefficients.csv",stringsAsFactors = F)
    od <- oc[oc$calib.date == calib.date & oc$serial == SBE43.serial,]
    od <- od[,c("coefficient", "type", "value")]
      
    os <- read.csv("oxygen.calibration.tests/oxygen_solubility_coefficients.csv",stringsAsFactors = F)
    os <- os[,c("coefficient", "type", "value")]
    
    c <- rbind(od, os)
    
    library(reshape2)
    dc <- dcast(c, type ~ coefficient)
    
    #2) calculate "Ts" variable
    temp.K <- temp + dc$degC.To.Kelvin
    Ts <- log((298.15-temp)/(temp.K))
    
    #3) calculate oxygen saturation [ml/l]
    OxySol <- exp(dc$A0 + (dc$A1*Ts) + (dc$A2*Ts^2) + (dc$A3*Ts^3) + (dc$A4*Ts^4) + (dc$A5*Ts^5) + sal*((dc$B0 + (dc$B1*Ts)) + (dc$B2*Ts^2) + (dc$B3*Ts^3)) + (dc$C0*sal^2))
    
    #4) back-calculate oxygen sensor raw voltage using equation from the ISIIS software
    total.volt <- oxygen/(dc$Soc*(1.0 + dc$A*temp + dc$B*temp^2 + dc$C*temp^3)*OxySol*exp(dc$E.nominal*press/temp.K))
    
    oxy.volts <- total.volt - dc$Voffset
    
    #5 re-calculate correct oxygen concentrations (ml/l) using correct equation (from Charles Cousin Bellamare, Inc. via Adam Greer).
    oxygen.ml.l <- (dc$Soc*(oxy.volts + dc$Voffset))*OxySol*(1.0 + dc$A*temp + dc$B*temp^2 + dc$C*temp^3)*exp(dc$E.nominal*press/temp.K)
    
    oxy.data <- data.frame(oxy.volts, oxygen.ml.l)
    
    return(oxy.data)
}

# Bin Data function

# Original 
  # bin.data <- function(x){ ----
#   
#   d <- x
#   
#   # bin taxon counts into 0.5-m3 bins using time it takes to image 1-m3 (4.6 sec)
#   #seconds it took to image 0.5-m3 of water ('see 'lib_process.R" scripts for calculations of 'm3.sec' field)
#   
#   binSize <-  ((1/mean(d$m3.sec, na.rm = TRUE))*bin.size)
#   
#   #calculate the number of bins
#   d.maxT <- max(d$dateTime, na.rm=T)
#   d.minT <- min(d$dateTime, na.rm=T)
#   d.bins=seq(d.minT, d.maxT, by=binSize)
#   d$timeBin <- cut(d$dateTime, breaks=d.bins, labels=1:(length(d.bins)-1))
#   
#   # convert from factor to numeric
#   d$timeBin <- as.numeric(d$timeBin)
#   
#   # binning
#   # find total counts of each taxa in a given time bin, which represents the number of seconds it took to image 1 m3 of water.
#   d.count.bin <- aggregate(count~timeBin+group, data = d, FUN = sum)
#   
#   # bin the dateTimes for each time bin & taxon combination
#   d.timeBin <- aggregate(dateTime~timeBin+group, data = d, FUN = mean)
#   
#   # bin the physical data for each time bin & taxon combination
#   if(region == "Eddy"){
#     d.phyBin <- aggregate(cbind(depth,temp,salinity,pressure,fluoro,chla.ug.l,oxygen,lat,lon,sw.density)~timeBin+group, data = d, FUN = mean)
#   } else {
#     d.phyBin <- aggregate(cbind(depth,temp,salinity,pressure,fluoro,chla.ug.l,irradiance,oxygen,lat,lon,sw.density)~timeBin+group, data = d, FUN = mean)
#   }
#   # cHECK for time off-set in d.timeBin against minimum and maximum times in 'bio'
#   #       summary(d.timeBin)
#   #       summary(bio.f)
#   
#   # OPTIONAL: fix the time offset if present (e.g. if a shift from 11:26:00 to 8:26:00 happened)
#   d.timeBin$dateTime <- d.timeBin$dateTime + 3*3600
#   #check again to make sure the dateTime was corrected
#   # summary(d.timeBin)
#   
#   #merge all three together (each should have the exact same number of rows)
#   d.taxa <- merge(d.count.bin, d.timeBin, by = c("timeBin", "group"))
#   d.taxa <- merge(d.taxa, d.phyBin, by = c("timeBin", "group"))
#   
#   #rename 'count' field to density.m3 because now it represents the total number of organisms in 1 m3.
#   names(d.taxa)[names(d.taxa)=="count"] <- (str_c("indiv.per.", bin.size, "m3", sep = ""))
#   
#   #order by time bin
#   binned_biophy <- d.taxa[order(d.taxa$timeBin),]
#   
#   return(binned_biophy)
#   
# }

# Revised 2016-07-19 KR ----
  bin.data <- function(x){
    
    d <- x
    
    # bin taxon counts into 0.5-m3 bins using time it takes to image 1-m3 (4.6 sec)
    #seconds it took to image 0.5-m3 of water ('see 'lib_process.R" scripts for calculations of 'm3.sec' field)
    if(camera == "lg_camera"){
      binSize <-  ((1/mean(d$lg.vol.rate, na.rm = TRUE))*bin.size)
    }else{
      binSize <- ((1/mean(d$sm.vol.rate,na.rm = TRUE))*bin.size)
    }
    
    #calculate the number of bins
    d.maxT <- max(d$dateTime, na.rm=T)
    d.minT <- min(d$dateTime, na.rm=T)
    d.bins=seq(d.minT, d.maxT, by=binSize)
    d$timeBin <- cut(d$dateTime, breaks=d.bins, labels=1:(length(d.bins)-1))
    
    # convert from factor to numeric
    d$timeBin <- as.numeric(d$timeBin)
    
    # binning
    # find total counts of each taxa in a given time bin, which represents the number of seconds it took to image 1 m3 of water.
    d.count.bin <- aggregate(count~timeBin+group, data = d, FUN = sum)
    
    # bin the dateTimes for each time bin & taxon combination
    d.timeBin <- aggregate(dateTime~timeBin+group, data = d, FUN = mean)
    
    # bin the physical data for each time bin & taxon combination
    if(proj.year == "OST14" & region == "Eddy"){
      d.phyBin <- aggregate(cbind(lat,lon,depth,temp,salinity,pressure,fluoro,chl.ug.l,sw.density)~timeBin+group, data = d, FUN = mean)
      
    } else if (proj.year == "OST14" & region != "Eddy"){
      d.phyBin <- aggregate(cbind(lat,lon,depth,temp,salinity,pressure,fluoro,chl.ug.l,irradiance,sw.density)~timeBin+group, data = d, FUN = mean)
      
    } else {
      d.phyBin <- aggregate(cbind(lat,lon,depth,temp,salinity,pressure,fluoro,chl.ug.l,oxgyen,irradiance,sw.density)~timeBin+group, data = d, FUN = mean)
    }
    
    # cHECK for time off-set in d.timeBin against minimum and maximum times in 'bio'
    #       summary(d.timeBin)
    #       summary(bio.f)
    
    # OPTIONAL: fix the time offset if present (e.g. if a shift from 11:26:00 to 8:26:00 happened)
    d.timeBin$dateTime <- d.timeBin$dateTime + 3*3600
    #check again to make sure the dateTime was corrected
    # summary(d.timeBin)
    
    #merge all three together (each should have the exact same number of rows)
    d.taxa <- merge(d.count.bin, d.timeBin, by = c("timeBin", "group"))
    d.taxa <- merge(d.taxa, d.phyBin, by = c("timeBin", "group"))
    
    #rename 'count' field to density.m3 because now it represents the total number of organisms in 1 m3.
    names(d.taxa)[names(d.taxa)=="count"] <- (str_c("indiv.per.", bin.size, "m3", sep = ""))
    
    #order by time bin
    binned_biophy <- d.taxa[order(d.taxa$timeBin),]
    
    return(binned_biophy)
    
  } 

# Spectral colour map from ColorBrewer
  spectral <- function(n=6) {
    library("RColorBrewer")
    rev(brewer.pal(name="Spectral", n=n))
  }
  
  scale_fill_spectral <- function(...) {
    scale_fill_gradientn(colours=spectral(...))
  }
  scale_colour_spectral <- function(...) {
    scale_colour_gradientn(colours=spectral(...))
  }

#get legend
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
    if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
  }