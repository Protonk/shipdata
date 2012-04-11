
buildGuide <- function() {
  # Table of variables and their meaning
  # Used to determine column width and header
  navguide <-htmlParse("http://www.ucm.es/info/cliwoc/content/CLIWOC15corelimit.htm")
  navtables <- readHTMLTable(navguide)
  # This version has only one table but we subset the list to get the dataframe
  format <- navtables[[1]][-1, 1:3]
  # coerce everything to character
  as.data.frame(matrix(as.character(unlist(format, recursive = FALSE, use.names = FALSE)), dim(format)), stringsAsFactors = FALSE)
}


buildNavdf <- function(ingest) {
  # From http://www.ucm.es/info/cliwoc/cliwoc15.htm
  nav.limit <- 245195
  # Location and name for data
  core.url <- "http://www.knmi.nl/cliwoc/download/CLIWOC15corelimit.zip"
  core.filename <- "CLIWOC15corelimit.txt"
  grabRemote <- function(create.dir = FALSE) {
    format.ref <- buildGuide()
    temp <- tempfile()
    download.file(core.url, temp)
    navin <- read.fwf(unz(temp, filename = core.filename),
                      # format page has start/stop numbers
                      # We need to convert these to widths
                      widths = c(diff(as.numeric(format.ref[,2])), 2),
                      col.names = format.ref[,1],
                      stringsAsFactors = FALSE,
                      n = nav.limit)
    unlink(temp)
    if (create.dir) {
      if(!file.exists(file.path(getwd(), "Data"))) dir.create(file.path(getwd(), "Data"))
      write.csv(navin, file.path(getwd(), "Data",  "corelimit.csv"))
    }
    return(navin)
  }
  switch(ingest,
         local = read.csv(file.path(getwd(), "Data",  "corelimit.csv"), stringsAsFactors = FALSE, nrows = nav.limit),
         remote = grabRemote(),
         initial = grabRemote(create.dir = TRUE)
         )
}

# remote grabs the data and doesn't save it locally
# local assumes you've saved the data and grabs it from the disk
# initial gets the data from the website and saves a cleaned csv on disk
# for now, all of them rely on the logbook being local (which is in the repo)
navpre <- buildNavdf(ingest = "local")

# Drops all but date/time, lat/long, ship ID, country and wind direction
# May drop wind direction later but it might be fun to plot
navpre <- navpre[, names(navpre) %in% c("YR", "MO", "DY", "HR", "LAT", "LON", "ID", "C1", "D")]

# About 5 seconds faster than paste(). 
navpre[, "Date"] <- as.Date(sprintf("%4.f-%02.f-%2.f", navpre[,"YR"], navpre[,"MO"], navpre[,"DY"]), format = "%Y-%m-%d")

# Build Factor variables and convert lat/long
navpre[, "Country"] <- as.factor(navpre[, "C1"])
navpre[, "Month"] <- factor(month.name[navpre[, "MO"]], levels = month.name, ordered = FALSE)
navpre[, "Lat"] <- navpre[, "LAT"]/100
navpre[, "Long"] <- navpre[, "LON"]/100


# Original logbook has some squirrely non-unicode characters
logbook <- read.csv(file.path(getwd(), "Data", "ShipLogbookID.csv"), strip.white = TRUE)
# I assume the ID numbers in the main file correspond to the rows in the logbook file
logbook[, "ID"] <- 1:nrow(logbook)
logbook <- logbook[logbook[, "Duplicate"] == 0, c("Name", "ID")]

# Add common ship names, drop duplicates
navpre <- merge(logbook, navpre, by = "ID")
row.names(navpre) <- as.character(1:nrow(navpre))

# reorder/rename
navpre <- navpre[, c("Name","Country", "YR", "Date",  "Month", "Lat", "Long", "D", "ID")]
names(navpre) <- c("Name","Country", "Year", "Date", "Month", "Lat", "Long", "WindDir", "ID")

# Within record, sort by date
navpre <- navpre[do.call(order, navpre[,c("ID", "Date")]), ]

# Lengths in sequence for each ID 
log.entries <- rle(navpre[, "ID"])$lengths


grabDistance <- function(lag = 1) {
  library(plyr)
  deg2rad <- function(deg) return(deg*pi/180)
  
  navcir <- navpre[, "ID", drop = FALSE]
  navcir[, "SinLat"] <- sin(deg2rad(navpre[, "Lat"]))
  navcir[, "CosLat"] <- cos(deg2rad(navpre[, "Lat"]))
  navcir[, "Long"] <- deg2rad(navpre[, "Long"])
  
  cir.list <- dlply(navcir, .(ID), transform)
  
  # Crudely borrowed from http://pineda-krch.com/2010/11/23/great-circle-distance-calculations-in-r/
  # But vectorized!
  gcd.mod <- function(x, n = lag) {
    extent <- nrow(x)
    if (extent < (1 + n)) return(rep(0, extent))
    R <- 6371
    d <- acos(x[1:(extent - n) , "SinLat"]*x[(n + 1):extent, "SinLat"] +
      x[1:(extent - n) , "CosLat"]*x[(n + 1):extent, "CosLat"] *
      cos(diff(x[, "Long"], lag = n))) * R
    return(c(rep(0, n), d))
  }
  lapply(cir.list, gcd.mod))
}

navpre[, "Distance"] <- unname(unlist(grabDistance()))

lag.test <- grabDistance(lag = 3)

initial.out <- which(navpre[, "Distance"] > 1000)
no.port.change <- initial.out[which(navpre[initial.out, "Date"] - navpre[initial.out - 1, "Date"] < 4)]

