
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
nav.pre <- buildNavdf(ingest = "local")

# Drops all but date/time, lat/long, ship ID, country and wind direction
# May drop wind direction later but it might be fun to plot
nav.pre <- nav.pre[, names(nav.pre) %in% c("YR", "MO", "DY", "HR", "LAT", "LON", "ID", "C1", "D")]

# About 5 seconds faster than paste(). 
nav.pre[, "Date"] <- as.Date(sprintf("%4.f-%02.f-%2.f", nav.pre[,"YR"], nav.pre[,"MO"], nav.pre[,"DY"]), format = "%Y-%m-%d")

# Build Factor variables and convert lat/long
nav.pre[, "Country"] <- as.factor(nav.pre[, "C1"])
nav.pre[, "Month"] <- factor(month.name[nav.pre[, "MO"]], levels = month.name, ordered = FALSE)
nav.pre[, "Lat"] <- nav.pre[, "LAT"]/100
nav.pre[, "Long"] <- nav.pre[, "LON"]/100


# Original logbook has some squirrely non-unicode characters
logbook <- read.csv(file.path(getwd(), "Data", "ShipLogbookID.csv"), strip.white = TRUE)
# I assume the ID numbers in the main file correspond to the rows in the logbook file
logbook[, "ID"] <- 1:nrow(logbook)
logbook <- logbook[logbook[, "Duplicate"] == 0, c("Name", "ID")]

# Add common ship names, drop duplicates
nav.pre <- merge(logbook, nav.pre, by = "ID")
row.names(nav.pre) <- as.character(1:nrow(nav.pre))

# reorder/rename
nav.pre <- nav.pre[, c("Name","Country", "YR", "Date",  "Month", "Lat", "Long", "D", "ID")]
names(nav.pre) <- c("Name","Country", "Year", "Date", "Month", "Lat", "Long", "WindDir", "ID")

# Within record, sort by date
nav.pre <- nav.pre[do.call(order, nav.pre[,c("ID", "Date")]), ]



# This will spit some OOR warnings out, but those are ok,
# we'll end up removing those anyway.
grabDistance <- function(lag = 1) {
  library(plyr)
  deg2rad <- function(deg) return(deg*pi/180)
  
  navcir <- nav.pre[, "ID", drop = FALSE]
  navcir[, "SinLat"] <- sin(deg2rad(nav.pre[, "Lat"]))
  navcir[, "CosLat"] <- cos(deg2rad(nav.pre[, "Lat"]))
  navcir[, "Long"] <- deg2rad(nav.pre[, "Long"])
  
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
  lapply(cir.list, gcd.mod)
}

# This is bad behavior but these are expected (kinda)
# and can be ignored

options("warn" = -1)
nav.pre[, "Distance"] <- unname(unlist(grabDistance()))
options("warn" = 0)

voyageSplit <- function() {
  # Split records up by likely unmarked log/port changes
  port.split <- which(diff(nav.pre[, "Date"]) > 30) + 1
  dist.split <- which(nav.pre[, "Distance"] > 1000)
  # Lengths in sequence for each ID
  log.split <- cumsum(rle(nav.pre[, "ID"])$lengths) + 1
  # combine voyage splits and prepend first length
  comb.split <- c(87, diff(sort.int(union(union(port.split, dist.split), log.split))))
  rep.int(x = 1:length(comb.split), times = comb.split)
}
nav.pre[, "Voyage"] <- voyageSplit()
nav.out <- nav.pre[, c("Name", "Country", "Year", "Date", "Month",
                      "Lat", "Long", "WindDir", "Voyage")]
rm(nav.pre, logbook, buildGuide, buildNavdf, voyageSplit, grabDistance)
