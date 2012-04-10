
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

diff.lat <- diff(navpre[, "LAT"])
diff.id <- diff(navpre[, "ID"])
diff.date <- diff(navpre[, "Date"])
summary(diff.lat[diff.id == 0 & diff.date == 1])
single.diff <- diff.lat[diff.id == 0 & diff.date == 1]

# Original logbook has some squirrely non-unicode characters
logbook <- read.csv(file.path(getwd(), "Data", "ShipLogbookID.csv"), strip.white = TRUE)
logbook[, "ID"] <- 1:nrow(logbook)
logbook <- logbook[logbook[, "Duplicate"] == 0, c("Name", "Ident", "ID")]

