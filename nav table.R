
buildGuide <- function() {
  navguide <-htmlParse("http://www.ucm.es/info/cliwoc/content/CLIWOC15corelimit.htm")
  navtables <- readHTMLTable(navguide)
  format <- navtables[[1]][-1, 1:3]
  as.data.frame(matrix(as.character(unlist(format, recursive = FALSE, use.names = FALSE)), dim(format)), stringsAsFactors = FALSE)
}


buildNavdf <- function(ingest) {
  nav.limit <- 245195
  core.url <- "http://www.knmi.nl/cliwoc/download/CLIWOC15corelimit.zip"
  core.filename <- "CLIWOC15corelimit.txt"
  grabRemote <- function(create.dir = FALSE) {
    format.ref <- buildGuide()
    temp <- tempfile()
    download.file(core.url, temp)
    navin <- read.fwf(unz(temp, filename = core.filename),
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
navtest <- buildNavdf(ingest = "initial")


navtest <- navtest[, names(navtest) %in% c("YR", "MO", "DY", "HR", "LAT", "LON", "ID", "C1", "D")]
navtest[, "Date"] <- as.Date(sprintf("%4.f-%02.f-%2.f", navtest[,"YR"], navtest[,"MO"], navtest[,"DY"]), format = "%Y-%m-%d")

navtest[, "HR"] <- sprintf("%04.f",navtest[,"HR"])
