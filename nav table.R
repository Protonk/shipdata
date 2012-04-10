
navguide <-htmlParse("http://www.ucm.es/info/cliwoc/content/CLIWOC15corelimit.htm")
navtables <- readHTMLTable(navguide)
format <- navtables[[1]][-1, 1:3]
format <- as.data.frame(matrix(as.character(unlist(format, recursive = FALSE, use.names = FALSE)), dim(format)), stringsAsFactors = FALSE)

nav.widths <- c(diff(as.numeric(format[,2])), 2)
nav.names <- format[,1]

nav.limit <- 245195
core.url <- "http://www.knmi.nl/cliwoc/download/CLIWOC15corelimit.zip"
core.filename <- "CLIWOC15corelimit.txt"

reduced.names <- c("YR", "MO", "DY", "HR", "LAT", "LON", "ID", "C1", "D")

buildNavdf <- function(ingest) {
  grabRemote <- function(create.dir = FALSE) {
    temp <- tempfile()
    download.file(core.url, temp)
    navin <- read.fwf(unz(temp, filename = core.filename),
                      widths = widths,
                      col.names = nav.names,
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
navtest <- navtest[, names(navtest) %in% reduced.names]

navin <- read.fwf(file.path(getwd(), "Data", core.filename),
                  widths = nav.widths,
                  col.names = nav.names,
                  stringsAsFactors = FALSE,
                  n = nav.limit)



# reduce: head(navtest[, names(navtest) %in% reduced.names])