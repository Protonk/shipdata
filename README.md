# 18th-19th Century Navigation Data

Originally recorded in ships' logs between ~1750-1850, the data track latitude, longitude and temperature (along with other factors including wind speed) for ships leaving Europe for North and South America as well as Africa and Asia.

The logbooks were digitized for the [Climatological Database for the World's Oceans](http://www.ucm.es/info/cliwoc/), a study of temperature change before and during the industrial revolution. However the data is also useful to track and map shipping during the heyday of [triangular trade](http://en.wikipedia.org/wiki/Triangular_trade) in the Atlantic.

## Parsing data

The original data comes in two formats: Microsoft Access and a semi-structured mixed fixed/variable width text format.

**nav table.R** loads the data in text format and produces a csv from the output as well as building a dataframe with the appropriate classes and what-not. 

While it is my intent to preserve as much of the original data as possible, I want to provide a simple, tractable dataset for mapping trade routes. Consequently information on temperature and other variables will be dropped in some steps of the script.

The script breaks up data into groups (voyage) by three criteria:

* Logbook changes. Different logbooks are noted for individual ships. A change in the logbook is a potential indicator that a new voyage has started
* Large time differences. Within the same logbook if a ship spends more than 30 days without a log entry we can imagine that a new voyage is started.
* Distance discontinuities. Some of the records jump large distances (great circle) in a day or two. Sometimes this may be from a transcription error and other times it may be because a logbook was transferred. In either case it would break up a chart were we to connect the lines.