library(XML)
library(lubridate)
library(raster)

infile <- htmlTreeParse("../komoot_data/2017-10-31_24717322_fahrradtour_export.gpx", 
                        useInternalNodes = TRUE)
elevation <- xpathSApply(infile, path = "//trkpt/ele", xmlValue)
time <- xpathSApply(infile, path = "//trkpt/time", xmlValue)
coords <- xpathSApply(infile, path = "//trkpt", xmlAttrs)
lat <- coords["lat", ]
lon <- coords["lon", ]

df <- data.frame(lat, lon, elevation, time,
                 stringsAsFactors = FALSE)

df$time2 <- c(0, head(df$time, n = -1))
df$time <- strptime(df$time, format = "%Y-%m-%dT%H:%M:%OS")
df$time2 <- strptime(df$time2, format = "%Y-%m-%dT%H:%M:%OS")
df$timediff <- difftime(df$time, df$time2, units = "secs")

df$elevation2 <- c(NA, head(df$elevation, n = -1))
df$elevationdiff <- as.numeric(df$elevation) - as.numeric(df$elevation2)

df$lat2 <- c(0, head(df$lat, n = -1))
df$lon2 <- c(0, head(df$lon, n = -1))


df$dist <- apply(df, 1, function(x) {
  pointDistance(c(as.numeric(x["lat2"]), as.numeric(x["lon2"])),
                c(as.numeric(x["lat"]), as.numeric(x["lon"])),
                lonlat = TRUE)
  })
df$dist[1] <- 0

max(df$elevation)
min(df$elevation)
dist <- sum(df$dist) / 1000
sum(df[df$elevationdiff > 0, "elevationdiff"], na.rm = TRUE)
sum(df[df$elevationdiff < 0, "elevationdiff"], na.rm = TRUE)
time <- as.numeric(difftime(df$time[nrow(df)], df$time[1], units = "hours"))
speed <- dist / time

