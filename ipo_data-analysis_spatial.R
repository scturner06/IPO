# Author: Sam Turner
# Script to analyze spatial component of IPO and Zillow data

# SWITCHES
linux <- FALSE # for setting directories
export <- TRUE # for exporting data to csv

# LIBRARIES
library(dplyr)
library(zipcode)

# SOURCE
#dir.root <- "/mnt/common/work/PointDigFin/2"
dir.root <- "D:/work/PointDigFin/2"
setwd(dir.root)
source("ipo_functions.R")

# IMPORT DATA
data.SqFt <- read.csv("data/processed/SqFt.csv", colClasses = "character")
data.SqFt <- changeClass(data.SqFt, 8, 243, as.numeric)
data.Value <- read.csv("data/processed/Value.csv", colClasses = "character")
data.Value <- changeClass(data.Value, 8, 243, as.numeric)
data.IPO <- read.csv("data/processed/MarketCap.csv"
                     , colClasses = "character") %>%
  transform(Year = as.numeric(Year)
            , Total.MarketCap = as.numeric(Total.MarketCap)
            , Cum.MarketCap = as.numeric(Cum.MarketCap))
data(zipcode)

data.SqFt <- data.SqFt %>%
  merge(zipcode, by.x = "RegionName", by.y = "zip") %>%
  select(1, city, state, latitude, longitude, c(8:243))
colnames(data.SqFt)[1:5] <- c("zip", "city", "state", "lat", "lon")
time.SqFt <- gsub("X", "", names(data.SqFt[6:241]))
time.SqFt <- gsub("\\.", "/", time.SqFt)
time.SqFt <- paste0(time.SqFt, "/01")
time.SqFt <- lapply(time.SqFt, as.Date, '%Y/%m/%d') %>%
  unlist %>%
  as.Date

data.Value <- data.Value %>%
  merge(zipcode, by.x = "RegionName", by.y = "zip") %>%
  select(1, city, state, latitude, longitude, c(8:243))
colnames(data.Value)[1:5] <- c("zip", "city", "state", "lat", "lon")
time.Value <- gsub("X", "", names(data.Value[6:241]))
time.Value <- gsub("\\.", "/", time.Value)
time.Value <- paste0(time.Value, "/01")
time.Value <- lapply(time.Value, as.Date, '%Y/%m/%d') %>%
  unlist %>%
  as.Date

zip.SqFt <- unique(data.SqFt$zip)
zip.SqFt <- zipcode[zipcode$zip %in% zip.SqFt, ] %>%
  select(zip, latitude, longitude) %>%
  setNames(c("zip", "lat", "lon"))
rownames(zip.SqFt) <- zip.SqFt$zip

zip.Value <- unique(data.Value$zip)
zip.Value <- zipcode[zipcode$zip %in% zip.Value, ] %>%
  select(zip, latitude, longitude) %>%
  setNames(c("zip", "lat", "lon"))
rownames(zip.Value) <- zip.Value$zip

zip.IPO <- unique(data.IPO$ZIP)
zip.IPO <- zipcode[zipcode$zip %in% zip.IPO, ] %>%
  select(zip, latitude, longitude) %>%
  setNames(c("zip", "lat", "lon"))

zip.IPO.SqFt <- zip.IPO[zip.IPO$zip %in% zip.SqFt$zip, ]
zip.IPO.Value <- zip.IPO[zip.IPO$zip %in% zip.Value$zip, ]

# GIS
setwd(dir.root)
load("workspace/good_breaks.RData")

library(gstat)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(ncf)

SqFt.radius <- getZipRadius(zip.SqFt, zip.IPO.SqFt, 40233.6) # ~25 mile radius
Value.radius <- getZipRadius(zip.Value, zip.IPO.Value, 40233.6)

dfl.SqFt <- lapply(names(SqFt.radius), function(nm)
  data.SqFt[data.SqFt$zip %in% c(nm, SqFt.radius[[nm]]$zip), ]) %>%
  setNames(names(SqFt.radius))
SqFt.breaks <- sapply(good.breaks$SqFt, function(x) computeInterval(x, 18, 6, 241))

SqFt.dist <- computeDist(dfl.SqFt, SqFt.breaks)
SqFt.dist.avg <- SqFt.dist %>%
  lapply(function(x) lapply(x[[1]], function(y) return(y))) %>%
  lapply(function(x) try(return(x$x.intercept))) %>%
  unlist %>%
  var(na.rm = TRUE) %>%
  sqrt
  
  
  tryCatch(lapply(function(x) return(x[[1]]$x.intercept)),
           error = function(e) return(NA)) %>%
  mean(na.rm = TRUE)

# DISPLAY
disp.breaks <- SqFt.breaks$`94105`
disp.data <- dfl.SqFt$`94105`
disp.dist <- correlog(x = disp.data$lon, y = disp.data$lat
                      , z = disp.data[-c(1:5)],
                      increment = 2, latlon = TRUE, na.rm = TRUE, quiet = TRUE)
plot(disp.dist)
disp.dist$correlation
