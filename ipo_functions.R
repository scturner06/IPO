# Author: Sam Turner
# Source file for common functions and variables used in IPO analysis

# LIBRARIES
library(xlsx)
library(car)
library(strucchange)

# FUNCTIONS
readData <- function(dir, ext, classes = NULL) {
  # Reads in files and puts them in a list of dataframes
  #
  # Args:
  #   dir: Directory where files are located
  #   classes: Vector of classes to be used in read.csv colClasses arg
  #
  # Returns:
  #   A list of dataframes containing the files in dir with the .csv extension
  #   removed.
  setwd(dir)
  files <- list.files(pattern = paste0("*.", ext))
  
  if(ext == "csv")
    data <- lapply(files, read.csv, colClasses = classes)
  if(ext == "xlsx") {
    data <- lapply(files, read.xlsx2
                   , sheetIndex = 1
                   , colClasses = classes
                   , stringsAsFactors = FALSE)
  }
  names(data) <- gsub("\\..*", "", files)
  
  setwd(dir.root)
  
  return(data)
}

fixCurrency <- function(df, curCol) {
  # Converts curCol to numeric from currency in format $*[M,B]
  #
  # Args:
  #   df: dataframe containing a currency column
  #   curCol: name of column with currency data
  #
  # Returns:
  # A dataframe with curCol converted to numeric
  temp <- df[[curCol]]
  temp <- gsub("\\$", "", temp)
  temp <- gsub("M", "e6", temp)
  temp <- gsub("B", "e9", temp)
  df[[curCol]] <- temp
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  return(df)
}

changeClass <- function(df, first, last, classFun) {
  # Changes class of cols in range first to last in df to class
  # in function classFun
  #
  # Args:
  #   df: dataframe to change classes
  #   first: first column number to change
  #   last: last column number to change
  #   classFun: function that changes class (as.numeric, etc...)
  #
  # Returns:
  # A dataframe with classes in range first to last changed by classFun
  temp <- df[first:last]
  temp <- lapply(temp, classFun)
  temp <- as.data.frame(cbind(df[-c(first:last)], temp)
                        , stringsAsFactors = FALSE)
  return(temp)
}

fillNA <- function(repCol) {
  # Takes a column repCol and replaces all NA entries with the last non-NA
  #
  # Args:
  #   repCol: column to operate on
  #
  # Returns:
  # Column as a list with NAs replaced
  temp <- repCol
  if(is.na(repCol[1]))
    temp[1] <- 0
  for(x in seq(from = 2, to = length(temp))) {
    if(is.na(repCol[x])){
      temp[x] <- temp[(x-1)]
    } else {
      temp[x] <- repCol[x]
    }
  }
  return(temp)
}

filterZIP <- function(df, zips.zillow, zips.IPO) {
  temp <- intersect(zips.zillow, zips.IPO) %>%
    lapply(function(x) paste0("X", x))
  temp <- select_(df, .dots = c("Time", temp))
  return(temp)
}

avgChange <- function(df.coefs) {
  # Takes coefs from time series intervention regression and computes the
  # mean percent change between each level
  #
  # Args:
  #   df.coefs: dataframe containing coefs from regression
  #
  # Returns:
  #   Dataframe containing pecent change and all factor levels filtered at
  #   95% CI.
  coefs <- df.coefs[!grepl("Time", rownames(df.coefs)), ] %>%
    setNames(c("Value", "Std. Error", "t value", "p-value")) %>%
    mutate(delta.pct = diff(c(Value, NA)) / Value)
  coefs <- cbind(seq(from = 1, to = nrow(coefs))
                 , coefs, c(coefs$`p-value`[-1], NA)) %>%
    setNames(c("MarketCap", "Value", "Std. Error", "t value"
               , "pval.1", "delta.pct", "pval.2")) %>%
    filter(pval.1 <= 0.05, pval.2 <= 0.05)
  return(coefs)
}

breakFactors <- function(df) {
  # Takes dataframe, computes breakpoints, and breaks df into factors by
  # breakpoints
  #
  # Args:
  #   df: dataframe to factorize by breaks
  #
  # Returns:
  #   Dataframe with a factor column by breaks
  brk <- breakpoints(ts(df$Price, frequency = 12) ~ 1)$breakpoints
  brk.1 <- c(0, brk)
  brk.2 <- c(brk, max(df$Time))
  
  df.temp <- mutate(df, bp = 1)
  df.temp <- Map(function(x, y) filter(df, Time > x, Time <= y)
                 , x = brk.1
                 , y = brk.2)
  df.temp <- Map(function(df, x) transform(df, bp = factor(x))
                 , df = df.temp
                 , x = seq(from = 1, to = length(brk.1))) %>%
    bind_rows
}

isBetweenVec <- function(x, y, window) {
  # Determines if elements of vector are between each element of vector y
  # offset by window specified by window
  #
  # Args:
  #   x: Vector to determine if between
  #   y: Vector of elements to be offset by interval in window
  #   window: Vector of form (window size, offset), e.g. (10, 3)
  #           would correspond to a window of size 10 starting 3
  #           units behind 10
  #
  # Returns:
  #   A dataframe in which columns are x, and rows are each element
  #   of y denoted in break.  A 0 indicates that it is not between
  #   while a number indicates the distance between x and y.
  temp <- sapply(x, function(x1) sapply(y, function(y1)
    isBetween(x1, y1 - window[1], y1 - window[2]))) %>%
    as.data.frame %>%
    cbind(y) #%>%
    #setNames(c(x, "brk"))
}

isBetween <- function(x, y1, y2) {
  # Funtion used in conjunction with isBetweenVec to simplify.
  # Determines if element is in between
  #
  # Args:
  #   x: Scalar to test
  #   y1: Lower bound of window
  #   y2: Upper bound of window
  #
  # Returns:
  #   0 if x is not between y1 and y2 and y2 - x otherwise
  if(x >= y1 & x <= y2) {
    return(y2 - x)
  } else {
    return(NA)
  }
}

ipoProb <- function(df1, df2) {
  # Computes probability that an IPO is associated with an increase in value
  #
  # Args:
  #   df1: Dataframe containing percent changes for each breakpoint
  #   df2: Dataframe containing lag times for each IPO
  #
  # Returns:
  #   Portion of IPO events that resulted in a change in the mean of price
  temp.num <- lapply(names(df1), function(nm)
    df1[[nm]]$MarketCap %in%
      gsub("V", "", colnames(
        df2[[nm]][ , colSums(df2[[nm]], na.rm = TRUE) != 0]))
    * df1[[nm]]$delta.pct) %>%
    unlist
  temp.num <- temp.num[temp.num > 0] %>%
    length
  
  temp.dem <- lapply(df2, function(df)
    ncol(df) - 1) %>%
    unlist %>%
    sum
  
  return(temp.num / temp.dem)
}

computeInterval <- function(brk, win, shift, ln) {
  # Computes intervals for computeDist
  #
  # Args:
  #   brk: Vector of breakpoints
  #   win: Window size in months
  #   shift: Offset to determine first time col
  #
  # Returns:
  #   A nx2 dataframe containing the start and stop index of each window
  l.temp <- (brk - win + shift) %>%
    sapply(function(x)
      if(x < 0) {
        return(0)
      } else {
        return(x)
      })
  
  h.temp <- (brk + win + shift) %>%
    sapply(function(x)
      if(x > ln) {
        return(ln)
      } else {
        return(x)
      })
  
  return(data.frame(l.temp, h.temp))
}

computeDist <- function(df.all, brk) {
  # Computes correlogram of df.all broken into segments defined by brk
  #
  # Args:
  #   df.all: dataframe containing lat, lon, and values in cols
  #   brk: 2xn dataframe containing start and end intervals for time cols in
  #        df.all
  #
  # Returns:
  #   List containing x.intercept of correlog, corresponding to 95% CI of
  #   correlated distance.
  dist.temp <- Map(function(df, b) lapply(seq(from = 1, to = nrow(b)), function(pt)
    tryCatch(
      correlog(x = df$lon, y = df$lat
               , z = df[seq(from = b[pt, 1], to = b[pt, 2])],
               increment = 2, latlon = TRUE, na.rm = TRUE, quiet = TRUE)$x.intercept,
      error = function(d) return(NA)))
    , df.all
    , brk)
  
  return(dist.temp)
}

getZipRadius <- function(df.zip, df.ipo.zip, distance) {
  # Computes distance of all points within distance radius of elements of
  # df.ipo.zip in df.zip
  #
  # Args:
  #   df.zip: Dataframe containing lat and lon
  #   df.ipo.zip: Dataframe containing lat and lon of zips with an IPO
  #   distance: radius to capture points in
  #
  # Returns:
  # A list of dataframes with names corresponding to ZIPs in df.ipo.zip
  # and the distance from the IPO ZIP within distance radius.
  coordinates(df.zip) <- ~lon+lat
  projection(df.zip) <- CRS("+init=epsg:4326")
  geo.temp <- spTransform(df.zip,CRS("+init=epsg:3395"))
  dist.temp <- gDistance(geo.temp, byid = TRUE)
  dist.temp <- as.data.frame(dist.temp)
  dist.temp <- lapply(dist.temp, function(x) x * (x <= distance))
  dist.temp <- as.data.frame(dist.temp)
  colnames(dist.temp) <- gsub("X", "", colnames(dist.temp))
  rownames(dist.temp) <- colnames(dist.temp)
  
  radius.temp <- lapply(df.ipo.zip$zip, function(x)
    dist.temp[[x]][dist.temp[[x]] > 0])
  zip.temp <- lapply(df.ipo.zip$zip, function(x)
    (dist.temp[[x]] > 0) * as.numeric(rownames(dist.temp))) %>%
    lapply(function(x) x[x > 0])
  df.temp <- Map(function(rad, zip) as.data.frame(cbind(zip, rad))
                 , radius.temp
                 , zip.temp) %>%
    lapply(function(df) transform(df, zip = clean.zipcodes(zip))) %>%
    setNames(df.ipo.zip$zip)
  
  Map(function(n, df) rbind(df, c(n, "0"))
      , names(df.temp)
      , df.temp)
  
  return(df.temp)
}

# DIRECTORIES
if(linux) {
  dir.root <- "/mnt/common/work/PointDigFin/2"
  dir.data.ipo <- "/mnt/common/work/PointDigFin/2/data/ipo"
  dir.data.export <- "/mnt/common/work/PointDigFin/2/data/export"
  dir.data.zillow <- "/mnt/common/work/PointDigFin/2/data/zillow"
  dir.data.inflation <- "/mnt/common/work/PointDigFin/2/data/inflation"
  dir.data.processed <- "/mnt/common/work/PointDigFin/2/data/processed"
} else {
  dir.root <- "D:/work/PointDigFin/2"
  dir.data.ipo <- "D:/work/PointDigFin/2/data/ipo"
  dir.data.export <- "D:/work/PointDigFin/2/data/export"
  dir.data.zillow <- "D:/work/PointDigFin/2/data/zillow"
  dir.data.inflation <- "D:/work/PointDigFin/2/data/inflation"
  dir.data.processed <- "D:/work/PointDigFin/2/data/processed"
}
setwd(dir.root)
