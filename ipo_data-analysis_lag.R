# Author: Sam Turner
# Script to analyze IPO and Zillow data
# ARIMA (2,1,2): www.zillow.com/research/new-home-sales-forecast-model-8064/

# SWITCHES
linux <- TRUE # for setting directories
export <- TRUE # for exporting data to csv

# LIBRARIES
library(dplyr)
library(mefa)
library(zipcode)
library(car)
library(nlme)

# SOURCE
#dir.root <- "/mnt/common/work/PointDigFin/2"
dir.root <- "D:/work/PointDigFin/2"
setwd(dir.root)
source("ipo_functions.R")

# IMPORT DATA
data.IPO <- read.csv("data/processed/MarketCap.csv"
                     , colClasses = "character") %>%
  transform(Year = as.numeric(Year)
            , Total.MarketCap = as.numeric(Total.MarketCap)
            , Cum.MarketCap = as.numeric(Cum.MarketCap))

data.SqFt <- read.csv("data/processed/SqFt.csv", colClasses = "character")
data.SqFt <- changeClass(data.SqFt, 8, 243, as.numeric)
data.Value <- read.csv("data/processed/Value.csv", colClasses = "character")
data.Value <- changeClass(data.Value, 8, 243, as.numeric)

# ANALYSIS
data.IPO.all <- lapply(unique(data.IPO$ZIP), function(x)
  filter(rep(data.IPO, 12), ZIP == x)) %>%
  setNames(unique(data.IPO$ZIP)) %>%
  lapply(group_by, Year) %>%
  lapply(function(df)
    mutate(df, Time = Year + seq(from = 0.01, to = 0.12, by = 0.01))) %>%
  lapply(merge, as.numeric(gsub("X", "", colnames(data.SqFt[8:243])))
         , by.x = "Time", by.y = "y", all = TRUE) %>%
  lapply(select, Time, Total.MarketCap, Cum.MarketCap) %>%
  lapply(function(df)
    transform(df, Total.MarketCap = fillNA(Total.MarketCap)
              , Cum.MarketCap = fillNA(Cum.MarketCap)))
data.IPO.all <- Map(function(df, dfName)
  mutate(df, ZIP = dfName), data.IPO.all, names(data.IPO.all))
ts.IPO <- data.IPO.all %>%
  lapply(function(df)
    ts(df[ , -1], start = c(1997, 1), c(2016, 7), frequency = 12))
data.IPO.all <- bind_rows(data.IPO.all)

ts.SqFt <- intersect(unique(data.IPO$ZIP), unique(data.SqFt$RegionName)) %>%
  lapply(function(x) filter(data.SqFt, RegionName == x)) %>%
  lapply(function(df) as.data.frame(t(df[ , -c(1:7)]))) %>%
  lapply(ts, start = c(1997, 1), c(2016, 7), frequency = 12) %>%
  setNames(intersect(unique(data.IPO$ZIP), unique(data.SqFt$RegionName)))

ts.Value <- intersect(unique(data.IPO$ZIP), unique(data.Value$RegionName)) %>%
  lapply(function(x) filter(data.Value, RegionName == x)) %>%
  lapply(function(df) as.data.frame(t(df[ , -c(1:7)]))) %>%
  lapply(ts, start = c(1997, 1), c(2016, 7), frequency = 12) %>%
  setNames(intersect(unique(data.IPO$ZIP), unique(data.Value$RegionName)))

ts.all <- list(ts.SqFt, ts.Value) %>%
  lapply(function(x)
    Map(function(y, z) ts.union(y, ts.IPO[[z]]), x, names(x))) %>%
  setNames(c("SqFt", "Value"))

data.all <- ts.all %>%
  lapply(function(l) lapply(l, function(df) as.data.frame(df))) %>%
  lapply(function(l) lapply(l, function(df)
    cbind(seq(from = 1, to = nrow(df)), df))) %>%
  lapply(function(l) lapply(l, function(df) select(df, 5, 1, 3, 2))) %>%
  lapply(function(l) lapply(l, function(df)
    setNames(df, c("ZIP", "Time", "MarketCap.p", "Price")))) %>%
  lapply(function(l) lapply(l, function(df)
    mutate(df, ZIP = factor(clean.zipcodes(ZIP))
              , MarketCap = as.numeric(factor(MarketCap.p))))) %>%
  lapply(function(l) lapply(l, function(df)
    transform(df, MarketCap = factor(
      Recode(MarketCap, paste(
        unique(MarketCap), seq(from = 1, to = length(unique(MarketCap)))
        ,sep = "=", collapse = ";")))))) %>%
  lapply(function(l) lapply(l, function(df)
    mutate(df
           , price.var = varWeights(Initialize((varFixed(~Price)), df))))) %>%
  lapply(function(l) lapply(l, function(df)
    breakFactors(df)))

ipo.time <- lapply(data.all, function(l) lapply(l, function(df)
  df[df$Time == ave(df$Time, df$MarketCap, FUN = min), ]))
brk.time <- lapply(data.all, function(l) lapply(l, function(df)
  df[df$Time == ave(df$Time, df$bp, FUN = min), ]))

# COMMON BREAKPOINT ANALYSIS
# Values to remove:
#   1: Common breakpoint due to data processing
#   35: Too common to be due to IPO event alone
#   130-138: Likely due to economic crash
#   165: Again, far too many to be due to chance
#   200: Due to data processing
SqFt.test <- lapply(brk.time$SqFt, function(df) df$Time) %>%
  unlist
png(filename = "brks.png", width = 800, height = 600)
hist(SqFt.test, breaks = 235)
axis(side = 1, at = seq(0, 235, 10), labels = seq(0, 235, 10))
dev.off()
test.hist.data <- cbind(test.hist$breaks[-(length(test.hist$breaks))]
                        , test.hist$counts)

brk.remove <- c(1, 35, 130:138, 165, 200)

brk.time <- brk.time %>%
  lapply(function(l) lapply(l, function(df)
    filter_(df, .dots = ~!(df$Time %in% brk.remove))))

# IPO EVENT CHECK
lag.time <- Map(function(l1, l2)
  Map(function(df1, df2) isBetweenVec(df1$Time, df2$Time, c(36, 3))
      , l1
      , l2)
  , ipo.time
  , brk.time)

# PERCENT CHANGE AND TIME LAG CALCULATION
# SQFT
SqFt.lm <- data.all$SqFt %>%
  lapply(function(df) tryCatch(
    gls(Price ~ Time*factor(bp) + factor(bp) - 1
        , data = df
        , weights = Initialize(varFixed(~Price), df), na.action = na.omit),
    error = function(e) return(NA)
    ))
SqFt.lm <- SqFt.lm[!is.na(SqFt.lm)]
SqFt.coef <- lapply(SqFt.lm, function(df)
  as.data.frame(summary(df)[["tTable"]]))

SqFt.change <- SqFt.coef %>%
  lapply(function(df) avgChange(df))
SqFt.change.means <- lapply(names(SqFt.change), function(nm)
  mean(SqFt.change[[nm]]$MarketCap %in%
         gsub("V", "", colnames(
           lag.time$SqFt[[nm]][ , colSums(lag.time$SqFt[[nm]]
                                          , na.rm = TRUE) != 0]))
       * SqFt.change[[nm]]$delta.pct, na.rm = TRUE))

SqFt.change.avg <- SqFt.change.means[SqFt.change.means > 0] %>%
  unlist %>%
  mean(na.rm = TRUE)
SqFt.lag <- lapply(lag.time$SqFt, function(df)
  rowMeans(df[-ncol(df)], na.rm = TRUE)) %>%
  unlist %>%
  mean(na.rm = TRUE)
SqFt.prob <- ipoProb(SqFt.change, lag.time$SqFt)

# VALUE
Value.lm <- data.all$Value %>%
  lapply(function(df) tryCatch(
    gls(Price ~ Time*factor(bp) + factor(bp) - 1
        , data = df
        , weights = Initialize(varFixed(~Price), df), na.action = na.omit),
    error = function(e) return(NA)
  ))
Value.lm <- Value.lm[!is.na(Value.lm)]
Value.coef <- lapply(Value.lm, function(df)
  as.data.frame(summary(df)[["tTable"]]))

Value.change <- Value.coef %>%
  lapply(function(df) avgChange(df))
Value.change.means <- lapply(names(Value.change), function(nm)
  mean(Value.change[[nm]]$MarketCap %in%
         gsub("V", "", colnames(
           lag.time$Value[[nm]][ , colSums(lag.time$Value[[nm]]
                                           , na.rm = TRUE) != 0]))
       * Value.change[[nm]]$delta.pct, na.rm = TRUE))

Value.change.avg <- Value.change.means[Value.change.means > 0] %>%
  unlist %>%
  mean(na.rm = TRUE)
Value.lag <- lapply(lag.time$Value, function(df)
  rowMeans(df[-ncol(df)], na.rm = TRUE)) %>%
  unlist %>%
  mean(na.rm = TRUE)
Value.prob <- ipoProb(Value.change, lag.time$Value)

# SPATIAL ANALYSIS DATA PREP
setwd(dir.data.export)
write.csv(lag.time, "lagTime.csv", row.names = isBe)

good.breaks <- lag.time %>%
  lapply(function(l) lapply(l, function(df)
    df[rowSums(df, na.rm = TRUE) - df$y > 0, ])) %>%
  lapply(function(l) lapply(l, function(df)
    select(df, y))) %>%
  lapply(function(l) lapply(l, function(df)
    if(nrow(df) > 0)
      return(df$y)))

save(good.breaks, file = "good_breaks.RData")

test <- good.breaks$SqFt
test.n <- test %>%
  lapply(function(x) is.null(x))
test.n <- test.n[test.n == TRUE]
test <- test[name]

temp <- good.breaks %>%
  lapply(function(l) lapply(l, function(df)
    nrow(df))) %>%
  lapply(function(l) l[l > 0])
