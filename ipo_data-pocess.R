# Author: Sam Turner
# Script to process IPO, Zillow, and ZIP Geometry files for analysis
# IPO Data: https://finance.yahoo.com/
# Zillow Data: http://www.zillow.com/research/data/
# Inlfation Data: http://www.usinflationcalculator.com
# ZCTA Shapefiles: https://www.census.gov/geo/maps-data/data/cbf/cbf_zcta.html

# SWITCHES
linux <- TRUE # for setting directories
export <- TRUE # for exporting data to csv

# ROOT
if(linux) {
  dir.root <- "/mnt/common/work/PointDigFin/2"
} else {
  dir.root <- "D:/work/PointDigFin/2/"
}
setwd(dir.root)

# LIBRARIES
library(dplyr)
library(zipcode)

# SOURCE
source("ipo_functions.R")

# IPO
data.ipo <- readData(dir.data.ipo, "xlsx", "character")
data.ipo$AMEX_companylist[13:19] <- NULL # remove empty cols

data.ipo <- lapply(data.ipo, function(df)
  lapply(df, function(x)gsub("n/a", NA, x)))
data.ipo <- lapply(data.ipo, fixCurrency, "MarketCap")
data.ipo <- lapply(data.ipo, transform, MarketCap = as.numeric(MarketCap))
data.ipo <- lapply(data.ipo, function(df)
  transform(df, Zip = clean.zipcodes(df$Zip)))
data.ipo <- lapply(data.ipo, select, -Summary.Quote, -State, -City)

data(zipcode)
data.ipo <- lapply(data.ipo, merge, zipcode, by.x = "Zip", by.y = "zip")

data.ipo <- lapply(data.ipo, setNames, c('ZIP', 'Symbol', 'Name', 'LastSale'
                                         , 'MarketCap', 'Year', 'Sector'
                                         , 'Industry', 'Address', 'City'
                                         , 'State', 'lat', 'lon'))
data.ipo <- lapply(data.ipo, select, Year, Symbol, Name, Sector, Industry
                   , LastSale, MarketCap, Address, City, State
                   , ZIP, lat, lon)

data.ipo <- lapply(data.ipo, subset, !is.na(MarketCap))
data.ipo <- bind_rows(data.ipo)
data.ipo <- transform(data.ipo, Year = as.numeric(Year))
data.ipo <- filter(data.ipo, Year >= 1998)

# MARKET CAP
data.marketCapTot <- data.ipo %>% 
  group_by(Year, ZIP) %>%
  summarize(Total.MarketCap = sum(MarketCap)) %>%
  arrange(ZIP, Year) %>%
  as.data.frame

data.marketCapTot <- lapply(unique(data.marketCapTot$ZIP), function(z)
  filter(data.marketCapTot, ZIP == z)) %>%
  lapply(function(df)
    setNames(cbind(df, cumsum(df$Total.MarketCap))
             , c(names(df), "Cum.MarketCap")))

ts.marketCapTot <- lapply(data.marketCapTot, function(df) ts(start = df[1,1], frequency = 1, df[3:4]))
names(ts.marketCapTot) <- lapply(data.marketCapTot
                                 , function(df)return(df$ZIP[1]))
data.marketCapTot <- bind_rows(data.marketCapTot)

# INFLATION
data.inflation <- as.data.frame(readData(dir.data.inflation
                                         , "csv", "character"))
data.inflation <- changeClass(data.inflation, 1
                              , ncol(data.inflation), as.numeric)
rownames(data.inflation) <- data.inflation[,1]
data.inflation <- data.inflation[ , -c(1, 14, 15, 16)]

inflation.base <- 240.853 # Current CPI as of 10/10/2016
data.inflation <- as.data.frame((data.inflation/inflation.base)^-1)
data.inflation <- as.numeric(as.character(
  apply(data.inflation[85:104, ], 1, paste0)))
length(data.inflation) <- length(data.inflation) - 4

# ZILLOW
data.zillow <- readData(dir.data.zillow, "csv", "character")
data.zillow <- lapply(data.zillow, select, -c(8:16))
data.zillow <- lapply(data.zillow, function(df)
  changeClass(df, 8, length(df), as.numeric))

data.zillow <- lapply(data.zillow, function(df)
  cbind(df[1:7], as.data.frame(Map('*', df[8:243], data.inflation))))

ts.zillow <- lapply(data.zillow, function(df) as.data.frame(t(df[ , -c(1:7)])))
ts.zillow <- lapply(ts.zillow, function(df)
  lapply(df, ts, start = c(1997, 1), c(2016, 7), frequency = 12))
ts.zillow <- Map(function(df.ts, df.data)
  setNames(df.ts, df.data[["RegionName"]]), ts.zillow, data.zillow)

# MAP DATA
data.map <- data.ipo %>%
  group_by(ZIP) %>%
  summarize(IPO.Count = n()) %>%
  merge(zipcode, by.x = "ZIP", by.y = "zip") %>%
  setNames(c("ZIP", "IPO.Count", "City", "State", "lat", "lon"))

data.SqFt <- data.zillow$SqFt[8:243] %>%
  t %>%
  as.data.frame %>%
  setNames(data.zillow$SqFt$RegionName)
data.SqFt <- cbind(gsub("X", "", rownames(data.SqFt)), data.SqFt)
colnames(data.SqFt)[1] <- "Time"
#data.SqFt <- lapply(data.SqFt, function(c) return(c))

data.Value <- data.zillow$Value[8:243] %>%
  t %>%
  as.data.frame %>%
  setNames(data.zillow$Value$RegionName)
data.Value <- cbind(gsub("X", "", rownames(data.Value)), data.Value)
colnames(data.Value)[1] <- "Time"
#data.Value <- lapply(data.Value, function(c) return(c))

# EXPORT
if(export) {
  setwd(dir.data.export)
  write.csv(data.map, file = "IPO_count.csv", row.names = FALSE)
  write.csv(data.marketCapTot, file = "MarketCap.csv", row.names = FALSE)
  write.csv(data.zillow$SqFt, file = "SqFt.csv", row.names = FALSE)
  write.csv(data.zillow$Value, file = "Value.csv", row.names = FALSE)
#  write.csv(data.SqFt, file = "SqFt.csv", row.names = FALSE)
#  write.csv(data.Value, file = "Value.csv", row.names = FALSE)
#  Map(function(df, zip)
#    write.csv(df, file = paste0(dir.data.export, "/sqft/", zip, ".csv")
#              , row.names = FALSE), data.SqFt, names(data.SqFt))
#  
#  Map(function(df, zip)
#    write.csv(df, file = paste0(dir.data.export, "/value/", zip, ".csv")
#              , row.names = FALSE), data.Value, names(data.Value))

}