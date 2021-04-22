#web API test for weather station data
#my CIMIS API key: 0032799d-060b-4859-b1d1-73dffc65f9e1
#template(ish): http://et.water.ca.gov/api/data?appKey=YOUR-APP-KEY&targets=2,8,127&startDate=2010-01-01&endDate=2010-01-05
library(httr)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(rvest)

#this works - kind of. I needed to run the GET function with the RETRY wrapper multiple times because the API is finicky 
#may be nice for wine quality. I guess we don't need refreshing data for wine quality?
url <- "https://et.water.ca.gov/api/data?appKey=0032799d-060b-4859-b1d1-73dffc65f9e1&targets=77&startDate=2010-01-01&endDate=2012-01-01&unitOfMeasure=M&dataItems=day-air-tmp-avg,day-air-tmp-min,day-air-tmp-max"

napa <- RETRY("GET", url)
napa_data <- rawToChar(napa$content)
napa_weird <- fromJSON(napa_data)
napa_less_weird <- as.data.frame(napa_weird$Data$Providers$Records)
napa_df <- data.frame(date = napa_less_weird$Date, station = napa_less_weird$Station, avgTemp = napa_less_weird$DayAirTmpAvg$Value, minTemp = napa_less_weird$DayAirTmpMin$Value, maxTemp = napa_less_weird$DayAirTmpMax$Value)

str(napa_less_weird)