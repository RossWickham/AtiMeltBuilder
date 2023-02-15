
# SWITCH TO snotelr package
library(gridExtra)
library(lubridate)
library(snotelr)
library(tidyverse)

### TODO ####
#Error handling for data download - return NA if no site data available, and handle in 
#  downstream calcs

### Config ------------------------------------------------------------------

# Snotel Station IDs
# Get them from here:
# https://www.wcc.nrcs.usda.gov/webmap/index.html#version=80.2&elements=&networks=!&states=!&counties=!&hucs=&minElevation=&maxElevation=&elementSelectType=all&activeOnly=true&activeForecastPointsOnly=false&hucLabels=false&hucParameterLabels=false&stationLabels=&overlays=&hucOverlays=&mode=data&openSections=dataElement,parameter&controlsOpen=true&popup=647:OR:SNTL&popupMulti=&base=esriNgwm&displayType=station&basinType=6&dataElement=WTEQ&parameter=PCTMED&frequency=DAILY&duration=I&customDuration=&dayPart=E&year=2019&month=5&day=28&monthPart=E&forecastPubMonth=5&forecastPubDay=1&forecastExceedance=50&seqColor=1&divColor=3&scaleType=D&scaleMin=&scaleMax=&referencePeriodType=POR&referenceBegin=1981&referenceEnd=2010&minimumYears=20&hucAssociations=true&lat=45.3714&lon=-117.3477&zoom=9.0
sntlIDs <- c(721, 563, 357)

# Base temperature (deg F) for calculations
baseTemp <- 32

### Functions ---------------------------------------------------------------

waterYear <- function (t){
  year(t) + ifelse(month(t) >= 10, 1, 0)
}

computeATI <- function(sntlData,baseTemp=0){
  # Computes cumulative degree days over base temperature (ATI) from SNOTEL
  #  dataset as exported using snotelr

  cat(sprintf("\nComputing ATI. WY = %g\tSite id = %g\tBase temp = %f",
              unique(sntlData$wy_), unique(sntlData$id), baseTemp))
  
  # Initialize the accumulated
  sntlData$ati <- NA
  #Skipping if missing winter months
  if(!all(c(10:12,1:5) %in% month(sntlData$date)) )
    return(sntlData) 
  
  #Setting NA values to base temp.
  sntlData$temperature_mean[is.na(sntlData$temperature_mean)] <- baseTemp
  
  sntlData$ati <- cumsum(x = sntlData$temperature_mean-baseTemp)
  sntlData
}

computeCumulativeMelt <- function(sntlData){
  # Compute the cumulative melt from SNOTEL
  #  dataset as exported using snotelr
  sntlData %>%
    mutate(
      melt_mm = c(NA, cumsum(diff(sntlData$snow_water_equivalent)))
    )
}

convertDFFactorsToChar <- function(inDF){
  
  out <- inDF
  for( col in names(inDF)  ){
    if(is.factor(inDF[,col])) out[,col] <- as.character(inDF[,col])
  }
  return(out)
}

getNtwkFromMeta <- function(site_id,meta){
  #Determines the network associated with the station
  #  given the metadata, as loaded from NRCS website using RNRCS lib
  meta$ntwk[ meta$site_id == site_id ]
}

parseIDFromName <- function(site_id){
  #Detects whether or not the site ID is valid for input argument to
  #  the 'grabNRCS.data' and returns extracted numeric site id if 
  #  applicable
  if(grepl(":",site_id)) {
    return(as.numeric(strsplit(site_id,":")[[1]][2]))
  }else{
    #Otherwise, assume ID is good
    return(as.numeric(site_id))
  }
}

getAllDailyStationData <- function(site_id, meta){
  #Given the site ID and metadata dataframe, returns all
  #  available data for the site
  
  cat(sprintf("\nRetrieving site data for:\t%s", site_id))
  
  out <- try(
    grabNRCS.data(network=getNtwkFromMeta(site_id, meta),
                  site_id=parseIDFromName(site_id),
                  timescale="daily",
                  DayBgn = getStartDateFromMeta(site_id, meta),
                  DayEnd = getEndDateFromMeta(site_id, meta)),
    silent = T
  )
  
  if( class(out) == "try-error" ){
    warning(sprintf("\nCould not retrieve data for site ID:\t%s\n\t%s", site_id,
                    gsub("\n","\n\t",as.character(out))))
    return(NA)
  }else{
    # out$Date <- as.POSIXct(out$Date,format="%Y-%m-%d %HH:%MM") #Formatting to POSIXct
    return(out)
  }
  
}

# Plots all datasets vs time, combining temperature datasets
plotSnltData <- function(rawSntl){
  p1 <- rawSntl %>%
    ggplot() + geom_line(aes(x=date, y=temperature_mean), color="black") +
    geom_line(aes(x=date, y=temperature_min), color="blue") +
    geom_line(aes(x=date, y=temperature_max), color="red")
  p2 <- rawSntl %>%
    ggplot() + geom_line(aes(x=date, y=precipitation_cumulative))
  p3 <- rawSntl %>%
    ggplot() + geom_line(aes(x=date, y=snow_water_equivalent))
  p <- lapply(c(p1, p2, p3), ggplotGrob)
  grid.arrange(p1,p2,p3, nrow=3, ncol=1)
}


### Load Data ---------------------------------------------------------------

# rawSntl <- 
#   snotel_download(site_id = sntlIDs,
#                   internal = T) %>%
#   mutate(
#     date = as.Date(date),
#     wy = waterYear(date)
#   )

# Taking a peek
# rawSntl %>%
#   subset(site_id == sntlIDs[1]) %>%
#   plotSnltData()

### Main --------------------------------------------------------------------

#Compute cumulative degree days above base temp by water year and site_id
# sntl <- 
#   rawSntl %>%
#   split(list(.$site_id, .$wy)) %>%
#   map(computeATI) %>%
#   compact() %>%
#   map(computeCumulativeMelt) %>%
#   compact() %>%
#   bind_rows()



