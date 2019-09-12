library(tidyverse)

interpolateData <- function(data = data,
                            dateCol = NA,
                            dateTimeFormat = '%M-%D-%Y',
                            depthCol= NA,
                            parameterCol = NA,
                            depth_units = NA){
  data <- as.data.frame(data) # only work with dataframes, tibbles etc. are difficult for interpolation method
  
  # manipulate dataset to standardized format
  data1 <- mutate(data, Date = as.Date(get(dateCol), format = dateTimeFormat),
                  Depth = get(depthCol),
                  parameter = get(parameterCol) ) %>%
    dplyr::select(Date, Depth, parameter) %>% # just the columns we want
    drop_na() # get rid of any missing data bc everything at this point is critical for interpolation
  
  # interpolate between sample events
  int = akima::interp(data1[, 'Date'], data1[, 'Depth'], data1[, 'parameter'], duplicate = "strip", linear = TRUE)
  
  # establish plotting date min/max 
  #min_date <- min(data1[, 'Date'], na.rm = T)
  #max_date <- max(data1[, 'Date'], na.rm = T)
  
  
  results <- list(data1,int)
  return(results)
  
}  

contour.heatmap <- function(int = int,
                            reverseColorPalette = FALSE,
                            title = NA,
                            criteria = NA,
                            depth_units = "m"){
  filled.contour(int[[2]], color.palette = function(n) topo.colors(n, rev= reverseColorPalette), 
                 xlim = c(min(int[[1]][, 'Date']) - 1, max(int[[1]][, 'Date']) + 1), 
                 ylim = rev(range(int[[1]][, 'Depth'])), 
                 ylab = paste0("Depth (", depth_units, ")"), 
                 main = title, 
                 plot.axes = {
                   contour(int[[2]], levels = criteria, lwd = 2, col = "red", 
                           drawlabels = TRUE, axes = FALSE, frame.plot = FALSE, 
                           add = TRUE, labcex = 2)
                   axis(1, at = unique(int[[1]][, 'Date']), labels = unique(int[[1]][,'Date']), par(las = 2))
                   axis(2)}
  )
  
}


library(wqTools) # use Utah's webscraping function

kerr1_nr <- readWQP(type="narrowresult", siteid="21VASWCB-4AROA038.49", print=F)
kerr1_act <- readWQP(type="activity", siteid="21VASWCB-4AROA038.49", print=F)
kerr1_nr_act <- merge(kerr1_nr, kerr1_act, all.x=T) %>%
  filter(!is.na(ResultMeasureValue)) #Subset to profile data EVJ change to ResultMeasureValue to get populated data lines


# function to simplify formatting
WQX_wide <- function(data = data){
  #make sure only one tz
  if(length(unique(data$ActivityStartTime.TimeZoneCode)) == 1){  tz <- unique(data$ActivityStartTime.TimeZoneCode)
  }else{
    stop("More than one time zone present, clean data.")
  }
  
  # Get columns of interest
  data1 <- dplyr::select(data, ActivityStartDate, ActivityStartTime.Time, ActivityStartTime.TimeZoneCode, MonitoringLocationIdentifier,
                         CharacteristicName, ResultSampleFractionText, ResultMeasureValue, ResultMeasure.MeasureUnitCode, ActivityDepthHeightMeasure.MeasureValue,
                         ActivityDepthHeightMeasure.MeasureUnitCode,ActivityLocation.LatitudeMeasure, ActivityLocation.LongitudeMeasure) %>%
    mutate(CharacteristicName_unit = paste(CharacteristicName, ' [', ResultSampleFractionText,']',
                                           ' (',ResultMeasure.MeasureUnitCode,')',sep = ''), #make one name for each parameter to not lose units
           DateTime = as.POSIXct(paste(ActivityStartDate, ActivityStartTime.Time, sep = ' '), format = '%Y-%m-%d %H:%M:%S', tz = tz),
           Depth = ActivityDepthHeightMeasure.MeasureValue) %>%
    unite(SampleDate_Depth, DateTime, Depth, sep = '.._/_!_..')  # use unique string to avoid separation issues
  
  
  # Collapse a smaller version of this to not lose wide data 
  data2 <- dplyr::select(data1, CharacteristicName_unit, SampleDate_Depth, ResultMeasureValue) %>%
    distinct(CharacteristicName_unit, SampleDate_Depth, .keep_all = TRUE) %>%
    # spread by activity (parameter collected)
    spread(CharacteristicName_unit,ResultMeasureValue,  convert = TRUE, drop=FALSE) # convert = TRUE keeps original data format
  
  data3 <- dplyr::select(data1, SampleDate_Depth, MonitoringLocationIdentifier, ActivityLocation.LatitudeMeasure, ActivityLocation.LongitudeMeasure) %>%
    distinct() %>%
    left_join(data2, by='SampleDate_Depth') %>%
    separate(SampleDate_Depth, c('DateTime', "Depth"), sep= '.._/_!_..') %>%
    # update column types
    mutate(DateTime = as.POSIXct(`DateTime`, format = '%Y-%m-%d %H:%M:%S', tz = tz),
           Depth = as.numeric(Depth)) %>%
    rename('LatitudeDD' = 'ActivityLocation.LatitudeMeasure',
           'LongitudeDD' = 'ActivityLocation.LongitudeMeasure')%>%
    arrange(DateTime)
  
  return(data3)
  
}

zz <- WQX_wide(kerr1_nr_act)%>%
  arrange(DateTime)

zz1 <- zz[1:230,]
zz2 <- zz[231:321,]
zz3 <- zz[322:627,]

data1 <- select(zz1, DateTime, Depth, `Temperature, water [Total] (deg C)`)
data2 <- select(zz2, DateTime, Depth, `Temperature, water [Total] (deg C)`)
data3 <- select(zz3, DateTime, Depth, `Temperature, water [Total] (deg C)`)


int3 <- interpolateData(data = zz3, # to adjust date range you need to limit input data
                       dateCol = 'DateTime',
                       dateTimeFormat = '%Y-%m-%d %H:%M:%S',
                       depthCol = "Depth",
                       parameterCol = 'Temperature, water [Total] (deg C)',
                       depth_units = "m")

saveRDS(int3,'int_2010-2019_T.RDS')

contour.heatmap(int3, reverseColorPalette = F, criteria = 30)

contour.heatmap(int_WQX, reverseColorPalette = TRUE, criteria = 4)

rm(kerr1_nr_act);rm(kerr1_nr);rm(kerr1_act)  