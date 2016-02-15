# StormTracking.Rmd
Chad Banicki  
February 12, 2016  


#   Reproducible Research
## A look at severe weather events over the last 4 decades and their effect on lives, property, and agriculture by state
### Synopsis: 
- Major weather event data is gathered from the U.S National Oceanic and Atmospheric Administration (NOAA) storm database. 
###This data conatins high-level details including:
<ul>
  <li>The dates of important weather events</li>
  <li>The location where they occurred/li>
  <li>The type of event</li>
  <li>The cost in US dollars to property and crops</li>
  <li>The number of lives lost as a result of the event</li>
  <li>The number of injuries sufferred</li>
</ul>


## Data Processing
-- Data collection is read from a URL proivded by the NOAA, there are aprox 1 Million rows dating back to 1971. Earlier years are less complete.
-- The date fields are split by begin and end dates and times, with another field for timezone.  As part of the processing dates these fields were combined below.
-- Due to the volume of data, and number of events, summary tables have been created to isoloate the top 20 events, in terms of damage to life and property.
-- A separate file, including the average lat and lons for each state, was added to be able to create the google map showing major storms, and the damage to life and crops respectively.

 
## Synopsis: This analysis focuses primarily on the types of events, and the costs through loss of life, injury, and damage.

- The top 20 event types have been isolated for the puroses of this analysis
- The overall totals are considered for the data collected, without consideration to the years or months occurred
- Monetary damages have all been converted to Millions of dollars US

-- Briefly, the findings suggest that Tornado's and Floods are the most damaging weather events that occur
-- in terms of both the cost of life, as well as monetary damages. 
-- The only exception being that hail contributes largely to the costs associated with crop damage

- If there is a general takeaway from this analysis, perhaps high winds and flood waters are the major weather events
- that pose the greatest risk to life and property.

-- 

##  Assignment: Course Project 2

### Load the packages needed for the script:
   

```r
    library(dplyr) 
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
    require(data.table) 
```

```
## Loading required package: data.table
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, last
```

```r
    require(lubridate) 
```

```
## Loading required package: lubridate
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:data.table':
## 
##     hour, mday, month, quarter, wday, week, yday, year
```

```r
    require(ggplot2) 
```

```
## Loading required package: ggplot2
```

```r
    require(ggrepel) 
```

```
## Loading required package: ggrepel
```

```r
    require(ggmap) 
```

```
## Loading required package: ggmap
```

```r
    require(rworldmap) 
```

```
## Loading required package: rworldmap
```

```
## Loading required package: sp
```

```
## ### Welcome to rworldmap ###
```

```
## For a short introduction type : 	 vignette('rworldmap')
```

```r
    require(grid)
```

```
## Loading required package: grid
```

```r
    require(scales)
```

```
## Loading required package: scales
```

```r
    require(tm)
```

```
## Loading required package: tm
```

```
## Loading required package: NLP
```

```
## 
## Attaching package: 'NLP'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     annotate
```

```r
    require(SnowballC)
```

```
## Loading required package: SnowballC
```

```r
    require(wordcloud) 
```

```
## Loading required package: wordcloud
```

```
## Loading required package: RColorBrewer
```
 
###   Set the working directory:


```r
     #setwd("C://R//RR//P2")
```

##     Load the Data
 

```r
    tryCatch(
      
      {
        #Check whether you have already dowloaded the data into the working director, and if not, download and unzip it 
        
        message("Checking local drive for data file. If it's not there then trying URL for new file.")
        
        if (!file.exists("repdata-data-StormData.zip")) {
          
          fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
          
          fileName <- "repdata-data-StormData.zip"
          
          download.file(fileURL, fileName, mode = "wb")
          
          dateDownloaded <- date()
          
          unzip(fileName, files = NULL, list = FALSE, overwrite = TRUE,
                junkpaths = FALSE, exdir = ".", unzip = "internal",
                setTimes = FALSE)
        }
        
      },
      error=function(cond) {
        message(paste("Error getting data:", fileURL))
        message("Original Error Message:")
        message(cond)
        # Return NA
        stop("Check connection and try again.")
        return(NA)
      },
      warning=function(cond) {
        message(paste("Warning getting data", fileURL))
        message("Original warning message:")
        message(cond)
      },
      
      finally={
        
      
      ## Read tracking data
      data <- read.csv("repdata-data-StormData.csv", header = TRUE,na.strings = c("NA"))

   
      }
    )
```

```
## Checking local drive for data file. If it's not there then trying URL for new file.
```
  
##     Formatting of the data 
   

```r
      ## Load lat and long for each state
      states <- read.csv("StatesLatLong.csv", header = TRUE, na.strings = c("NA"))
      
      dataSum <- tbl_df(data)
      
      dataCorpus <- Corpus(VectorSource(dataSum$REMARKS))
      
      rm(data)
      
      timeZones <- data.frame(TIME_ZONE = c("CST","AKS","MST","PST","EST","ESt","HST","SST","AST","GMT","UTC","MDT","EDT","PDT","CDT","GST"),
                              tz = c("CST6CDT","America/Anchorage","MST7MDT","PST8PDT","EST5EDT","EST5EDT","HST","Pacific/Samoa","America/Puerto_Rico","GMT","UTC",
                                     "US/Mountain","US/Eastern","US/Pacific","US/Central","Asia/Bahrain"))
      
      # STill not mapped ... GST  SCT PDT CDT CSt ESY CSC ADT UNK  
      
      dataNew <- merge(dataSum,timeZones, by = "TIME_ZONE", all = TRUE)
      
      dataNew <- merge(dataSum,timeZones, by = "TIME_ZONE", all = TRUE)
      
      # Get the average lat and long for each state
      dataNew <- merge(dataSum,states, by = "STATE", all = TRUE)
      
      # some Event Types are in mixed case, do avoid duplication convert them to UPPER
      dataNew$EVTYPE <- toupper(dataNew$EVTYPE)
      
      #Order the data by the latest date just for more complete data in head commands
      dataNew<- dataNew[order(-dataNew$REFNUM),] 
      
      
      dataNew <- tbl_df(dataNew)
```
  
  
  ##  Interested in getting the datetime by timezone correctly into one field to help with analysis
  
  
  ```r
      # Get just the date
      beginDate <-  format(strptime(dataNew$BGN_DATE, "%m/%d/%Y"), format = "%m/%d/%Y", tz="", usetz=FALSE)
      
      endDate <-  format(strptime(dataNew$BGN_DATE, "%m/%d/%Y"), format = "%m/%d/%Y", tz="", usetz=FALSE)
      
   
      # Get just the time
      beginTime <-  strftime(strptime(dataNew$BGN_TIME, "%I:%M:%S %p"), format = "%I:%M:%S %p", tz="", usetz = FALSE)
      
      endTime <- strftime(strptime(dataNew$END_TIME, "%I:%M:%S %p"), format = "%I:%M:%S %p", tz="", usetz = FALSE)
     
    
      # Append correct timezone to time
      tzTimeStart <- paste(beginTime,dataNew$tz)  
      
      tzTimeEnd <- paste(endTime,dataNew$tz)
     
      # combine dates and times 
      
      beginDT <-  paste(beginDate, tzTimeStart) 
      
      endDT <- paste(endDate, tzTimeEnd)  
    
     # Add the new (text) datetimes to the dataframe 
      dataNew["beginDT"] <- beginDT
      
      dataNew["endDT"] <- endDT
      
      Begin <- sapply( beginDT, function(x) paste(strsplit(x, split = " ")[[1]][1:3], 
                                              collapse = " "))
      Begin <- strptime(as.character(Begin), format = "%m/%d/%Y %I:%M:%S %p")
      
      
      End <- sapply( beginDT, function(x) paste(strsplit(x, split = " ")[[1]][1:3], 
                                                  collapse = " "))
      End <- strptime(as.character(End), format = "%m/%d/%Y %I:%M:%S %p")
      
      dataNew <- cbind(dataNew,Begin,End)
  ```
     
#   1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
      

```r
              # Summarize data by fatality
              stormDeath <- 
              dataNew %>%
              group_by(YEAR=as.numeric(format(Begin,"%Y")),MONTH=as.numeric(format(Begin,"%m")),STATE,EVTYPE)  %>%  
              summarize(DEATH=sum(FATALITIES, na.rm=TRUE))  %>%  
              select(YEAR,MONTH,STATE,EVTYPE,DEATH) 
            
              # Order by most recent year
              stormDeath<- stormDeath[order(-stormDeath$YEAR),] 
              
              # Summarize data by injury
              stormCasualty <- 
              subset(dataNew,(INJURIES != 0 | FATALITIES != 0)) %>%
              group_by(YEAR=as.numeric(format(Begin,"%Y")),MONTH=as.numeric(format(Begin,"%m")),STATE,EVTYPE)  %>%  
              #summarize(INJURY=sum(INJURIES),DEATH=sum(FATALITIES))  %>%  
              select(YEAR,MONTH,STATE,EVTYPE,INJURY=sum(INJURIES),DEATH=sum(FATALITIES)) 
```

#   Wordcloud from the Remarks, Deaths vs Injuries 


```r
           # options(scipen=5)
              
            #png("wordcloud_storm.png", width=12,height=8, units='in', res=300)
  
            #png("wordcloud_storm.png", width=1200,height=800)  
               
            pal <- brewer.pal(9,"YlGnBu")
            pal <- pal[-(1:4)]
            
            dataCorpus <- Corpus(VectorSource(stormDeath$EVTYPE))
            
            dataCorpus <- tm_map(dataCorpus, stripWhitespace)
            
            dataCorpus <- tm_map(dataCorpus, removeWords, stopwords("english"))
            
            dataCorpus <- tm_map(dataCorpus, removeWords, c("excessive", "extreme", "unseasonal"))
            
            dataCorpus <- tm_map(dataCorpus, PlainTextDocument)
            
            
            dataCorpus <- tm_map(dataCorpus, stemDocument)
            
            wordcloud(dataCorpus, max.words = 50, random.order = FALSE,
                      rot.per=0.35, use.r.layout=TRUE, colors=pal, main="Events resulting in Death")
```

![](StormTracking/tree/master/Figure-html/wordcloud_storm.png)



```r
            # dev.off()
```
           

           
#   Summarize data where a death or injury occurred by Event Type


```r
            TopCasualtyYear <- 
              stormCasualty %>%
              group_by(YEAR,EVTYPE)  %>%  
              summarize(DEATH=sum(DEATH),INJURY=sum(INJURY)) %>%
              select(YEAR,EVTYPE,DEATH,INJURY) 
            
            TopInj <- 
              stormCasualty %>%
              group_by(EVTYPE)  %>%  
              summarize(INJURY=sum(INJURY, na.rm=TRUE))  %>%  
              select(EVTYPE,INJURY) 
            
            TopDeath <- 
              stormCasualty %>%
              group_by(EVTYPE)  %>%  
              summarize(DEATH=sum(DEATH, na.rm=TRUE))  %>%  
              select(EVTYPE,DEATH) 
            
            TopCasualty <- 
              TopCasualtyYear %>%
              group_by(EVTYPE)  %>%
              summarize(DEATH=sum(DEATH),INJURY=sum(INJURY)) %>%
              select(EVTYPE,DEATH,INJURY) 
            
             
            #Get the top 20 death causing weather types    
            TopInj<- TopInj[order(-TopInj$INJURY),]
       
            TopInj<- head(TopInj,20)
            
        
            #Get the top 20 death causing weather types    
            TopDeath <- TopDeath[order(-TopDeath$DEATH),]
            
            TopDeath <- head(TopDeath,20)
            
            # Injury and Death top 20
            stormCS <- merge(TopInj,TopDeath, by = "EVTYPE", all = TRUE)
            
            #Identify the major weather event types
            majorEvents <- unique(stormCS$EVTYPE)
            
            
            #Take the top 20 weather events for the top casualty analysis
            TopCasualtyYear <- TopCasualtyYear[TopCasualtyYear$EVTYPE %in% (majorEvents),]
```
 
#plot the top 20 causes of death and injury.  
 

```r
            #options(scipen=5)
            
              #png("StormCasualty.png", width=1200,height=800) 
            
              ggplot(stormCS, aes(DEATH, INJURY), width=1200,height=800) +
              geom_point(color = 'red') +
              geom_text_repel(aes(label = stormCS$EVTYPE)) +
              theme_classic(base_size = 16)
```

```
## Warning: Removed 16 rows containing missing values (geom_point).
```

```
## Warning: Removed 16 rows containing missing values (geom_text_repel).
```

![](StormTracking/tree/master/Figure-html/StormCasualty.png)

```r
              #dev.off()
```


#2.   Across the United States, which types of events have the greatest economic consequences?
 
##Summrize data by cost to property and crops       

```r
            stormCost <- 
                   dataNew %>%
                   group_by(STATE,longitude,latitude,EVTYPE,PROPDMGEXP,CROPDMGEXP)  %>%  
                   summarize(PROP=sum(PROPDMG),CROP=sum(CROPDMG))  %>%  
                   select(STATE,longitude,latitude,EVTYPE,PROP,PROPDMGEXP,CROP,CROPDMGEXP) 
            
            stormCost <- subset(stormCost,(PROP != 0 | CROP != 0)) 
       
           
             # Divide K values by 1000 to express them as millions
 
            stormCost$PROP <- ifelse(stormCost$PROPDMGEXP == "K",stormCost$PROP <- stormCost$PROP/1000, stormCost$PROP)
 
            stormCost$CROP <- ifelse(stormCost$CROPDMGEXP == "K",stormCost$CROP <- stormCost$CROP/1000, stormCost$CROP)
            
            
            TopCost <-
             stormCost %>%
             group_by(EVTYPE) %>%
             summarize(PROP=sum(PROP),CROP=sum(CROP)) %>%
             select(EVTYPE,PROP,CROP)
            
            #Get the top 5 Prop Cost Events    
            TopPropCost<- TopCost[order(-TopCost$PROP),]
            
            TopPropCost <-  head(TopPropCost,5)
            
            #Just look at the top five weather event types by prop damage cost
            stormPropFive <- merge(stormCost,TopPropCost, by = "EVTYPE")
            
            #SHOW OUTPUT FOR THESE  
            
            #Get the top 5 Prop Cost Events    
            TopCropCost<- TopCost[order(-TopCost$CROP),]
            
            TopCropCost <-  head(TopCropCost,5)
            
            #Just look at the top five weather event types by Crop damage cost
            stormCropFive <- merge(stormCost,TopCropCost, by = "EVTYPE")
```
            
           
##Graph of Top 5 weather events by cost to propery and crops


```r
           # options(scipen=5)
              
            #png("wordcloud_storm.png", width=12,height=8, units='in', res=300)
  
          #  png("StormDamage.png", width=1200,height=800)  
            
            vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
            
           
          
            map <- get_map(location = 'United States', zoom = 4) 
```

```
## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=United+States&zoom=4&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false
```

```
## Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=United%20States&sensor=false
```

```r
            mapProp <- ggmap(map) +
              geom_point(aes(x = stormPropFive$longitude, y = stormPropFive$latitude, size = stormPropFive$PROP.x, 
                             col=stormPropFive$EVTYPE), data = stormPropFive, alpha = 1) 
            # + scale_size_continuous(range=range(stormPropFive$PROP.x))

            mapCrop <- ggmap(map) +
              geom_point(aes(x = stormCropFive$longitude, y = stormCropFive$latitude, size = stormCropFive$CROP.x, 
                             col=stormCropFive$EVTYPE), data = stormCropFive, alpha = 1) 
                         # + scale_size_continuous(range=range(stormCropFive$PROP.x))) 
              
            
            
            grid.newpage()
            
            pushViewport(viewport(layout = grid.layout(1, 2),width=1, height=1))
            
            print(mapProp, vp = vplayout(1, 1))
```

```
## Warning: Removed 50 rows containing missing values (geom_point).
```

```r
            print(mapCrop, vp = vplayout(1, 2))
```

```
## Warning: Removed 50 rows containing missing values (geom_point).
```

![](StormTracking/tree/master/Figure-html/StormDamage.png)

```r
           # dev.off()
```

