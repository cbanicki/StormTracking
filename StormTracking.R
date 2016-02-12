


  StormTracking <- function() {
    
  library(dplyr) #
  require(data.table) #
  require(lubridate) #
  require(ggplot2) #
  require(ggrepel) #
  require(ggmap) #
  require(rworldmap)
  require(scales)
  require(tm)
  require(SnowballC)
  require(wordcloud) #
   
# library(grid)
  
  setwd("C://R//RR//P2")
  
  #####################################################################################################
  #                                           Collect Data
  #####################################################################################################
  
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
    
    states <- read.csv("StatesLatLong.csv", header = TRUE, na.strings = c("NA"))
    

    #data<- data[order(-data$REFNUM),] 
    
    dataSum <- tbl_df(data)
    
    dataCorpus <- Corpus(VectorSource(dataSum$REMARKS))
    
    # eopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
    
    rm(data)
    
#     timeZones <- data.frame(TIME_ZONE = c("CST","AKS","MST","PST","EST","ESt","HST","SST","AST","GMT","UTC","MDT","EDT","PDT","CDT","GST"),
#                             tz = c("CST6CDT","America/Anchorage","MST7MDT","PST8PDT","EST5EDT","EST5EDT","HST","Pacific/Samoa","America/Puerto_Rico","GMT","UTC",
#                                    "US/Mountain","US/Eastern","US/Pacific","US/Central","Asia/Bahrain"))
    
    #  STill not mapped ... GST  SCT PDT CDT CSt ESY CSC ADT UNK  
    
#     dataNew <- merge(dataSum,timeZones, by = "TIME_ZONE", all = TRUE)
#     
#     dataNew <- merge(dataSum,timeZones, by = "TIME_ZONE", all = TRUE)
    
    # Get the average lat and long for each state
    dataNew <- merge(dataSum,states, by = "STATE", all = TRUE)
    
    # some Event Types are in mixed case, do avoid duplication convert them to UPPER
    dataNew$EVTYPE <- toupper(dataNew$EVTYPE)
    
    #Order the data by the latest date just for more complete data in head commands
    dataNew<- dataNew[order(-dataNew$REFNUM),] 
    
    
    dataNew <- tbl_df(dataNew)
    
    #------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #                 Interested in getting the datetime by timezone correctly into one field to help with analysis
    # -----------------------------------------------------------------------------------------------------------------------------------------------------------------
   
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
    
    
    # How can I capture these using the correct timezone???
    
    Begin <- sapply( beginDT, function(x) paste(strsplit(x, split = " ")[[1]][1:3], 
                                            collapse = " "))
    Begin <- strptime(as.character(Begin), format = "%m/%d/%Y %I:%M:%S %p")
    
    
    End <- sapply( beginDT, function(x) paste(strsplit(x, split = " ")[[1]][1:3], 
                                                collapse = " "))
    End <- strptime(as.character(End), format = "%m/%d/%Y %I:%M:%S %p")
    
    dataNew <- cbind(dataNew,Begin,End)
    
    

      #----------------------------------------------------------------------------------------------------------------------------------------------           
      
     
       # Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
      
      
      #----------------------------------------------------------------------------------------------------------------------------------------------           
    
    
              stormDeath <- 
              dataNew %>%
              group_by(YEAR=as.numeric(format(Begin,"%Y")),MONTH=as.numeric(format(Begin,"%m")),STATE,EVTYPE)  %>%  
              summarize(DEATH=sum(FATALITIES, na.rm=TRUE))  %>%  
              select(YEAR,MONTH,STATE,EVTYPE,DEATH) 
            
              # Order by most recent year
              stormDeath<- stormDeath[order(-stormDeath$YEAR),] 
              
#               stormHurt <- 
#               dataNew %>%
#               group_by(YEAR=as.numeric(format(Begin,"%Y")),MONTH=as.numeric(format(Begin,"%m")),STATE,EVTYPE)  %>%  
#               summarize(INJURY=sum(INJURIES, na.rm=TRUE))  %>%  
#               select(YEAR,MONTH,STATE,EVTYPE,INJURY) 
#             
#               # Order by most recent year
#               stormhURT<- stormHurt[order(-stormHurt$YEAR),]
              
            
              stormCasualty <- 
              subset(dataNew,(INJURIES != 0 | FATALITIES != 0)) %>%
              group_by(YEAR=as.numeric(format(Begin,"%Y")),MONTH=as.numeric(format(Begin,"%m")),STATE,EVTYPE)  %>%  
              #summarize(INJURY=sum(INJURIES),DEATH=sum(FATALITIES))  %>%  
              select(YEAR,MONTH,STATE,EVTYPE,INJURY=sum(INJURIES),DEATH=sum(FATALITIES)) 
            
            
 #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------           
                                                         # Wordcloud for Remarks,  Deaths vs Injuries
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------            
            # par(mfrow=c(1,2))
            
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
            

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------            
      #Summarize data where a death or injury occurred
         
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
           
          # plot the top 20 causes of death and injury.  
            ggplot(stormCS, aes(DEATH, INJURY)) +
              geom_point(color = 'red') +
              geom_text_repel(aes(label = stormCS$EVTYPE)) +
              theme_classic(base_size = 16)
         

    #----------------------------------------------------------------------------------------------------------------------------------------------           
          
    #  2.   Across the United States, which types of events have the greatest economic consequences?
        
    #----------------------------------------------------------------------------------------------------------------------------------------------           
            
            
           # PROPDMG	PROPDMGEXP	CROPDMG	CROPDMGEXP
            

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
            
            #require(mapproj)
            #require(maps)
            library(ggmap)
          
            
          
            map <- get_map(location = 'United States', zoom = 4) 
            
            
            mapProp <- ggmap(map) +
              geom_point(aes(x = stormPropFive$longitude, y = stormPropFive$latitude, size = stormPropFive$PROP.x, col=stormPropFive$EVTYPE), data = stormPropFive, alpha = .5)

                         
                         
                         # #             
# #             mapPropFacets <- mapProp +
# #                 facet_grid(stormPropFive$EVTYPE ~ .)
#             
#             mapPropFacets
            
            mapCrop <- ggmap(map) +
              geom_point(aes(x = stormCropFive$longitude, y = stormCropFive$latitude, size = stormCropFive$CROP.x, col=stormCropFive$EVTYPE), data = stormCropFive, alpha = .5)
           
            par(mfrow=c(1,2))
             
            mapProp
            
            mapCrop
            
    
            

    }

)
# return(output)

}

