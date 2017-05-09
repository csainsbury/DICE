library(data.table)


returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%d/%m/%Y", tz="GMT"))
  return(returnVal)
}

demogALL<-read.csv("~/R/GlCoSy/SDsource/diagnosisDateDeathDate.txt")
id_lookup <- data.frame(demogALL$LinkId, demogALL$PatId); colnames(id_lookup) <- c("LinkId", "PatId")

#import hba1c data
cleanHbA1cData <- read.csv("~/R/GlCoSy/SD_workingSource/hba1cDTclean.csv", sep=",", header = TRUE, row.names = NULL)
cleanHbA1cData$timeSeriesDataPoint <- cleanHbA1cData$hba1cNumeric

hba1c_withID <- merge(cleanHbA1cData, id_lookup, by.x = "LinkId", by.y = "LinkId")

# import DICE data
diceData <- read.csv("~/R/GlCoSy/SDsource/diceDAFNE.csv", sep=",", header = TRUE, row.names = NULL)
diceData$DICE_unix <- returnUnixDateTime(diceData$Date.attended.DICE)

diceHbA1c <- merge(hba1c_withID, diceData)


























# file to find hba1c values for 
findHbA1cValues <- function(LinkId_value, firstSGLT2Prescription, firstWindowMonths, IntervalMonths) {
  
  # print(LinkId_value)
  
  firstSGLT2time <- firstSGLT2Prescription[1]
  
  firstWindowSeconds <- firstWindowMonths * (60*60*24*(365.25/12))
  IntervalSeconds <- IntervalMonths * (60*60*24*(365.25/12))
  
  hb_sub <- cleanHbA1cDataDT[LinkId == LinkId_value]
  
  # find 1st hba1c
  hb_sub$firstDiff <- hb_sub$dateplustime1 - firstSGLT2time
  first_hb_sub <- hb_sub[sqrt(firstDiff^2) < (firstWindowSeconds/2)]
  if (nrow(first_hb_sub) > 0) {firstHb <- first_hb_sub[sqrt(firstDiff^2) == min(sqrt(firstDiff^2))]$timeSeriesDataPoint}
  if (nrow(first_hb_sub) == 0) {firstHb = 0}
  
  # find 2nd hba1c
  hb_sub$secondDiff <- hb_sub$dateplustime1 - (firstSGLT2time + IntervalSeconds)
  second_hb_sub <- hb_sub[sqrt(secondDiff^2) < (firstWindowSeconds/2)]
  if (nrow(second_hb_sub) > 0) {secondHb <- second_hb_sub[sqrt(firstDiff^2) == min(sqrt(firstDiff^2))]$timeSeriesDataPoint}
  if (nrow(second_hb_sub) == 0) {secondHb = 0}
  
  returnList <- list(firstHb, secondHb)
  
  return(returnList)
  
}

drugsetDT[, c("firstHbA1c", "secondHbA1c") :=  findHbA1cValues(LinkId, firstSGLT2Prescription, 3, 12), by=.(LinkId)]

drugsetDT$include <- ifelse(drugsetDT$firstHbA1c > 0 & drugsetDT$secondHbA1c >0, 1, 0)
drugsetDT$y <- ifelse(drugsetDT$include == 1 & (drugsetDT$firstHbA1c - drugsetDT$secondHbA1c) >= 10, 1, 0)

# flag single row per ID for merging back with combination data
drugsetDT$index <- seq(1, nrow(drugsetDT), 1)
drugsetDT[, c("firstRow") :=  ifelse(index == min(index), 1, 0), by=.(LinkId)]

meetsCriteriaDT <- drugsetDT[include == 1 & firstRow == 1]
mergeSet <- data.frame(meetsCriteriaDT$LinkId, meetsCriteriaDT$y); colnames(mergeSet) <- c("LinkId", "y")

exportMerge <- merge(mergeSet, drugWordFrame, by.x = "LinkId", by.y = "LinkId")