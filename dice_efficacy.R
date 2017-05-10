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

# combined DICE hba1c data
diceHbA1c <- merge(hba1c_withID, diceData, by.x = "PatId", by.y = "CHI")
diceHbA1cDT <- data.table(diceHbA1c)

# simple plot
diceHbA1cDT$timeRelativeToDICE <- diceHbA1cDT$dateplustime1 - diceHbA1cDT$DICE_unix
diceHbA1cDT$timeRelativeToDICE_years <- diceHbA1cDT$timeRelativeToDICE / (60*60*24*365.25)

boxplot(diceHbA1cDT$timeSeriesDataPoint ~ cut(diceHbA1cDT$timeRelativeToDICE_years, breaks = seq(-1,5,1)), varwidth = T, ylim = c(55,85))

idList <- unique(diceHbA1cDT$LinkId)

for (j in seq(1, length(idList), 1)) {
  plotSet <- diceHbA1cDT[LinkId == idList[j]]
  plotSet <- plotSet[order(plotSet$timeRelativeToDICE_years), ]
  
  if (j == 1) {
    plot(plotSet$timeRelativeToDICE_years, plotSet$timeSeriesDataPoint, cex = 0, xlim = c(-10, 8), ylim = c(40, 120))
    lines(plotSet$timeRelativeToDICE_years, plotSet$timeSeriesDataPoint, col = rgb(0, 0, 0, 0.05, maxColorValue = 1))
  }
  if (j > 1) {
    points(plotSet$timeRelativeToDICE_years, plotSet$timeSeriesDataPoint, cex = 0)
    lines(plotSet$timeRelativeToDICE_years, plotSet$timeSeriesDataPoint, col = rgb(0, 0, 0, 0.05, maxColorValue = 1))
  }
}

abline(v = 0, col = rgb(1, 0, 0, 0.5, maxColorValue = 1))

# file to find hba1c values for 
findHbA1cValues <- function(LinkId_value, firstSGLT2Prescription, firstWindowMonths, IntervalMonths) {
  
  # print(LinkId_value)
  
  firstSGLT2time <- firstSGLT2Prescription[1]
  
  firstWindowSeconds <- firstWindowMonths * (60*60*24*(365.25/12))
  IntervalSeconds <- IntervalMonths * (60*60*24*(365.25/12))
  
  hb_sub <- cleanHbA1cDataDT[LinkId == LinkId_value]
  
  # find 1st hba1c
  hb_sub$firstDiff <- hb_sub$dateplustime1 - firstSGLT2time
  first_hb_sub <- hb_sub[firstDiff <= 0 & firstDiff > (0 - firstWindowSeconds)]
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

diceHbA1cDT[, c("firstHbA1c", "secondHbA1c") :=  findHbA1cValues(LinkId, DICE_unix, 6, 6), by=.(LinkId)]
diceHbA1cDT$include <- ifelse(diceHbA1cDT$firstHbA1c > 0 & diceHbA1cDT$secondHbA1c >0, 1, 0)

# flag single row per ID for merging back with combination data
diceHbA1cDT$index <- seq(1, nrow(diceHbA1cDT), 1)
diceHbA1cDT[, c("firstRow") :=  ifelse(index == min(index), 1, 0), by=.(LinkId)]

diceHbA1cDT_perID <- diceHbA1cDT[firstRow == 1]

analysisSet_6months <- diceHbA1cDT_perID[include == 1]
analysisSet_9months <- diceHbA1cDT_perID[include == 1]
analysisSet_12months <- diceHbA1cDT_perID[include == 1]
analysisSet_18months <- diceHbA1cDT_perID[include == 1]

reportingFrame <- as.data.frame(matrix(nrow = 0, ncol = 5))
colnames(reportingFrame) <- c("months", "n", "median", "IQR1", "IQR2")

increment = 4

for (i in seq(4, 48, increment)) {
  
  print(i)

    # file to find hba1c values for 
    findHbA1cValues <- function(LinkId_value, firstSGLT2Prescription, firstWindowMonths, secondWindowMonths, IntervalMonths) {
      
      # print(LinkId_value)
      
      firstSGLT2time <- firstSGLT2Prescription[1]
      
      firstWindowSeconds <- firstWindowMonths * (60*60*24*(365.25/12))
      secondWindowSeconds <- secondWindowMonths * (60*60*24*(365.25/12))
      IntervalSeconds <- IntervalMonths * (60*60*24*(365.25/12))
      
      hb_sub <- cleanHbA1cDataDT[LinkId == LinkId_value]
      
      # find 1st hba1c
      hb_sub$firstDiff <- hb_sub$dateplustime1 - firstSGLT2time
      first_hb_sub <- hb_sub[firstDiff <= 0 & firstDiff > (0 - firstWindowSeconds)]
      if (nrow(first_hb_sub) > 0) {firstHb <- first_hb_sub[sqrt(firstDiff^2) == min(sqrt(firstDiff^2))]$timeSeriesDataPoint}
      if (nrow(first_hb_sub) == 0) {firstHb = 0}
      
      # find 2nd hba1c
      hb_sub$secondDiff <- hb_sub$dateplustime1 - (firstSGLT2time + IntervalSeconds)
      second_hb_sub <- hb_sub[sqrt(secondDiff^2) < (secondWindowSeconds/2)]
      if (nrow(second_hb_sub) > 0) {secondHb <- second_hb_sub[sqrt(firstDiff^2) == min(sqrt(firstDiff^2))]$timeSeriesDataPoint}
      if (nrow(second_hb_sub) == 0) {secondHb = 0}
      
      returnList <- list(firstHb, secondHb)
      
      return(returnList)
      
    }
    
    diceHbA1cDT[, c("firstHbA1c", "secondHbA1c") :=  findHbA1cValues(LinkId, DICE_unix, 6, increment, i), by=.(LinkId)]
    diceHbA1cDT$include <- ifelse(diceHbA1cDT$firstHbA1c > 0 & diceHbA1cDT$secondHbA1c >0, 1, 0)
    
    # flag single row per ID for merging back with combination data
    diceHbA1cDT$index <- seq(1, nrow(diceHbA1cDT), 1)
    diceHbA1cDT[, c("firstRow") :=  ifelse(index == min(index), 1, 0), by=.(LinkId)]

diceHbA1cDT_perID <- diceHbA1cDT[firstRow == 1]
analysisSet <- diceHbA1cDT_perID[include == 1]

medianDiff <- quantile(analysisSet$secondHbA1c)[3] - quantile(analysisSet$firstHbA1c)[3]
IQR1_diff  <- quantile(analysisSet$secondHbA1c)[2] - quantile(analysisSet$firstHbA1c)[2]
IQR2_diff  <- quantile(analysisSet$secondHbA1c)[4] - quantile(analysisSet$firstHbA1c)[4]

reportSet <- data.frame(i, nrow(analysisSet), medianDiff, IQR1_diff, IQR2_diff)
colnames(reportSet) <- c("months", "n", "median", "IQR1", "IQR2")

reportingFrame <- rbind(reportingFrame, reportSet)


}

plot(reportingFrame$months, reportingFrame$n)

plot(reportingFrame$months, reportingFrame$median, pch = 16, cex = 2)
fit <- lm(reportingFrame$median ~ reportingFrame$month)
abline(fit, col = "red", lwd = 3)

lines(reportingFrame$months, reportingFrame$median)
lines(reportingFrame$months, reportingFrame$IQR1, col = "red")

analysisSet_6months <- diceHbA1cDT_perID[include == 1]
analysisSet_9months <- diceHbA1cDT_perID[include == 1]
analysisSet_12months <- diceHbA1cDT_perID[include == 1]
analysisSet_18months <- diceHbA1cDT_perID[include == 1]




















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