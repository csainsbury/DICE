library(data.table)

returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%d/%m/%Y", tz="GMT"))
  return(returnVal)
}

## NOTE 11/5/17
# for hba1c variability need to take wide window pre/post, given the low frequency of hba1c measurement. 2y pre vs 2y post. for other measures, benefits are seen on a shorter timescale - 6m for peak efficacy of hba1c reduction and 1y pre/post for reduction in admissions.

# load admissions
T1_admissions<-read.csv("~/R/GlCoSy/source/admissionDataDT_T1DM.csv")
T1_admissions_sub<-data.frame(T1_admissions$ID,T1_admissions$dateplustime1,T1_admissions$admissionNumberFlag,T1_admissions$nCBGperAdmission,T1_admissions$admissionDurationDays, T1_admissions$IQR, T1_admissions$diagnosisDateUnix); colnames(T1_admissions_sub)<-c("ID","dateplustime1","admissionNumberFlag","nCBGperAdmission","admissionDurationDays", "IQR", "diagnosisDateUnix")

T2_admissions<-read.csv("~/R/GlCoSy/source/admissionDataDT_T2DM.csv")
T2_admissions_sub<-data.frame(T2_admissions$ID,T2_admissions$dateplustime1,T2_admissions$admissionNumberFlag,T2_admissions$nCBGperAdmission,T2_admissions$admissionDurationDays, T2_admissions$IQR, T2_admissions$diagnosisDateUnix); colnames(T2_admissions_sub)<-c("ID","dateplustime1","admissionNumberFlag","nCBGperAdmission","admissionDurationDays", "IQR", "diagnosisDateUnix")

admissions<-rbind(T1_admissions_sub,T2_admissions_sub)
admissionsDT<-data.table(admissions)

lastAdmssionDate <- max(admissionsDT$dateplustime1)

demogALL<-read.csv("~/R/GlCoSy/SDsource/diagnosisDateDeathDate.txt")
demogALL$diagnosis_unix <- as.numeric(as.POSIXct(demogALL$DateOfDiagnosisDiabetes_Date, format="%Y-%m-%d", tz="GMT"))
demogALLDT <- data.table(demogALL)
diagnosis_id <- data.table(demogALLDT$PatId, demogALLDT$diagnosis_unix); colnames(diagnosis_id) <- c("PatId", "diagnosis_unix")

id_lookup <- data.frame(demogALL$LinkId, demogALL$PatId); colnames(id_lookup) <- c("LinkId", "PatId")

#import hba1c data
cleanHbA1cData <- read.csv("~/R/GlCoSy/SD_workingSource/hba1cDTclean.csv", sep=",", header = TRUE, row.names = NULL)
cleanHbA1cData$timeSeriesDataPoint <- cleanHbA1cData$hba1cNumeric
cleanHbA1cDataDT <- data.table(cleanHbA1cData)

hba1c_withID <- merge(cleanHbA1cData, id_lookup, by.x = "LinkId", by.y = "LinkId")

# import DICE data
diceData <- read.csv("~/R/GlCoSy/SDsource/diceDAFNE.csv", sep=",", header = TRUE, row.names = NULL)
diceData$DICE_unix <- returnUnixDateTime(diceData$Date.attended.DICE)

# add diagnosis data
diceData <- merge(diceData, diagnosis_id, by.x = "CHI", by.y = "PatId")

            ###########
            # choose whether to subset by diagnosis date:
            diceData$timeToDICEfromDIagnosis <- diceData$DICE_unix - diceData$diagnosis_unix
            diceData$timeToDICEfromDIagnosis_years <- diceData$timeToDICEfromDIagnosis / (60*60*24*365.25)
            
            diceData <- subset(diceData, timeToDICEfromDIagnosis_years >= 1.5 )
            ###########

# combined DICE hba1c data
diceHbA1c <- merge(hba1c_withID, diceData, by.x = "PatId", by.y = "CHI")
diceHbA1cDT <- data.table(diceHbA1c)

#####
# simple plots
diceHbA1cDT$timeRelativeToDICE <- diceHbA1cDT$dateplustime1 - diceHbA1cDT$DICE_unix
diceHbA1cDT$timeRelativeToDICE_years <- diceHbA1cDT$timeRelativeToDICE / (60*60*24*365.25)

x <- boxplot(diceHbA1cDT$timeSeriesDataPoint ~ cut(diceHbA1cDT$timeRelativeToDICE_years, breaks = seq(-1,5,1)), varwidth = T, ylim = c(55,85))

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

#####
# hba1c variability pre/post
# set pre/post times

idList <- unique(diceHbA1cDT$LinkId)

report_IQR_Frame <- as.data.frame(matrix(nrow = length(idList), ncol = 5))
colnames(report_IQR_Frame) <- c("id", "IQR_pre", "IQR_post", "n_pre", "n_post")
report_IQR_Frame$id <- idList

for (j in seq(1, length(idList), 1)) {
  plotSet <- diceHbA1cDT[LinkId == idList[j]]
  preSet <- plotSet[timeRelativeToDICE_years > -2 & timeRelativeToDICE_years <= 0]
  postSet <- plotSet[timeRelativeToDICE_years > 0 & timeRelativeToDICE_years < 2]
  
  IQR_pre <- quantile(preSet$timeSeriesDataPoint)[4] - quantile(preSet$timeSeriesDataPoint)[2]
  IQR_post <- quantile(postSet$timeSeriesDataPoint)[4] - quantile(postSet$timeSeriesDataPoint)[2]
  
  plot_x <- c(0, 1)
  plot_y <- c(IQR_pre, IQR_post)

  if (j == 1) {
    plot(plot_x, plot_y, xlim = c(-0.5, 1.5), ylim = c(0,40), cex = sqrt(nrow(preSet)))
    lines(plot_x, plot_y, col = rgb(0, 0, 0, 0.2, maxColorValue = 1))
  }
  if (j > 1) {
    points(plot_x, plot_y, cex = sqrt(nrow(postSet)))
    lines(plot_x, plot_y, col = rgb(0, 0, 0, 0.2, maxColorValue = 1))
  }
  
  report_IQR_Frame$IQR_pre[j] <- IQR_pre
  report_IQR_Frame$IQR_post[j] <- IQR_post
  report_IQR_Frame$n_pre[j] <- nrow(preSet)
  report_IQR_Frame$n_post[j] <- nrow(postSet)
  
}

print(quantile(report_IQR_Frame$IQR_pre, na.rm = T))
print(quantile(report_IQR_Frame$IQR_post, na.rm = T))
wilcox.test(report_IQR_Frame$IQR_pre, report_IQR_Frame$IQR_post, paired = T)

#####
# admissions pre/post

# admissionsDT<-admissionsDT[nCBGperAdmission>2]

windowOfInterestYears <- 0.75
windowOfInterestSeconds <- windowOfInterestYears * (60*60*24*365.25)
admissionIdList <- unique(diceHbA1cDT[DICE_unix < (lastAdmssionDate - windowOfInterestSeconds)]$PatId)

report_admission_Frame <- as.data.frame(matrix(nrow = length(admissionIdList), ncol = 5))
colnames(report_admission_Frame) <- c("id", "adm_pre", "adm_post", "admIQR_pre", "admIQR_post")
report_admission_Frame$id <- admissionIdList

for (jj in seq(1, length(admissionIdList), 1)) {
  admissionsSet <- admissionsDT[ID == admissionIdList[jj]]
  diceDate <- diceHbA1cDT[PatId == admissionIdList[jj]]$DICE_unix[1]
  
  admissionsSet$timeRelativeToDICE <- admissionsSet$dateplustime1 - diceDate
  admissionsSet$timeRelativeToDICE_years <- admissionsSet$timeRelativeToDICE / (60*60*24*365.25)
  
  preSet <- admissionsSet[timeRelativeToDICE_years > -windowOfInterestYears & timeRelativeToDICE_years <= 0]
  postSet <- admissionsSet[timeRelativeToDICE_years > 0 & timeRelativeToDICE_years < windowOfInterestYears]
  
  report_admission_Frame$adm_pre[jj] <- nrow(preSet)
  report_admission_Frame$adm_post[jj] <- nrow(postSet)
  
    IQR_preSet <- preSet[nCBGperAdmission > 1]
    IQR_postSet <- postSet[nCBGperAdmission > 1]
    
}

quantile(report_admission_Frame$adm_pre)
quantile(report_admission_Frame$adm_post)

sum(report_admission_Frame$adm_pre)
sum(report_admission_Frame$adm_post)

atLeastOneAdmissionPre <- ifelse(report_admission_Frame$adm_pre > 0, 1, 0)
atLeastOneAdmissionPost <- ifelse(report_admission_Frame$adm_post > 0, 1, 0)

wilcox.test(report_admission_Frame$adm_pre, report_admission_Frame$adm_post, paired = T)
wilcox.test(atLeastOneAdmissionPre, atLeastOneAdmissionPost, paired = T)
prop.test(c(sum(atLeastOneAdmissionPre), sum(atLeastOneAdmissionPost)), c(length(admissionIdList), length(admissionIdList)))

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

diceHbA1cDT[, c("firstHbA1c", "secondHbA1c") :=  findHbA1cValues(LinkId, DICE_unix, 6, 18), by=.(LinkId)]
diceHbA1cDT$include <- ifelse(diceHbA1cDT$firstHbA1c > 0 & diceHbA1cDT$secondHbA1c >0, 1, 0)

# flag single row per ID for merging back with combination data
diceHbA1cDT$index <- seq(1, nrow(diceHbA1cDT), 1)
diceHbA1cDT[, c("firstRow") :=  ifelse(index == min(index), 1, 0), by=.(LinkId)]

diceHbA1cDT_perID <- diceHbA1cDT[firstRow == 1]

analysisSet_6months <- diceHbA1cDT_perID[include == 1]
analysisSet_9months <- diceHbA1cDT_perID[include == 1]
analysisSet_12months <- diceHbA1cDT_perID[include == 1]
analysisSet_18months <- diceHbA1cDT_perID[include == 1]

    print(nrow(analysisSet_6months))
    print(quantile(analysisSet_6months$firstHbA1c))
    print(quantile(analysisSet_6months$secondHbA1c))
    wilcox.test(analysisSet_6months$firstHbA1c, analysisSet_6months$secondHbA1c, paired = T)
    
      analysisSet_6months_58plus <- subset(analysisSet_6months, firstHbA1c >= 58)
      print(nrow(analysisSet_6months_58plus))
      print(quantile(analysisSet_6months_58plus$firstHbA1c))
      print(quantile(analysisSet_6months_58plus$secondHbA1c))
      wilcox.test(analysisSet_6months_58plus$firstHbA1c, analysisSet_6months_58plus$secondHbA1c, paired = T)
    
    print(nrow(analysisSet_12months))
    print(quantile(analysisSet_12months$firstHbA1c))
    print(quantile(analysisSet_12months$secondHbA1c))
    wilcox.test(analysisSet_12months$firstHbA1c, analysisSet_12months$secondHbA1c)
    
      analysisSet_12months_58plus <- subset(analysisSet_12months, firstHbA1c >= 58)
      print(nrow(analysisSet_12months_58plus))
      print(quantile(analysisSet_12months_58plus$firstHbA1c))
      print(quantile(analysisSet_12months_58plus$secondHbA1c))
      wilcox.test(analysisSet_12months_58plus$firstHbA1c, analysisSet_12months_58plus$secondHbA1c)
      
    print(nrow(analysisSet_18months))
    print(quantile(analysisSet_18months$firstHbA1c))
    print(quantile(analysisSet_18months$secondHbA1c))
    wilcox.test(analysisSet_18months$firstHbA1c, analysisSet_18months$secondHbA1c)
    
      analysisSet_18months_58plus <- subset(analysisSet_18months, firstHbA1c >= 58)
      print(nrow(analysisSet_18months_58plus))
      print(quantile(analysisSet_18months_58plus$firstHbA1c))
      print(quantile(analysisSet_18months_58plus$secondHbA1c))
      wilcox.test(analysisSet_18months_58plus$firstHbA1c, analysisSet_18months_58plus$secondHbA1c)


reportingFrame <- as.data.frame(matrix(nrow = 0, ncol = 5))
colnames(reportingFrame) <- c("months", "n", "median", "IQR1", "IQR2")

increment = 3

for (i in seq(3, 24, increment)) {
  
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