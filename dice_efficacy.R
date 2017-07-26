library(data.table)

set.seed(43)
 
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
demogALL$dob_unix <- as.numeric(as.POSIXct(demogALL$BirthDate, format="%Y-%m-%d", tz="GMT"))
demogALL$diagnosis_unix <- as.numeric(as.POSIXct(demogALL$DateOfDiagnosisDiabetes_Date, format="%Y-%m-%d", tz="GMT"))
demogALLDT <- data.table(demogALL)
diagnosis_id <- data.table(demogALLDT$PatId, demogALLDT$dob_unix, demogALLDT$DiabetesMellitusType_Mapped, demogALLDT$diagnosis_unix); colnames(diagnosis_id) <- c("PatId", "dob_unix", "diabetes_type","diagnosis_unix")

id_lookup <- data.frame(demogALL$LinkId, demogALL$PatId); colnames(id_lookup) <- c("LinkId", "PatId")

#import hba1c data
cleanHbA1cData <- read.csv("~/R/GlCoSy/SD_workingSource/hba1cDTclean.csv", sep=",", header = TRUE, row.names = NULL)
cleanHbA1cDataDT <- data.table(cleanHbA1cData)
cleanHbA1cDataDT[, c("lastA1cDate") := max(dateplustime1) , by=.(LinkId)]

hba1c_withID <- merge(cleanHbA1cDataDT, id_lookup, by = "LinkId")
lastA1cFrame <- data.frame(hba1c_withID$LinkId, hba1c_withID$lastA1cDate); colnames(lastA1cFrame) <- c("LinkId", "lastA1cDate")

diagnosisExclusionYears <- 1.5
testWindow <- 4
hba1cWindow <- 3
exclusionWindowMonths <- 0
priorHba1cWindowYears <- 1

# import DICE data
# diceData <- read.csv("~/R/GlCoSy/SDsource/diceDAFNE_combined.csv", sep=",", header = TRUE, row.names = NULL)
diceData <- read.csv("~/R/GlCoSy/SDsource/diceDAFNE_annotated.csv", sep=",", header = TRUE, row.names = NULL)
diceData$DICE_unix <- returnUnixDateTime(diceData$Date)

# import pump data
pumpList <- read.csv("~/R/GlCoSy/SDsource/pumpList.csv", sep=",", header = TRUE, row.names = NULL)
pumpList$pumpDate_unix <- returnUnixDateTime(pumpList$pumpdate)

  # merge DICE and pump - cut all pumps in use before end of test window post DICE/DAFNE
  diceData_pump <- merge(diceData, pumpList, by.x = "CHI", by.y = "chi", all.x = T)
    diceData_pump$courseToPump <- (diceData_pump$pumpDate_unix - diceData_pump$DICE_unix) / (60*60*24*365.25)
    diceData_pump$courseToPump[is.na(diceData_pump$courseToPump)] <- 0
  # diceData_pump$useFlag <- ifelse(diceData_pump$pumpDate_unix - diceData_pump$DICE_unix < (testWindow *60*60*24*365.25), 1, 0)
  # diceData_pump$useFlag[is.na(diceData_pump$useFlag)] <- 1
  diceData <- diceData_pump
  
# import pregnancy data
  dice_preg<-read.csv("~/R/GlCoSy/SDsource/dice_pregnancyData_cleaned.csv")
  dafne_preg<-read.csv("~/R/GlCoSy/SDsource/dafne_pregnancyData_cleaned.csv")
  
  # combine pregnancy data
  pregDF <- rbind(dice_preg, dafne_preg); colnames(pregDF) <- c('CHI', 'unix_deliveryDate')
  # merge pregnancy and all data
  diceData_preg <- merge(diceData, pregDF, by.x = "CHI", by.y = "CHI", all.x = T)
  diceData_preg$courseToDelivery <- (diceData_preg$unix_deliveryDate - diceData_preg$DICE_unix) / (60*60*24*365.25)
  diceData_preg$courseToDelivery[is.na(diceData_preg$courseToDelivery)] <- 0
  # diceData_preg$useFlag <- ifelse(diceData_preg$course_preg_interval > 0 & diceData_preg$course_preg_interval < testWindow, 0, 1)
  # diceData_preg$useFlag[is.na(diceData_preg$useFlag)] <- 1
  diceData <- diceData_preg
  
# remove dups - use first only
diceDataDT <- data.table(diceData)
diceDataDT[, c("firstCourse") := ifelse(DICE_unix == min(DICE_unix), 1, 0) , by=.(CHI)]
diceData <- as.data.frame(diceDataDT[firstCourse == 1])

# add diagnosis data
diceData <- merge(diceData, diagnosis_id, by.x = "CHI", by.y = "PatId")

            ###########
            # choose whether to subset by diagnosis date:
            diceData$timeToDICEfromDIagnosis <- diceData$DICE_unix - diceData$diagnosis_unix
            diceData$timeToDICEfromDIagnosis_years <- diceData$timeToDICEfromDIagnosis / (60*60*24*365.25)
            
            diceData <- subset(diceData, timeToDICEfromDIagnosis_years >= diagnosisExclusionYears)
            ###########

# combined DICE hba1c data
diceHbA1c <- merge(hba1c_withID, diceData, by.x = "PatId", by.y = "CHI")
diceHbA1cDT <- data.table(diceHbA1c)

# ensure all have at least one hba1c within 6 months of the end of the followup window
diceHbA1cDT$lastA1cThreshold <- ((diceHbA1cDT$DICE_unix + (testWindow * (60*60*24*365.25))) - hba1cWindow * (60*60*24*365.25))
diceHbA1cDT <- diceHbA1cDT[lastA1cDate > lastA1cThreshold]

diceHbA1cDT$timeRelativeToDICE <- diceHbA1cDT$dateplustime1 - diceHbA1cDT$DICE_unix
diceHbA1cDT$timeRelativeToDICE_years <- diceHbA1cDT$timeRelativeToDICE / (60*60*24*365.25)

diceHbA1cDT_courseComp <- diceHbA1cDT

# exclude hba1c values in window around course
diceHbA1cDT <- diceHbA1cDT[timeRelativeToDICE_years < (-(exclusionWindowMonths / 12)) | timeRelativeToDICE_years > (exclusionWindowMonths / 12)]

# limit to those with an hba1c >=58 prior to course (av during period priorHba1cWindowYears)
average_hba1c_beforeCourse <- function(timeRelativeToDICE_years, hba1cNumeric) {
  flagInWindowPrior <- ifelse(timeRelativeToDICE_years < 0 & timeRelativeToDICE_years > (0 - priorHba1cWindowYears), 1, 0)
  averageHba1c <- quantile(hba1cNumeric[flagInWindowPrior == 1])[3]
  return(averageHba1c)
}

diceHbA1cDT[, c("av_hba1c_priorWindow") := average_hba1c_beforeCourse(timeRelativeToDICE_years, hba1cNumeric) , by=.(LinkId)]
diceHbA1cDT <- diceHbA1cDT[av_hba1c_priorWindow >= 58]

# optional - limit to T1
diceHbA1cDT <- diceHbA1cDT[diabetes_type == 'Type 1 Diabetes Mellitus']

# remove those with no recorded date of diagnosis
diceHbA1cDT <- diceHbA1cDT[diagnosis_unix > (-2208988800)]
# remove those with a recorded HbA1c measuement date in the future
diceHbA1cDT <- diceHbA1cDT[dateplustime1 < returnUnixDateTime('01/01/2017')]


# dice vs dafne
# diceHbA1cDT <- diceHbA1cDT[Course == 'dice']
# diceHbA1cDT <- diceHbA1cDT[Course == 'dafne']

average_hba1c_atTimePoint <- function(timeRelativeToDICE_years, hba1cNumeric, timePointMonths, windowMonths) {
  timePointYears <- timePointMonths / 12
  windowYears <- windowMonths / 12
  
  flagInWindow <- ifelse(timeRelativeToDICE_years > (timePointYears - (windowYears / 2)) & timeRelativeToDICE_years < (timePointYears + (windowYears / 2)), 1, 0)
  averageHba1c <- quantile(hba1cNumeric[flagInWindow == 1], na.rm = T)[3]
  
  return(averageHba1c)
}

diceHbA1cDT[, c("singleRowFlag") := ifelse(dateplustime1 == min(dateplustime1),1 , 0) , by=.(LinkId)]

interval_difference_variableTime <- function(test_DT, inputTimes, windowMonths, impute, hba1c_valueToAdd) {

  
  reportingFrame <- as.data.frame(matrix(nrow = length(inputTimes), ncol = 10))
  colnames(reportingFrame) <- c('interval', 'n', 'n_available','median_pre', 'median_post', 'pval', "pre_25", "post_25", "pre_75", "post_75")
  
  maxHbA1cTimePoint <- max(test_DT$dateplustime1) # - ((windowMonths * (60*60*24*(365.25/12))) /2)
  
  for (j in seq(1, length(inputTimes), 1)) {
    
    reportingFrame$interval[j] <- inputTimes[j]
  
  print(paste('month_', inputTimes[j], sep=''))
  
  ## calculate the max n available for each time point
      # timeIntervalSeconds <- inputTimes[j] * (60*60*24*(365.25/12))
      # courseTimePoint <- maxHbA1cTimePoint - timeIntervalSeconds
      # half_windowMonthsInSeconds <- ((windowMonths * (60*60*24*(365.25/12))) /2)
      # # 
      # setOfAllCandidatesWithCourseInTimeRange <- test_DT
      # setOfAllCandidatesWithCourseInTimeRange <- setOfAllCandidatesWithCourseInTimeRange[DICE_unix < (courseTimePoint + half_windowMonthsInSeconds) & (courseToDelivery == 0 | courseToDelivery > (inputTimes[j]/12)) & (courseToPump == 0 | courseToPump > (inputTimes[j]/12))]
      # 
      # n_availableForFollowUp <- uniqueN(setOfAllCandidatesWithCourseInTimeRange$LinkId)
      # 
      # reportingFrame$n_available[j] <- n_availableForFollowUp
      # 
      # id_frame_forMerge <- data.frame(unique(setOfAllCandidatesWithCourseInTimeRange$LinkId)); colnames(id_frame_forMerge) <- c('LinkId')
      # id_frame_forMerge$mergeFlag = 1
  
    
  test_DT[, c('testCol') := average_hba1c_atTimePoint(timeRelativeToDICE_years, hba1cNumeric, inputTimes[j], windowMonths) , by=.(LinkId)]
  
  # if NA is returned for testCol then no value available.
  # first count the number of IDs who went to course but have no return value for this time point
  test_DT$testCol[is.na(test_DT$testCol)] <- 0
  noReturnedHbA1cValue_set <- test_DT[test_DT$testCol == 0]
  noReturnedHbA1cValue_set_meetingAnalysisParameter <- noReturnedHbA1cValue_set[singleRowFlag == 1 &
                                                                                  (courseToDelivery == 0 | courseToDelivery > (inputTimes[j]/12)) &
                                                                                  (courseToPump == 0 | courseToPump > (inputTimes[j]/12)) &
                                                                                  (max(test_DT$dateplustime1) - DICE_unix) < (inputTimes[j] * (60*60*24*(365.25/12)))]
  
  numberOfUnreturnedTests <- nrow(noReturnedHbA1cValue_set_meetingAnalysisParameter)
  
  # now make the returned value equal to the pre course value, if the impute switch == 1
  if(impute == 1) {
    test_DT$testCol <- ifelse(test_DT$testCol == 0, test_DT$av_hba1c_priorWindow + hba1c_valueToAdd, test_DT$testCol)
  }

  comparisonSet <-  test_DT[singleRowFlag == 1 &
                              testCol > 0 &
                              (courseToDelivery == 0 | courseToDelivery > (inputTimes[j]/12)) &
                              (courseToPump == 0 | courseToPump > (inputTimes[j]/12))]
  
  comparisonSet_forMerge <- data.frame(unique(comparisonSet$LinkId)); colnames(comparisonSet_forMerge) <- c('LinkId')
  comparisonSet_forMerge$comparison_mergeFlag = 1
  
  # identify those in comparison set that aren't in the available set
  setInBoth <- merge(comparisonSet, id_frame_forMerge, by.x = 'LinkId', by.y = 'LinkId', all.x = T)
  inAvailableButNotFollowedUp <- merge(id_frame_forMerge, comparisonSet_forMerge, by.x = 'LinkId', by.y = 'LinkId', all.x = T)
    inAvailableButNotFollowedUp$comparison_mergeFlag[is.na(inAvailableButNotFollowedUp$comparison_mergeFlag)] <- 0
  
    print(nrow(comparisonSet))
      reportingFrame$n[j] <- uniqueN(comparisonSet$LinkId)
      
      reportingFrame$n_available[j] <- reportingFrame$n[j] + numberOfUnreturnedTests
      
    print(quantile(comparisonSet$av_hba1c_priorWindow))
      reportingFrame$median_pre[j] <- quantile(comparisonSet$av_hba1c_priorWindow)[3]
      reportingFrame$pre_25[j] <- quantile(comparisonSet$av_hba1c_priorWindow)[2]
      reportingFrame$pre_75[j] <- quantile(comparisonSet$av_hba1c_priorWindow)[4]
    
    print(quantile(comparisonSet$testCol))
      reportingFrame$median_post[j] <- quantile(comparisonSet$testCol)[3]
      reportingFrame$post_25[j] <- quantile(comparisonSet$testCol)[2]
      reportingFrame$post_75[j] <- quantile(comparisonSet$testCol)[4]
      
      
    print(wilcox.test(comparisonSet$av_hba1c_priorWindow, comparisonSet$testCol, paired = T))
    
    test <- wilcox.test(comparisonSet$av_hba1c_priorWindow, comparisonSet$testCol, paired = T)
    
      reportingFrame$pval[j] <- test$p.val
  
  }
  
  reportingFrame$median_diff <- reportingFrame$median_post - reportingFrame$median_pre
  # reportingFrame$diff_25 <- reportingFrame$post_25 - reportingFrame$pre_25
  # reportingFrame$diff_75 <- reportingFrame$post_75 - reportingFrame$pre_75
  
  plot(reportingFrame$interval, reportingFrame$median_diff, xlab = 'months', ylab = 'difference median hba1c', cex = 2, pch = 16, col = ifelse(reportingFrame$pval < 0.05, 'red', 'black')); lines(reportingFrame$interval, reportingFrame$median_diff)
  abline(0, 1)
  
  reportingFrame$n_prop <- reportingFrame$n / reportingFrame$n_available

  print(reportingFrame)
  
  return(reportingFrame)
  
  
}

printValuesForTable <- function(inputDT) {
  
  inputDT$age_at_course <- (inputDT$DICE_unix - inputDT$dob_unix) / (60*60*24*365.25)
  inputDT[, c('single_id') := (ifelse(dateplustime1 == min(dateplustime1), 1, 0)), by = .(LinkId)]
  
  print('n hba1c tests')
  print(nrow(inputDT))
  
  print('n IDs')
  print(uniqueN(inputDT$LinkId))
  
  print('age quantile')
  print(quantile(inputDT[single_id == 1]$age_at_course))
  
  print('time to dice from diagnosis')
  print(quantile(inputDT[single_id == 1]$timeToDICEfromDIagnosis_years))
  
  print('diabetes type')
  print(table(inputDT[single_id == 1]$diabetes_type))
  
}

compareDiceDafneCharacteristics <- function(inputDT) {
  
  inputDT$age_at_course <- (inputDT$DICE_unix - inputDT$dob_unix) / (60*60*24*365.25)
  inputDT[, c('single_id') := (ifelse(dateplustime1 == min(dateplustime1), 1, 0)), by = .(LinkId)]
  
  print('compare age at course')
  diceAge <- inputDT[single_id == 1 & Course == 'dice']$age_at_course
  dafneAge <- inputDT[single_id == 1 & Course == 'dafne']$age_at_course
  
  print('dice age')
  print(quantile(diceAge))
  print('dafne age')
  print(quantile(dafneAge))
  testAge <- wilcox.test(diceAge, dafneAge)
  print(testAge)
  
  print('time to dice from diagnosis')
  diceTimeToDice <- inputDT[single_id == 1 & Course == 'dice']$timeToDICEfromDIagnosis_years
  dafneTimeToDafne <- inputDT[single_id == 1 & Course == 'dafne']$timeToDICEfromDIagnosis_years
  
  print('dice time to course')
  print(quantile(diceTimeToDice))
  print('dafne time to course')
  print(quantile(dafneTimeToDafne))
  
  testTimeToCourse <- wilcox.test(diceTimeToDice, dafneTimeToDafne)
  print(testTimeToCourse)
  
  print('diabetes type test')
    diceT2 <- nrow(inputDT[single_id == 1 & Course == 'dice' & diabetes_type == 'Type 2 Diabetes Mellitus'])
    dafneT2 <- nrow(inputDT[single_id == 1 & Course == 'dafne' & diabetes_type == 'Type 2 Diabetes Mellitus'])
    dice_all <- nrow(inputDT[single_id == 1 & Course == 'dice'])
    dafne_all <- nrow(inputDT[single_id == 1 & Course == 'dafne'])
  
  print('diceT2'); print(diceT2)
  print('dafneT2'); print(dafneT2)
  print('dice_all'); print(dice_all)
  print('dafne_all'); print(dafne_all)
  
  print(prop.test(c(diceT2, dafneT2), c(dice_all, dafne_all)))
  
  print(table(inputDT[single_id == 1]$diabetes_type))
  
}

diceDT <- diceHbA1cDT[Course == 'dice']
dafneDT <- diceHbA1cDT[Course == 'dafne']

# interval_difference(diceHbA1cDT)

# interval_difference(diceDT)
# interval_difference(dafneDT)

compareDiceDafneCharacteristics(diceHbA1cDT)
# 
# allFrame <- interval_difference_variableTime(diceHbA1cDT, seq(6, 48, 6), 6)
# diceFrame <- interval_difference_variableTime(diceDT, seq(15, 60, 15), 15, 1, 0)

## main plot here
dafneFrame <- interval_difference_variableTime(dafneDT, seq(6, 60, 6), 6, 1, 0)


# 
#   plot(dafneFrame$interval, dafneFrame$median_post, pch = 16, cex = 2, ylim = c(60, 85)); lines(dafneFrame$interval, dafneFrame$median_post)
#   points(dafneFrame$interval, dafneFrame$post_25, pch = 16, cex = 1, col = 'red'); lines(dafneFrame$interval, dafneFrame$post_25, lty = 3, col = 'red')
#   points(dafneFrame$interval, dafneFrame$post_75, pch = 16, cex = 1, col = 'red'); lines(dafneFrame$interval, dafneFrame$post_75, lty = 3, col = 'red')
#   abline(quantile(dafneFrame$median_pre)[3], 0, col = 'blue')
#   abline(quantile(dafneFrame$pre_25)[3], 0, lty = 4, col = 'blue')
#   abline(quantile(dafneFrame$pre_75)[3], 0, lty = 4, col = 'blue')
# dafneFrame <- interval_difference_variableTime(dafneDT, c(6, 12, 36), 6)


printValuesForTable(diceHbA1cDT)

####################

# simple plots

x <- boxplot(diceHbA1cDT$hba1cNumeric ~ cut(diceHbA1cDT$timeRelativeToDICE_years, breaks = seq(-3,3,0.2)), varwidth = T, ylim = c(55,85))

#dice vs dafne
dice <- boxplot(diceHbA1cDT_courseComp[Course == 'dice']$hba1cNumeric ~ cut(diceHbA1cDT_courseComp[Course == 'dice']$timeRelativeToDICE_years, breaks = seq(-3,3,0.5)), varwidth = T, ylim = c(55,85))
dafne <- boxplot(diceHbA1cDT_courseComp[Course == 'dafne']$hba1cNumeric ~ cut(diceHbA1cDT_courseComp[Course == 'dafne']$timeRelativeToDICE_years, breaks = seq(-3,3,0.5)), varwidth = T, ylim = c(55,85))

plot(dice$stats[3, ], col = "red", ylim = c(50, 90)); lines(dice$stats[3, ], col = "red")
  points(dice$stats[2, ], col = "red", cex = 0); lines(dice$stats[2, ], col = "red", lty = 3)
  points(dice$stats[4, ], col = "red", cex = 0); lines(dice$stats[4, ], col = "red", lty = 3)
  
points(dafne$stats[3, ], col = "blue"); lines(dafne$stats[3, ], col = "blue")
  points(dafne$stats[2, ], col = "blue", cex = 0); lines(dafne$stats[2, ], col = "blue", lty = 3)
  points(dafne$stats[4, ], col = "blue", cex = 0); lines(dafne$stats[4, ], col = "blue", lty = 3)


idList <- unique(diceHbA1cDT$LinkId)

for (j in seq(1, length(idList), 1)) {
  plotSet <- diceHbA1cDT[LinkId == idList[j]]
  plotSet <- plotSet[order(plotSet$timeRelativeToDICE_years), ]
  
  if (j == 1) {
    plot(plotSet$timeRelativeToDICE_years, plotSet$timeSeriesDataPoint, cex = 0, xlim = c(-5, 5), ylim = c(40, 120))
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

variabilityDT <- diceHbA1cDT
variabilityDT <- diceHbA1cDT[Course == 'dice']
variabilityDT <- diceHbA1cDT[Course == 'dafne']

variabilityWindowYears <- 4

variabilityDT <- variabilityDT[(courseToPump == 0 | courseToPump > variabilityWindowYears) & ( courseToDelivery == 0 |  courseToDelivery > variabilityWindowYears)]

idList <- unique(variabilityDT$LinkId)

report_IQR_Frame <- as.data.frame(matrix(nrow = length(idList), ncol = 13))
colnames(report_IQR_Frame) <- c("id", "IQR_pre", "IQR_post", "n_pre", "n_post", "median_pre", "median_post", "CV_pre", "CV_post", "mean_pre", "mean_post", "sd_pre", "sd_post")
report_IQR_Frame$id <- idList

print(length(idList))

set.seed(1234)
for (j in seq(1, length(idList), 1)) {
  
  #j = 76
  
  if (j %% 100 == 0) {print(j)}
  
  plotSet <- variabilityDT[LinkId == idList[j]]
  preSet <- plotSet[timeRelativeToDICE_years > (-variabilityWindowYears) & timeRelativeToDICE_years < 0]
  postSet <- plotSet[timeRelativeToDICE_years >= 0 & timeRelativeToDICE_years < variabilityWindowYears]
  
    # randomly equalise post and pre N hba1c values
    if (nrow(postSet) > nrow(preSet)) {
      postSet$randomCol <- runif(nrow(postSet), 0, 1)
      postSet <- postSet[order(postSet$randomCol), ]
      postSet <- postSet[1 : nrow(preSet), ]
    }
  
  # randomly equalise post and pre N hba1c values
  if (nrow(preSet) > nrow(postSet)) {
    preSet$randomCol <- runif(nrow(preSet), 0, 1)
    preSet <- preSet[order(preSet$randomCol), ]
    preSet <- preSet[1 : nrow(postSet), ]
  }
  
  IQR_pre <- quantile(preSet$hba1cNumeric)[4] - quantile(preSet$hba1cNumeric)[2]
  IQR_post <- quantile(postSet$hba1cNumeric)[4] - quantile(postSet$hba1cNumeric)[2]
  
  median_pre <- quantile(preSet$hba1cNumeric)[3]
  median_post <- quantile(postSet$hba1cNumeric)[3]
  
  mean_pre <- mean(preSet$hba1cNumeric)
  mean_post <- mean(postSet$hba1cNumeric)
  
  sd_pre <- sd(preSet$hba1cNumeric)
  sd_post <- sd(postSet$hba1cNumeric)
  
  cv_pre <- sd_pre / mean_pre
  cv_post <- sd_post / mean_post
  
  plot_x <- c(0, 1)
  plot_y <- c(IQR_pre, IQR_post)

  if (j == 1) {
    # plot(plot_x, plot_y, xlim = c(-0.5, 1.5), ylim = c(0,10), cex = sqrt(nrow(preSet)))
    # lines(plot_x, plot_y, col = rgb(0, 0, 0, 0.2, maxColorValue = 1))
  }
  if (j > 1) {
    # points(plot_x, plot_y, cex = sqrt(nrow(postSet)))
    # lines(plot_x, plot_y, col = rgb(0, 0, 0, 0.2, maxColorValue = 1))
  }
  
  report_IQR_Frame$IQR_pre[j] <- IQR_pre
  report_IQR_Frame$IQR_post[j] <- IQR_post
  report_IQR_Frame$n_pre[j] <- nrow(preSet)
  report_IQR_Frame$n_post[j] <- nrow(postSet)
  report_IQR_Frame$median_pre[j] <- median_pre
  report_IQR_Frame$median_post[j] <- median_post
  report_IQR_Frame$cv_pre[j] <- cv_pre
  report_IQR_Frame$cv_post[j] <- cv_post
  report_IQR_Frame$mean_pre[j] <- mean_pre
  report_IQR_Frame$mean_post[j] <- mean_post
  report_IQR_Frame$sd_pre[j] <- sd_pre
  report_IQR_Frame$sd_post[j] <- sd_post
  
  
}

report_IQR_Frame <- subset(report_IQR_Frame, n_pre >1 & n_post >1)

print(quantile(report_IQR_Frame$IQR_pre, na.rm = T))
print(quantile(report_IQR_Frame$IQR_post, na.rm = T))
wilcox.test(report_IQR_Frame$IQR_pre, report_IQR_Frame$IQR_post, paired = T)

print(quantile(report_IQR_Frame$n_pre, na.rm = T))
print(quantile(report_IQR_Frame$n_post, na.rm = T))
wilcox.test(report_IQR_Frame$n_pre, report_IQR_Frame$n_post, paired = T)

print(quantile(report_IQR_Frame$median_pre, na.rm = T))
print(quantile(report_IQR_Frame$median_post, na.rm = T))
wilcox.test(report_IQR_Frame$median_pre, report_IQR_Frame$median_post, paired = T)

print(quantile(report_IQR_Frame$cv_pre, na.rm = T))
print(quantile(report_IQR_Frame$cv_post, na.rm = T))
wilcox.test(report_IQR_Frame$cv_pre, report_IQR_Frame$cv_post, paired = T)

print(quantile(report_IQR_Frame$mean_pre, na.rm = T))
print(quantile(report_IQR_Frame$mean_post, na.rm = T))
wilcox.test(report_IQR_Frame$mean_pre, report_IQR_Frame$mean_post, paired = T)



#########################################################################################################
#########################################################################################################
# admissions pre/post

#define what constitutes an admission
admissionsDT_forTesting <- admissionsDT[(nCBGperAdmission>2 & admissionDurationDays >= 0.5)]

# set test set
admissionFrequencyTestSet_DT <- diceHbA1cDT
admissionFrequencyTestSet_DT <- diceHbA1cDT[Course == 'dice']
admissionFrequencyTestSet_DT <- diceHbA1cDT[Course == 'dafne']

windowOfInterestYears <- 2
windowOfInterestSeconds <- windowOfInterestYears * (60*60*24*365.25)
# select group for analysis: time from diagnosis >= windowOfInterestYears and max followup time <windowOfInterestYears
admissionIdList <- unique(admissionFrequencyTestSet_DT[timeToDICEfromDIagnosis_years > windowOfInterestYears & DICE_unix < (lastAdmssionDate - windowOfInterestSeconds)]$PatId)

report_admission_Frame <- as.data.frame(matrix(nrow = length(admissionIdList), ncol = 5))
colnames(report_admission_Frame) <- c("id", "adm_pre", "adm_post", "admIQR_pre", "admIQR_post")
report_admission_Frame$id <- admissionIdList

for (jj in seq(1, length(admissionIdList), 1)) {
  admissionsSet <- admissionsDT_forTesting[ID == admissionIdList[jj]]
  diceDate <- admissionFrequencyTestSet_DT[PatId == admissionIdList[jj]]$DICE_unix[1]
  
  admissionsSet$admission_timeRelativeToDICE <- admissionsSet$dateplustime1 - diceDate
  admissionsSet$admission_timeRelativeToDICE_years <- admissionsSet$admission_timeRelativeToDICE / (60*60*24*365.25)
  
  preSet <- admissionsSet[admission_timeRelativeToDICE_years > -windowOfInterestYears & admission_timeRelativeToDICE_years <= 0]
  postSet <- admissionsSet[admission_timeRelativeToDICE_years > 0 & admission_timeRelativeToDICE_years < windowOfInterestYears]
  
  report_admission_Frame$adm_pre[jj] <- nrow(preSet)
  report_admission_Frame$adm_post[jj] <- nrow(postSet)
  
    IQR_preSet <- preSet[nCBGperAdmission > 1]
    IQR_postSet <- postSet[nCBGperAdmission > 1]
    
}

print(length(admissionIdList))

quantile(report_admission_Frame$adm_pre)
quantile(report_admission_Frame$adm_post)

sum(report_admission_Frame$adm_pre)
sum(report_admission_Frame$adm_post)

atLeastOneAdmissionPre <- ifelse(report_admission_Frame$adm_pre > 0, 1, 0)
atLeastOneAdmissionPost <- ifelse(report_admission_Frame$adm_post > 0, 1, 0)

  sum(atLeastOneAdmissionPre)
  sum(atLeastOneAdmissionPost)

wilcox.test(report_admission_Frame$adm_pre, report_admission_Frame$adm_post, paired = T)
wilcox.test(atLeastOneAdmissionPre, atLeastOneAdmissionPost, paired = T)
prop.test(c(sum(atLeastOneAdmissionPre), sum(atLeastOneAdmissionPost)), c(length(admissionIdList), length(admissionIdList)))

#########################################################################################################
#########################################################################################################
# match DICE to DAFNE. match for initial hba1c and followup duration

dice_IDs <- unique(diceDT$PatId)

for (jj in seq(1, length(dice_IDs), 1)) {
  
  ID_interest <- dice_IDs[jj]
  
  
  
}

  
  
  
  
  
  
  
  
  
  
  

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