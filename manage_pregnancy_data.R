library(data.table)

returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%d/%m/%Y", tz="GMT"))
  return(returnVal)
}

# load admissions
dice_pregDF<-read.csv("~/R/GlCoSy/SDsource/dice_pregnancyData.csv")
dice_pregDF$unix_DOdelivery <- returnUnixDateTime(dice_pregDF$date.of.childbirth)
dice_pregDF$unix_DOdelivery[is.na(dice_pregDF$unix_DOdelivery)] <- 0

dice_pregDT <- data.table(dice_pregDF)
dice_predDT <- dice_pregDT[unix_DOdelivery > 0]

dice_pregDT <- dice_predDT[date.of.childbirth != "no progression of pregnancy"]

saveOutFrame <- data.frame(dice_predDT$CHI, dice_predDT$unix_DOdelivery)
colnames(saveOutFrame) <- c('ID', 'unix_deliveryDate')

dice_saveName <- paste('~/R/GlCoSy/SDsource/dice_pregnancyData_cleaned.csv')