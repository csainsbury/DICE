library(data.table)

returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%d/%m/%Y", tz="GMT"))
  return(returnVal)
}

###############################################################################
# dice

# load admissions
dice_pregDF<-read.csv("~/R/GlCoSy/SDsource/dice_pregnancyData.csv")
dice_pregDF$unix_DOdelivery <- returnUnixDateTime(dice_pregDF$date.of.childbirth)
dice_pregDF$unix_DOdelivery[is.na(dice_pregDF$unix_DOdelivery)] <- 0

dice_pregDT <- data.table(dice_pregDF)
dice_pregDT <- dice_pregDT[unix_DOdelivery > 0]

dice_pregDT <- dice_pregDT[date.of.childbirth != "no progression of pregnancy"]

saveOutFrame <- data.frame(dice_pregDT$CHI, dice_pregDT$unix_DOdelivery)
colnames(saveOutFrame) <- c('ID', 'unix_deliveryDate')

saveOutFrame <- unique(saveOutFrame)

dice_saveName <- paste('~/R/GlCoSy/SDsource/dice_pregnancyData_cleaned.csv')

write.table(saveOutFrame, file = dice_saveName, row.names = F, col.names = T)

###############################################################################
# dafne


# load admissions
dafne_pregDF<-read.csv("~/R/GlCoSy/SDsource/dafne_pregnancyData.csv")
dafne_pregDT <- data.table(dafne_pregDF)
dafne_pregDT <- dafne_pregDT[X.pregnancy.and.babies.DOB.if.so != 'm']
dafne_pregDT$unix_deliveryDate <- returnUnixDateTime(dafne_pregDT$X.pregnancy.and.babies.DOB.if.so)

dafne_pregDT$unix_deliveryDate[is.na(dafne_pregDT$unix_deliveryDate)] <- 0
dafne_pregDT <- dafne_pregDT[unix_deliveryDate > 0]

dafne_saveOutFrame <- data.frame(dafne_pregDT$CHI.Number, dafne_pregDT$unix_deliveryDate)
colnames(dafne_saveOutFrame) <- c('ID', 'unix_deliveryDate')

dafne_saveOutFrame <- unique(dafne_saveOutFrame)

dafne_saveName <- paste('~/R/GlCoSy/SDsource/dafne_pregnancyData_cleaned.csv')
