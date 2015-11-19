library(dplyr)
library(lubridate)
library(data.table)
library(caret)

allDisconnected <- fread("Discon1.csv")
colnames(allDisconnected) <- toupper(colnames(allDisconnected))

# TODO : Pick records earlier than present month.
req1 <- fread("Req_1.csv")
req1 <- mutate(req1, date2 = dmy(req1$date2))

lastMonth <- "30-04-2015"

# TODO : Pick records earlier than present month.
req2 <- fread("Req_2.csv")
req2 <- mutate(req2, date2 = dmy(req2$date2))


monthAna <- function(thisMonth, fileName) {
  #thisMonth <- "01-08-2014"
  payUsageData <- fread(fileName)

  req1 <- filter(req1, req1$date2 - dmy(thisMonth) < 0)
  req2 <- filter(req2, req2$date2 - dmy(thisMonth) < 0)
  
  payUsageData <- mutate(payUsageData, req1 = ID %in% req1$id, req2 = ID %in% req2$id)
  #monthlyDisconnected <- select(monthlyDisconnected, -ID, -Requesttype, -RequestDate, -Disconnection_date, -req_cnt)
  
  # Disconnection date
  payUsageData <- left_join(payUsageData,allDisconnected, by="ID" )
  payUsageData[is.na(payUsageData$DATE2)]$DATE2 <- lastMonth
  
  payUsageData <- mutate(payUsageData, DisconDate = dmy(DATE2), Acct_actv_date = dmy(Acct_actv_date))
  payUsageData <- mutate(payUsageData, AgeOnNet = as.numeric(difftime(payUsageData$DisconDate, payUsageData$Acct_actv_date, units = c("days"))))
  payUsageData <- select(payUsageData, -id_pk, -Disconnection_date, -Requesttype, -RequestDate, -month, -req_cnt, -DATE2)
  
  #Change Data Types 
  #Assign meaningful values to NA
  
  payUsageData$VOL_2G <- as.numeric(payUsageData$VOL_2G)
  payUsageData$VOL_3G <- as.numeric(payUsageData$VOL_3G)
  payUsageData$NonRentalCharge <- as.numeric(payUsageData$NonRentalCharge)
  
  payUsageData$Total_Bill <- as.numeric(payUsageData$Total_Bill)
  payUsageData[is.na(payUsageData$Total_Bill)]$Total_Bill <- 0
  
  payUsageData$Adj <- as.numeric(payUsageData$Adj)
  payUsageData$Usag <- as.numeric(payUsageData$Usag)
  
  payUsageData$LOC_OG_XYZ2XYZ_MOU <- as.numeric(payUsageData$LOC_OG_XYZ2XYZ_MOU)
  payUsageData[is.na(payUsageData$LOC_OG_XYZ2XYZ_MOU)]$LOC_OG_XYZ2XYZ_MOU <- 0
  
  payUsageData$STD_OG_XYZ2XYZ_MOU <- as.numeric(payUsageData$STD_OG_XYZ2XYZ_MOU)
  payUsageData[is.na(payUsageData$STD_OG_XYZ2XYZ_MOU)]$STD_OG_XYZ2XYZ_MOU <- 0
  
  payUsageData$LOC_OG_XYZ2M_MOU <- as.numeric(payUsageData$LOC_OG_XYZ2M_MOU)
  payUsageData[is.na(payUsageData$LOC_OG_XYZ2M_MOU)]$LOC_OG_XYZ2M_MOU <- 0
  
  payUsageData$STD_OG_XYZ2M_MOU <- as.numeric(payUsageData$STD_OG_XYZ2M_MOU)
  payUsageData[is.na(payUsageData$STD_OG_XYZ2M_MOU)]$STD_OG_XYZ2M_MOU <- 0
  
  payUsageData$ISD_OG_MOU <- as.numeric(payUsageData$ISD_OG_MOU)
  payUsageData[is.na(payUsageData$ISD_OG_MOU)]$ISD_OG_MOU <- 0
  
  payUsageData$TOTAL_OG_MOU <- as.numeric(payUsageData$TOTAL_OG_MOU)
  payUsageData[is.na(payUsageData$TOTAL_OG_MOU)]$TOTAL_OG_MOU <- 0
  
  payUsageData$LOC_IC_XYZ2XYZ_MOU <- as.numeric(payUsageData$LOC_IC_XYZ2XYZ_MOU)
  payUsageData[is.na(payUsageData$LOC_IC_XYZ2XYZ_MOU)]$LOC_IC_XYZ2XYZ_MOU <- 0
  
  payUsageData$STD_IC_XYZ2XYZ_MOU <- as.numeric(payUsageData$STD_IC_XYZ2XYZ_MOU)
  payUsageData[is.na(payUsageData$STD_IC_XYZ2XYZ_MOU)]$STD_IC_XYZ2XYZ_MOU <- 0
  
  payUsageData$LOC_IC_XYZ2M_MOU <- as.numeric(payUsageData$LOC_IC_XYZ2M_MOU)
  payUsageData[is.na(payUsageData$LOC_IC_XYZ2M_MOU)]$LOC_IC_XYZ2M_MOU <- 0
  
  payUsageData$STD_IC_XYZ2M_MOU <- as.numeric(payUsageData$STD_IC_XYZ2M_MOU)
  payUsageData[is.na(payUsageData$STD_IC_XYZ2M_MOU)]$STD_IC_XYZ2M_MOU <- 0
  
  payUsageData$STD_IC_MOU <- as.numeric(payUsageData$STD_IC_MOU)
  payUsageData[is.na(payUsageData$STD_IC_MOU)]$STD_IC_MOU <- 0
  
  payUsageData$ISD_IC_MOU <- as.numeric(payUsageData$ISD_IC_MOU)
  payUsageData[is.na(payUsageData$ISD_IC_MOU)]$ISD_IC_MOU <- 0
  
  payUsageData$ROAM_OG_MOU <- as.numeric(payUsageData$ROAM_OG_MOU)
  payUsageData[is.na(payUsageData$ROAM_OG_MOU)]$ROAM_OG_MOU <- 0
  
  payUsageData$ROAM_IC_MOU <- as.numeric(payUsageData$ROAM_IC_MOU)
  payUsageData[is.na(payUsageData$ROAM_IC_MOU)]$ROAM_IC_MOU <- 0
  
  payUsageData$TOTAL_IC_MOU <- as.numeric(payUsageData$TOTAL_IC_MOU)
  payUsageData[is.na(payUsageData$TOTAL_IC_MOU)]$TOTAL_IC_MOU <- 0
  
  #Remove outliers
  payUsageData <- payUsageData[payUsageData$Total_Bill != 999999999]
  
  
  #Remove rows where disconnection date is earlier than this month.
  fir<-dmy(thisMonth) - days(29)
  payUsageData <- payUsageData[(as.numeric(difftime(payUsageData$DisconDate, fir, units = c("days"))) >0) | is.na(payUsageData$DisconDate)]
  
  #Calculate variables
  payUsageData <- mutate(payUsageData, networkRatio = VOL_2G / (VOL_2G + VOL_3G))
  payUsageData <- mutate(payUsageData, nonRentRatio = (NonRentalCharge / (NonRentalCharge + Total_Bill)))
  payUsageData <- mutate(payUsageData, totalSTD = STD_IC_MOU + STD_OG_XYZ2M_MOU + STD_OG_XYZ2XYZ_MOU)
  payUsageData <- mutate(payUsageData, totalLocal = LOC_IC_XYZ2M_MOU + LOC_IC_XYZ2XYZ_MOU + LOC_OG_XYZ2M_MOU + LOC_OG_XYZ2XYZ_MOU)
  payUsageData <- mutate(payUsageData, totalHost = LOC_IC_XYZ2XYZ_MOU + LOC_OG_XYZ2XYZ_MOU + 
                           STD_IC_XYZ2XYZ_MOU + STD_OG_XYZ2XYZ_MOU)
  payUsageData <- mutate(payUsageData, totalOther = LOC_IC_XYZ2M_MOU + LOC_OG_XYZ2M_MOU + 
                           STD_IC_XYZ2M_MOU + STD_OG_XYZ2M_MOU)
  
  payUsageData <- filter(payUsageData, TOTAL_OG_MOU > 0 | TOTAL_IC_MOU >0 | VOL_3G > 0 | VOL_2G > 0)
  payUsageData <- mutate(payUsageData, total_IC_XYZ2XYZ_MOU = LOC_IC_XYZ2XYZ_MOU + STD_IC_XYZ2XYZ_MOU)
  payUsageData <- mutate(payUsageData, total_OG_XYZ2XYZ_MOU = LOC_OG_XYZ2XYZ_MOU + STD_OG_XYZ2XYZ_MOU)
  payUsageData <- mutate(payUsageData, total_IC_XYZ2M_MOU = LOC_IC_XYZ2M_MOU + STD_IC_XYZ2M_MOU)
  payUsageData <- mutate(payUsageData, total_OG_XYZ2M_MOU = LOC_OG_XYZ2M_MOU + STD_OG_XYZ2M_MOU)
  payUsageData <- mutate(payUsageData, total_XYZ2XYZ_MOU = LOC_IC_XYZ2XYZ_MOU + STD_IC_XYZ2XYZ_MOU + LOC_OG_XYZ2XYZ_MOU + STD_OG_XYZ2XYZ_MOU)
  payUsageData <- mutate(payUsageData, total_XYZ2M_MOU = LOC_IC_XYZ2M_MOU + STD_IC_XYZ2M_MOU + LOC_OG_XYZ2M_MOU + STD_OG_XYZ2M_MOU)
  payUsageData <- mutate(payUsageData, XYZ2XYZ_XYZ2M_RATIO = (total_XYZ2XYZ_MOU / (total_XYZ2XYZ_MOU + total_XYZ2M_MOU)))

  #dataOnly <- payUsageData[payUsageData$TOTAL_OG_MOU == 0 & payUsageData$TOTAL_IC_MOU == 0]
  #voiceOnly <- payUsageData[payUsageData$VOL_2G == 0 & payUsageData$VOL_3G == 0]
  #dataAndVoice <- filter(payUsageData, (TOTAL_OG_MOU > 0 | TOTAL_IC_MOU >0) & (VOL_3G > 0 | VOL_2G > 0))
  
  return (payUsageData)
}

#thisMonth <- "01-08-2014"
#payUsageData <- fread("churn_info_aug.csv")

 julFiltered <- monthAna("31-07-2014" , "churn_info_jul.csv")
 augFiltered <- monthAna("31-08-2014" , "churn_info_aug.csv")
 sepFiltered <- monthAna("30-09-2014" , "churn_info_sep.csv")
 octFiltered <- monthAna("31-10-2014" , "churn_info_oct.csv")
 novFiltered <- monthAna("30-11-2014" , "churn_info_nov.csv")
 janFiltered <- monthAna("31-01-2015" , "churn_info_jan.csv")
 marFiltered <- monthAna("31-03-2015" , "churn_info_mar.csv")
 aprValdata <- monthAna("30-04-2015" , "churn_info_apr.csv")

 payUsageData <- bind_rows(list(julFiltered,augFiltered,sepFiltered,octFiltered,novFiltered,
                                marFiltered,janFiltered))
 payUsageData <- mutate(payUsageData, churnStatus = ID %in% allDisconnected$ID)
 aprValdata <- mutate(aprValdata, churnStatus = ID %in% allDisconnected$ID)
 rm(req1,req2,allDisconnected,julFiltered,augFiltered,sepFiltered,octFiltered,novFiltered,marFiltered,janFiltered)
 
 aprValdata[length(aprValdata)==0]<-0
 aprValdata[is.na(aprValdata)]<-0
 
 payUsageData[length(payUsageData)==0]<-0
 payUsageData[is.na(payUsageData)]<-0
 
 payUsageData %>% group_by(ID) %>% 
   mutate(mean2G3G = mean(networkRatio)) %>% mutate(var2G3G = var(networkRatio)) %>%
   mutate(meanVOL_2G = mean(VOL_2G)) %>% mutate(varVOL_2G = var(VOL_2G)) %>%
   mutate(meanVOL_3G = mean(VOL_3G)) %>% mutate(varVOL_3G = var(VOL_3G)) %>%
   mutate(meanTotal_Bill = mean(Total_Bill)) %>% mutate(varTotal_Bill = var(Total_Bill)) %>%
   mutate(meanRentalCharge = mean(RentalCharge)) %>% mutate(varRentalCharge = var(RentalCharge)) %>%
   mutate(meanNonRentalCharge = mean(NonRentalCharge)) %>% mutate(varNonRentalCharge = var(NonRentalCharge)) %>%
   mutate(meanUsag = mean(Usag)) %>% mutate(varUsag = var(Usag)) %>%
   mutate(meanLOC_OG_XYZ2XYZ_MOU = mean(LOC_OG_XYZ2XYZ_MOU)) %>% mutate(varLOC_OG_XYZ2XYZ_MOU = var(LOC_OG_XYZ2XYZ_MOU)) %>%
   mutate(meanSTD_OG_XYZ2XYZ_MOU = mean(STD_OG_XYZ2XYZ_MOU)) %>% mutate(varSTD_OG_XYZ2XYZ_MOU = var(STD_OG_XYZ2XYZ_MOU)) %>%
   mutate(meanLOC_OG_XYZ2M_MOU = mean(LOC_OG_XYZ2M_MOU)) %>% mutate(varLOC_OG_XYZ2M_MOU = var(LOC_OG_XYZ2M_MOU)) %>%
   mutate(meanSTD_OG_XYZ2M_MOU = mean(STD_OG_XYZ2M_MOU)) %>% mutate(varSTD_OG_XYZ2M_MOU = var(STD_OG_XYZ2M_MOU)) %>%
   mutate(meanSTD_OG_MOU = mean(STD_OG_MOU)) %>% mutate(varSTD_OG_MOU = var(STD_OG_MOU)) %>%
   mutate(meanISD_OG_MOU = mean(ISD_OG_MOU)) %>% mutate(varISD_OG_MOU = var(ISD_OG_MOU)) %>%
   mutate(meanTOTAL_OG_MOU = mean(TOTAL_OG_MOU)) %>% mutate(varTOTAL_OG_MOU = var(TOTAL_OG_MOU)) %>%
   mutate(meanLOC_IC_XYZ2XYZ_MOU = mean(LOC_IC_XYZ2XYZ_MOU)) %>% mutate(varLOC_IC_XYZ2XYZ_MOU= var(LOC_IC_XYZ2XYZ_MOU)) %>%
   mutate(meanSTD_IC_XYZ2XYZ_MOU = mean(STD_IC_XYZ2XYZ_MOU)) %>% mutate(varSTD_IC_XYZ2XYZ_MOU = var(STD_IC_XYZ2XYZ_MOU)) %>%
   mutate(meanLOC_IC_XYZ2M_MOU = mean(LOC_IC_XYZ2M_MOU)) %>% mutate(varLOC_IC_XYZ2M_MOU = var(LOC_IC_XYZ2M_MOU)) %>%
   mutate(meanSTD_IC_XYZ2M_MOU = mean(STD_IC_XYZ2M_MOU)) %>% mutate(varSTD_IC_XYZ2M_MOU = var(STD_IC_XYZ2M_MOU)) %>%
   mutate(meanSTD_IC_MOU = mean(STD_IC_MOU)) %>% mutate(varSTD_IC_MOU = var(STD_IC_MOU)) %>%
   mutate(meanISD_IC_MOU = mean(ISD_IC_MOU)) %>% mutate(varISD_IC_MOU = var(ISD_IC_MOU)) %>%
   mutate(meanROAM_OG_MOU = mean(ROAM_OG_MOU)) %>% mutate(varROAM_OG_MOU = var(ROAM_OG_MOU)) %>%
   mutate(meanROAM_IC_MOU = mean(ROAM_IC_MOU)) %>% mutate(varROAM_IC_MOU = var(ROAM_IC_MOU)) %>%
   mutate(meanTOTAL_IC_MOU = mean(TOTAL_IC_MOU)) %>% mutate(varTOTAL_IC_MOU = var(TOTAL_IC_MOU)) %>%
   mutate(req1 = max(as.logical(req1))) %>% 
   mutate(req2 = max(as.logical(req2))) %>% 
   mutate(AgeOnNet = min(AgeOnNet)) %>% 
   mutate(meannonRentRatio = mean(nonRentRatio)) %>% mutate(varnonRentRatio = var(nonRentRatio)) %>%
   mutate(meantotalSTD = mean(totalSTD)) %>% mutate(vartotalSTD = var(totalSTD)) %>%
   mutate(meantotalLocal = mean(totalLocal)) %>% mutate(vartotalLocal = var(totalLocal)) %>%
   mutate(meantotalHost = mean(totalHost)) %>% mutate(vartotalHost = var(totalHost)) %>%
   mutate(meantotalOther = mean(totalOther)) %>% mutate(vartotalOther = var(totalOther)) %>%
   mutate(meantotal_IC_XYZ2XYZ_MOU= mean(total_IC_XYZ2XYZ_MOU)) %>% mutate(vartotal_IC_XYZ2XYZ_MOU = var(total_IC_XYZ2XYZ_MOU)) %>%
   mutate(meantotal_OG_XYZ2XYZ_MOU = mean(total_OG_XYZ2XYZ_MOU)) %>% mutate(vartotal_OG_XYZ2XYZ_MOU = var(total_OG_XYZ2XYZ_MOU)) %>%
   mutate(meantotal_IC_XYZ2M_MOU = mean(total_IC_XYZ2M_MOU)) %>% mutate(vartotal_IC_XYZ2M_MOU = var(total_IC_XYZ2M_MOU)) %>%
   mutate(meantotal_OG_XYZ2M_MOU = mean(total_OG_XYZ2M_MOU)) %>% mutate(vartotal_OG_XYZ2M_MOU = var(total_OG_XYZ2M_MOU)) %>%
   mutate(meantotal_XYZ2XYZ_MOU = mean(total_XYZ2XYZ_MOU)) %>% mutate(vartotal_XYZ2XYZ_MOU = var(total_XYZ2XYZ_MOU)) %>%
   mutate(meantotal_XYZ2M_MOU = mean(total_XYZ2M_MOU)) %>% mutate(vartotal_XYZ2M_MOU = var(total_XYZ2M_MOU)) %>%
   mutate(meanXYZ2XYZ_XYZ2M_RATIO = mean(XYZ2XYZ_XYZ2M_RATIO)) %>% mutate(varXYZ2XYZ_XYZ2M_RATIO = var(XYZ2XYZ_XYZ2M_RATIO)) -> x
 
 
 
 y <- x[,c(1,35,49:119)]
 data <- distinct(y)
 data[is.na(data)] = 0
 write.csv(data,file="churn_data.csv",sep=",",eol="\n",row.names=F,col.names=T,quote=F)
 
 aprValdata %>% group_by(ID) %>% 
   mutate(mean2G3G = mean(networkRatio)) %>% mutate(var2G3G = var(networkRatio)) %>%
   mutate(meanVOL_2G = mean(VOL_2G)) %>% mutate(varVOL_2G = var(VOL_2G)) %>%
   mutate(meanVOL_3G = mean(VOL_3G)) %>% mutate(varVOL_3G = var(VOL_3G)) %>%
   mutate(meanTotal_Bill = mean(Total_Bill)) %>% mutate(varTotal_Bill = var(Total_Bill)) %>%
   mutate(meanRentalCharge = mean(RentalCharge)) %>% mutate(varRentalCharge = var(RentalCharge)) %>%
   mutate(meanNonRentalCharge = mean(NonRentalCharge)) %>% mutate(varNonRentalCharge = var(NonRentalCharge)) %>%
   mutate(meanUsag = mean(Usag)) %>% mutate(varUsag = var(Usag)) %>%
   mutate(meanLOC_OG_XYZ2XYZ_MOU = mean(LOC_OG_XYZ2XYZ_MOU)) %>% mutate(varLOC_OG_XYZ2XYZ_MOU = var(LOC_OG_XYZ2XYZ_MOU)) %>%
   mutate(meanSTD_OG_XYZ2XYZ_MOU = mean(STD_OG_XYZ2XYZ_MOU)) %>% mutate(varSTD_OG_XYZ2XYZ_MOU = var(STD_OG_XYZ2XYZ_MOU)) %>%
   mutate(meanLOC_OG_XYZ2M_MOU = mean(LOC_OG_XYZ2M_MOU)) %>% mutate(varLOC_OG_XYZ2M_MOU = var(LOC_OG_XYZ2M_MOU)) %>%
   mutate(meanSTD_OG_XYZ2M_MOU = mean(STD_OG_XYZ2M_MOU)) %>% mutate(varSTD_OG_XYZ2M_MOU = var(STD_OG_XYZ2M_MOU)) %>%
   mutate(meanSTD_OG_MOU = mean(STD_OG_MOU)) %>% mutate(varSTD_OG_MOU = var(STD_OG_MOU)) %>%
   mutate(meanISD_OG_MOU = mean(ISD_OG_MOU)) %>% mutate(varISD_OG_MOU = var(ISD_OG_MOU)) %>%
   mutate(meanTOTAL_OG_MOU = mean(TOTAL_OG_MOU)) %>% mutate(varTOTAL_OG_MOU = var(TOTAL_OG_MOU)) %>%
   mutate(meanLOC_IC_XYZ2XYZ_MOU = mean(LOC_IC_XYZ2XYZ_MOU)) %>% mutate(varLOC_IC_XYZ2XYZ_MOU= var(LOC_IC_XYZ2XYZ_MOU)) %>%
   mutate(meanSTD_IC_XYZ2XYZ_MOU = mean(STD_IC_XYZ2XYZ_MOU)) %>% mutate(varSTD_IC_XYZ2XYZ_MOU = var(STD_IC_XYZ2XYZ_MOU)) %>%
   mutate(meanLOC_IC_XYZ2M_MOU = mean(LOC_IC_XYZ2M_MOU)) %>% mutate(varLOC_IC_XYZ2M_MOU = var(LOC_IC_XYZ2M_MOU)) %>%
   mutate(meanSTD_IC_XYZ2M_MOU = mean(STD_IC_XYZ2M_MOU)) %>% mutate(varSTD_IC_XYZ2M_MOU = var(STD_IC_XYZ2M_MOU)) %>%
   mutate(meanSTD_IC_MOU = mean(STD_IC_MOU)) %>% mutate(varSTD_IC_MOU = var(STD_IC_MOU)) %>%
   mutate(meanISD_IC_MOU = mean(ISD_IC_MOU)) %>% mutate(varISD_IC_MOU = var(ISD_IC_MOU)) %>%
   mutate(meanROAM_OG_MOU = mean(ROAM_OG_MOU)) %>% mutate(varROAM_OG_MOU = var(ROAM_OG_MOU)) %>%
   mutate(meanROAM_IC_MOU = mean(ROAM_IC_MOU)) %>% mutate(varROAM_IC_MOU = var(ROAM_IC_MOU)) %>%
   mutate(meanTOTAL_IC_MOU = mean(TOTAL_IC_MOU)) %>% mutate(varTOTAL_IC_MOU = var(TOTAL_IC_MOU)) %>%
   mutate(req1 = max(as.logical(req1))) %>% 
   mutate(req2 = max(as.logical(req2))) %>% 
   mutate(AgeOnNet = min(AgeOnNet)) %>% 
   mutate(meannonRentRatio = mean(nonRentRatio)) %>% mutate(varnonRentRatio = var(nonRentRatio)) %>%
   mutate(meantotalSTD = mean(totalSTD)) %>% mutate(vartotalSTD = var(totalSTD)) %>%
   mutate(meantotalLocal = mean(totalLocal)) %>% mutate(vartotalLocal = var(totalLocal)) %>%
   mutate(meantotalHost = mean(totalHost)) %>% mutate(vartotalHost = var(totalHost)) %>%
   mutate(meantotalOther = mean(totalOther)) %>% mutate(vartotalOther = var(totalOther)) %>%
   mutate(meantotal_IC_XYZ2XYZ_MOU= mean(total_IC_XYZ2XYZ_MOU)) %>% mutate(vartotal_IC_XYZ2XYZ_MOU = var(total_IC_XYZ2XYZ_MOU)) %>%
   mutate(meantotal_OG_XYZ2XYZ_MOU = mean(total_OG_XYZ2XYZ_MOU)) %>% mutate(vartotal_OG_XYZ2XYZ_MOU = var(total_OG_XYZ2XYZ_MOU)) %>%
   mutate(meantotal_IC_XYZ2M_MOU = mean(total_IC_XYZ2M_MOU)) %>% mutate(vartotal_IC_XYZ2M_MOU = var(total_IC_XYZ2M_MOU)) %>%
   mutate(meantotal_OG_XYZ2M_MOU = mean(total_OG_XYZ2M_MOU)) %>% mutate(vartotal_OG_XYZ2M_MOU = var(total_OG_XYZ2M_MOU)) %>%
   mutate(meantotal_XYZ2XYZ_MOU = mean(total_XYZ2XYZ_MOU)) %>% mutate(vartotal_XYZ2XYZ_MOU = var(total_XYZ2XYZ_MOU)) %>%
   mutate(meantotal_XYZ2M_MOU = mean(total_XYZ2M_MOU)) %>% mutate(vartotal_XYZ2M_MOU = var(total_XYZ2M_MOU)) %>%
   mutate(meanXYZ2XYZ_XYZ2M_RATIO = mean(XYZ2XYZ_XYZ2M_RATIO)) %>% mutate(varXYZ2XYZ_XYZ2M_RATIO = var(XYZ2XYZ_XYZ2M_RATIO)) -> x
 
 
 x<-as.data.frame(x)
 y <- x[,c(1,35,49:119)]
 data <- distinct(y)
 data[is.na(data)] = 0
 write.csv(data,file="Validation_data.csv",sep=",",eol="\n",row.names=F,col.names=T,quote=F)
