# library
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)  

dflog <- read.csv2("C:/Users/Christopher/Desktop/ING5/data analytics/project_part_1/logs.csv" )
dflog$Date <- as.Date(as.character(dflog$Time), format="%d/%m/%Y %H:%M")

# Week
dflog$Week <- (-1)
for(name in unique(dflog$User)){
  Date<-dflog$Date[order(dflog$Date) & dflog$User == name][1]
  dflog$Week[which(dflog$User == name)]<- trunc(difftime(dflog$Date[which(dflog$User == name)],dflog$Date[which(dflog$User == name & dflog$Date == Date)],units = "week"),0)
}

# Week Day
dflog$WDay <- wday(dflog$Date)
dflog$TimeInput <-as.Date(dflog$Time, format('%d/%m/%Y %H:%M'))
dflog$TimeFormat <-dmy_hm(dflog$Time)
dflog$HourInput <-hour(dflog$TimeFormat)
dflog$MinuteInput <- minute(dflog$TimeFormat)

dfstats<-dflog[,c("User","Week")]
dfstats$temp<-1
dfstats<-dfstats%>%
  group_by(User, Week) %>%
  summarise(sum=sum(temp))
dfstats$sum<-NULL
dfstats$engagement<-1
dfstats$nbcig<-0
dfstats$progress<-0
dfstats$progressrate<-0

source("C:/Users/Christopher/Desktop/ING5/data analytics/project_part_1/fonction.r")

for(user in dfstats$User){
  dfstats<-Engagement_perWeek(dfstats,user)
}
dfstats<-nbcigarettes(dflog,dfstats)
dfstats<-checkengagement(dfstats)
dfstats<-progressfunction(dfstats)
dfstats<-progressratefunction(dfstats)

inputWeek <- 4
inputPerson <- "Abel Sharpe"
inputMode <- "Cheated"

#Single User
##################################################################################
##Info Tab
###Cigarettes Saved

CigSaved<-CigarettesSaved(dfstats,inputPerson)

###Money Saved

MoneySaved<-CigarettesSaved(dfstats,inputPerson)

###Overall Progress Average

OverallProgress <- overallprogress(dfstats, inputPerson)

###Overall Progress Cgy

OPCgy <- overallprogresscgy(dfstats, inputPerson)

###Overall Engagement Average

OverallEngagement<-overallengagement(dfstats, inputPerson)

###Best Rate of Progress

BestProgressRate<-bestprogressrate(dfstats, inputPerson)

### Mean Per Day

MeanPerDay<-meanperday(dflog,inputPerson)

### Mean Per WDay

MeanPerWeekDay<-meanweekday(dflog,inputPerson)

### Mean Per WEnd

MeanPerWeekEnd<-meanweekend(dflog,inputPerson)

### Most Smoking Intensity Slot

MostSmokingSlot<-mostsmokingslot(dflog, inputPerson)

#############################################################################
##Classic Tab
### Cigarettes consumption per weekday

dfcons<-cigconsumption(dflog,inputPerson)

### Cigarettes consumption weekday

CigConsWDay <- cigconsumptionwday(dflog,inputPerson)

### Cigarettes consumption weekend

CigConsWEnd <- cigconsumptionwend(dflog,inputPerson)

### Cigarettes consumption in last seven days

CigCons7 <- cigconsumption7(dfstats,inputPerson)

### Mean of cigarette consumption per weekday

dfmean<-meancons(dflog,inputPerson)

### std of cigarette consumption per weekday

dfstd<-stdcons(dflog,inputPerson)

### Progress over all period

dfprogress<-dfstats[which(dfstats$User == inputPerson), c("User","Week","progress")]

### Progress Rate

dfprogressrate<-dfstats[which(dfstats$User == inputPerson), c("User","Week","progressrate")]

## Week Tab
### Cigarettes per weekday per time slot

CigarettePerWeekdayPerTimeSlot <- Cigarette_perWD_perTS(dflog, inputWeek , inputPerson)

### Comparision of cigarettes consumption between weeks

ConsumptionBetweenWeeks <- Consumption_Between_Weeks(dflog, inputPerson)

### Mode usage per week 

ModeUsagePerWeek <- Mode_Usage_Per_Week(dflog, inputPerson, inputMode)

### Cigarette Consumption per weekday

CigaretteConsumptionPerWeekday <- Cigarette_Consumption_perWD(dflog, inputWeek, inputPerson)

#########################################################################
## Engagement tab 
### Engagement over all period 

### Engagement per day

EngagementPerDay <- Engagement_perDay (dflog, inputPerson)

### Engagement per week

EngagementPerWeek<- EngagementPerWeek (dfstats, inputPerson)


#########################################################################
## All days Tab
### Cigarettes consumption over all period 

CigarettesConsumptionOverAllPeriod <- Consumption_Over_All_Period (dflog, inputPerson)

### Mode usage over all period
ModeUsageOverAllPeriod<- Mode_Usage (dflog, inputPerson, inputMode)

#All Users
##################################################################################
### 1. Information Tab
### Total Number of saved cigarettes

TotalNumberOfSavedCigarettes<-total_number_of_cigarettes_saved(dflog,dfstats)

### Total Number of Money saved

TotalNumberOfSavedMoney<-total_number_of_money_saved(dflog,dfstats)

### Avg number of saved cigarettes

AvgCigSaved <- avg_nb_cig(dflog,dfstats)

### Average amount of money saved

AvgMoneySaved <- avg_money(dflog,dfstats)

########################################################
### Classic
### Mean and std of cigarette consumption per weekday

MeanStdCigarette <- mean_consumption_std_weekday (dflog)

### Cigarettes per weekday per time slots

CigPerWdayPerTsRes <- ALL_cigarettes_per_WDAY_per_TS(dflog)

### Overall Progress

dfprogall<-progressalluser(dfstats)

########################################################
### Engagement Overall

dfengall<-engagementoverall(dfstats)