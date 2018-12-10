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
