# library
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)  

source("C:/Users/Christopher/Desktop/ING5/data analytics/project_part_1/fonction.r")
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
for(user in dfstats$User){
  dfstats<-Engagement_perWeek(dfstats,user)
}
dfstats$nbcig<-0
dfstats<-nbcigarettes(dflog,dfstats)
dfstats<-checkengagement(dfstats)
dfstats$progress<-0
dfstats<-progressfunction(dfstats)
dfstats$progressrate<-0
dfstats<-progressratefunction(dfstats)