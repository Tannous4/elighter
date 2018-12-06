# library
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)

dflog <- read.csv2("C:/Users/Christopher/Desktop/ING5/data analytics/project_part_1/logs.csv" )
dflog$Date <- as.Date(as.character(dflog$Time), format="%d/%m/%Y %H:%M")

# function progress
progressfunction <- function(inputWeek, inputPerson){
  result <- 0
  conditionWeek <- inputWeek - max(dflog$Week[which(dflog$User == inputPerson & dflog$Type == "Behaviour")])
  if(conditionWeek > 0 & conditionWeek < 3){
    result <- ((length(which(dflog$User == inputPerson & dflog$Type == "Behaviour"))
                - length(which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated") & dflog$Week == inputWeek)))
               / length(which(dflog$User == inputPerson & dflog$Type == "Behaviour")))
  }
  if(conditionWeek > 2){
    temp <- length(which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated") & (dflog$Week == inputWeek-1 | dflog$Week == inputWeek-2 | dflog$Week == inputWeek-3)))/3
    result <- ((temp
                - length(which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated") & dflog$Week == inputWeek)))
               / temp)
  }
  return(result)
}

progressratefunction <- function(inputWeek, inputPerson){
  ProgressRate <- 0
  Progress<-progressfunction(inputWeek,inputPerson)
  if(Progress != 0){
    if(length(which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated") & dflog$Week == inputWeek-1)) > 0){
      ProgressLast <- progressfunction(inputWeek-1,inputPerson)
      ProgressRate <- (Progress - ProgressLast)/abs(ProgressLast)
    }else if(length(which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated") & dflog$Week == inputWeek-2)) > 0){
      ProgressLast <- progressfunction(inputWeek-2,inputPerson)
      ProgressRate <- (Progress - ProgressLast)/abs(ProgressLast)
    }
  }
  if(ProgressRate)
  return(ProgressRate)
}

# Week
dflog$Week <- (-1)
for(name in unique(dflog$User)){
  Date<-dflog$Date[order(dflog$Date) & dflog$User == name][1]
  dflog$Week[which(dflog$User == name)]<- trunc(difftime(dflog$Date[which(dflog$User == name)],dflog$Date[which(dflog$User == name & dflog$Date == Date)],units = "week"),0)
}

# Week Day
dflog$WDay <- wday(dflog$Date)

inputWeek <- 4
inputPerson <- "Joseph Toussaint"
print(inputWeek)
print(inputPerson)

## A SINGLE USER
### 1 Info table

#### Cigarettes Saved
CigarettesSaved<-c()
for(w in unique(dflog$Week[which(dflog$User == inputPerson)])){
  CigarettesSaved<-c(CigarettesSaved, sum(length(which(dflog$User == inputPerson & dflog$Type == "Behaviour")) 
                                         - length(which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated") & dflog$Week == w))))
}
CigarettesSaved<-sum(CigarettesSaved)
print("Cigarettes Saved")
print(CigarettesSaved)

#### Money Saved
MoneySaved <- CigarettesSaved
print("Money Saved")
print(MoneySaved)

#### Overall Progress
OverallProgress <- c()
for(w in unique(dflog$Week[which(dflog$User == inputPerson & dflog$Week != 0)])){
  OverallProgress<-c(OverallProgress, progressfunction(w,inputPerson))
}
OverallProgress <- mean(OverallProgress)
print("Overall Progress")
print(OverallProgress)

#### Overall Progress Category
OverallProgressCgy <- "nothing"
if(OverallProgress <= 0.2){
  OverallProgressCgy<-"Low"
}else if(OverallProgress >= 0.5){
  OverallProgressCgy<-"High"
}else{
  OverallProgressCgy<-"Medium"
}
print("Overall Progress Cgy")
print(OverallProgressCgy)

#### Overall Engagement

#### Best Rate of Progress

BestRateProgress <- c()
for(w in unique(dflog$Week[which(dflog$User == inputPerson & dflog$Week != 0)])){
  BestRateProgress<-c(BestRateProgress, progressratefunction(w,inputPerson))
}
#BestRateProgress <- max(BestRateProgress)

print("BestRateProgress")
print(BestRateProgress)

#### Mean of consumed cigarettes

#### Mean of consumed cigarettes in weekdays

#### Mean of consumed cigarettes in weekends

#### Most Smoking Intensity Slot



### 2 Classic table
#### Cigarettes consumption per weekday
Consumption <- length(which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated")))
ConsumptionWD <- length(which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated") & dflog$WDay < 6))
ConsumptionWE <- length(which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated") & dflog$WDay > 5))

#### Cigarettes consumption in last seven days
Consumption7 <- length(which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated") & dflog$Week == max(dflog$Week[which(dflog$User == inputPerson)])))

#### Mean and std of cigarette consumption per weekday
MeanWD <- length(which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated" | dflog$Type == "Behaviour") & dflog$Week == inputWeek & dflog$WDay < 6))/5

Stdlist <- c()
for(i in 1:5){
  Stdlist <- c(stdlist, length(which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated" | dflog$Type == "Behaviour") & dflog$Week == 30 & dflog$WDay == i)))
}
StdWD <- sd(stdlist)

#### Progress over all period

Progress <- progressfunction(inputWeek,inputPerson)

#### Rate of progress
ProgressRate <- progressratefunction(inputWeek,inputPerson)
