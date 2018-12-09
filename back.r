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

### 3 Week Tab
### Cigarettes per weekday per time slots
### TIME FORMAT
dflog$TimeInput <-as.Date(dflog$Time, format('%d/%m/%Y %H:%M'))
dflog$TimeFormat <-dmy_hm(dflog$Time)
dflog$HourInput <-hour(dflog$TimeFormat)
dflog$MinuteInput <- minute(dflog$TimeFormat)



stat_day <- function(weekday , dfWeek ){
  
  result <- c()
  result <-c(length(dfWeek$User[which(dfWeek$WDay== weekday & dfWeek$HourInput >= 0 & dfWeek$HourInput < 2)]),
             length(dfWeek$User[which(dfWeek$WDay== weekday & dfWeek$HourInput >= 2 & dfWeek$HourInput < 4)]),
             length(dfWeek$User[which(dfWeek$WDay== weekday & dfWeek$HourInput >= 4 & dfWeek$HourInput < 6)]),
             length(dfWeek$User[which(dfWeek$WDay== weekday & dfWeek$HourInput >= 6 & dfWeek$HourInput < 8)]),
             length(dfWeek$User[which(dfWeek$WDay== weekday & dfWeek$HourInput >= 8 & dfWeek$HourInput< 10)]),
             length(dfWeek$User[which(dfWeek$WDay== weekday & dfWeek$HourInput >= 10 & dfWeek$HourInput < 12)]),
             length(dfWeek$User[which(dfWeek$WDay== weekday & dfWeek$HourInput >= 12 & dfWeek$HourInput < 14)]),
             length(dfWeek$User[which(dfWeek$WDay== weekday & dfWeek$HourInput >= 14 & dfWeek$HourInput < 16)]),
             length(dfWeek$User[which(dfWeek$WDay== weekday & dfWeek$HourInput >= 16 & dfWeek$HourInput < 18)]),
             length(dfWeek$User[which(dfWeek$WDay== weekday & dfWeek$HourInput >= 18 & dfWeek$HourInput < 20)]),
             length(dfWeek$User[which(dfWeek$WDay== weekday & dfWeek$HourInput >= 20 & dfWeek$HourInput < 22)]),
             length(dfWeek$User[which(dfWeek$WDay== weekday & dfWeek$HourInput >= 22 & dfWeek$HourInput < 0)]))
  return(result)
}

Cigarette_perWD_perTS <- function(inputWeek, inputPerson){
  result <- c()
  
  if (inputWeek ==0){
    dfWeek <- dflog[which(dflog$User == inputPerson & dflog$Week == inputWeek & dflog$Type == "Behaviour"),]
    
    #in case we are in behavior week 
    monday <- stat_day(1, dfWeek )
    tuesday <- stat_day(2, dfWeek)
    wednesday<- stat_day(3 , dfWeek)
    thursday <- stat_day(4, dfWeek)
    friday <- stat_day(5, dfWeek)
    saturday <- stat_day(6, dfWeek)
    sunday <- stat_day(7, dfWeek)
    
    result <- c(monday, tuesday, wednesday, thursday, friday, saturday, sunday )
    
  }else{
    
    dfWeek <- dflog[which(dflog$User == inputPerson & dflog$Week == inputWeek & (dflog$Type == "Cheated" | dflog$Type == "On time")),]
    
    monday <- stat_day(1, dfWeek)
    tuesday <- stat_day(2, dfWeek)
    wednesday<- stat_day(3, dfWeek)
    thursday <- stat_day(4, dfWeek)
    friday <- stat_day(5, dfWeek)
    saturday <- stat_day(6, dfWeek)
    sunday <- stat_day(7, dfWeek)
    
    result <- c(monday, tuesday, wednesday, thursday, friday, saturday, sunday )
  }
  
  matrix_res <- matrix(result, nrow=7, ncol=12, byrow=T)
  return(matrix_res)
}

#If you want to test it
resultat <- Cigarette_perWD_perTS(0, "Armel Duret")
print(resultat)

### Comparision of cigarettes consumption between weeks
Consumption_Between_Weeks <- function(inputPerson){
  nb_weeks <- length(unique(dflog$Week[which(dflog$User == inputPerson)]))
  result <- c()
  
  for (i in 0:(nb_weeks-1)){
    avg <- 0
    if (i == 0){
      dfWeek <- dflog[which(dflog$User == inputPerson & dflog$Week == i & dflog$Type == "Behaviour"),]
    }else{
      dfWeek <- dflog[which(dflog$User == inputPerson & dflog$Week == i & (dflog$Type == "Cheated" | dflog$Type == "On time")),]
    }
    sum_week <-0
    for (d in 1:7){
      sum_week <- sum_week + sum(stat_day(d,dfWeek ))
    }
    avg <- round(sum_week/7)
    result <- c(result, avg)
  }
  return(result)
}
res <- Consumption_Between_Weeks("Armel Duret")
print(res)

### Mode usage per week 
Consumption_Between_Weeks <- function(inputPerson, inputMode){
  nb_weeks <- length(unique(dflog$Week[which(dflog$User == inputPerson)]))
  result <- c()
  
  for (i in 0:(nb_weeks-1)){
    dfWeek <- dflog[which(dflog$User == inputPerson & dflog$Week == i & dflog$Type == inputMode),]
    sum_week <-0
    for (d in 1:7){
      sum_week <- sum_week + sum(stat_day(d,dfWeek ))
    }
    result <- c(result, sum_week)
  }
  return(result)
}
res2 <- Consumption_Between_Weeks("Baptiste Mallet", "Cheated")
print(res2)

### Cigarette Consumption per weekday

### 4 Engagement tab 
### Engagement over all period 

### 5 All days Tab
### Cigarettes consumption over all period 

### Mode usage over all period

