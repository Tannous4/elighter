library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)

dflog <- read.csv2("C:/Users/Christopher/Desktop/ING5/data analytics/project_part_1/logs.csv" )
dflog$Date <- as.Date(as.character(dflog$Time), format="%d/%m/%Y %H:%M")
dflog$Week <- week(dflog$Date)
dflog$WDay <- wday(dflog$Date)

## A SINGLE USER
### 1 Info table

MoneySaved <- length(which(dflog$User == "Christophe Rouanet" & (dflog$Type == "Skipped" | dflog$Type == "Auto skipped")))


### 2 Classic table
#### Cigarettes consumption per weekday
Consumption <- length(which(dflog$User == "Christophe Rouanet" & (dflog$Type == "On time" | dflog$Type == "Cheated")))
ConsumptionWD <- length(which(dflog$User == "Christophe Rouanet" & (dflog$Type == "On time" | dflog$Type == "Cheated") & dflog$WDay < 6))
ConsumptionWE <- length(which(dflog$User == "Christophe Rouanet" & (dflog$Type == "On time" | dflog$Type == "Cheated") & dflog$WDay > 5))

#### Cigarettes consumption in last seven days
Consumption7 <- length(which(dflog$User == "Jordan Lafaille" & (dflog$Type == "On time" | dflog$Type == "Cheated") & as.Date(as.character("2017-11-01"), format="%Y-%m-%d") - dflog$Date < 8))

#### Mean and std of cigarette consumption per weekday
MeanWD <- length(which(dflog$User == "Christophe Rouanet" & (dflog$Type == "On time" | dflog$Type == "Cheated" | dflog$Type == "Behaviour") & dflog$Week == 30 & dflog$WDay < 6))/5

Stdlist <- c()
for(i in 1:5){
  Stdlist <- c(stdlist, length(which(dflog$User == "Christophe Rouanet" & (dflog$Type == "On time" | dflog$Type == "Cheated" | dflog$Type == "Behaviour") & dflog$Week == 30 & dflog$WDay == i)))
}
StdWD <- sd(stdlist)

#### Progress over all period
inputWeek <- 32
inputPerson <- "Christophe Rouanet"
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
Progress <- progressfunction(inputWeek,inputPerson)

#### Rate of progress
ProgressRate <- 0
if(Progress != 0){
  if(length(which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated") & dflog$Week == inputWeek-1)) > 0){
    ProgressLast <- progressfunction(inputWeek-1,inputPerson)
    ProgressRate <- (Progress - ProgressLast)/abs(ProgressLast)
  }else if(length(which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated") & dflog$Week == inputWeek-2)) > 0){
    ProgressLast <- progressfunction(inputWeek-2,inputPerson)
    ProgressRate <- (Progress - ProgressLast)/abs(ProgressLast)
  }
}
