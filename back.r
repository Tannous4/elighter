library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)

mydf <- read.csv2("C:/Users/Christopher/Desktop/ING5/data analytics/project_part_1/logs.csv" )
mydf$Week <- strftime(mydf$Time, format = "%V")
mydf$WDay <- wday(mydf$Time)
mydf$Date <- as.Date(as.character(mydf$Time), format="%d/%m/%Y %H:%M")

## A SINGLE USER
### 1 Info table

MoneySaved <- length(which(mydf$User == "Christophe Rouanet" & (mydf$Type == "Skipped" | mydf$Type == "Auto skipped")))


### 2 Classic table
#### Cigarettes consumption per weekday
Consumption <- length(which(mydf$User == "Christophe Rouanet" & (mydf$Type == "On time" | mydf$Type == "Cheated")))
ConsumptionWD <- length(which(mydf$User == "Christophe Rouanet" & (mydf$Type == "On time" | mydf$Type == "Cheated") & mydf$WDay < 6))
ConsumptionWE <- length(which(mydf$User == "Christophe Rouanet" & (mydf$Type == "On time" | mydf$Type == "Cheated") & mydf$WDay > 5))

#### Cigarettes consumption in last seven days
Consumption7 <- length(which(mydf$User == "Jordan Lafaille" & (mydf$Type == "On time" | mydf$Type == "Cheated") & as.Date(as.character("2017-11-01"), format="%Y-%m-%d") - mydf$Date < 8))

#### Mean and std of cigarette consumption per weekday
MeanWD <- length(which(mydf$User == "Christophe Rouanet" & (mydf$Type == "On time" | mydf$Type == "Cheated" | mydf$Type == "Behaviour") & mydf$Week == 30))/5

