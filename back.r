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
CigarettesSaved <- function (inputPerson){
  result<-c()
  for(w in unique(dflog$Week[which(dflog$User == inputPerson)])){
    result<-c(result, sum(length(which(dflog$User == inputPerson & dflog$Type == "Behaviour")) 
                          - length(which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated") & dflog$Week == w))))
  }
  result<-sum(result)
  return (result)
}
cig_saved_res<-CigarettesSaved("Joseph Toussaint")
print(cig_saved_res)

#### Money Saved
MoneySaved <- function (inputPerson){
  result<-c()
  #If we want to change the price of one cigarette one day
  set_price <- 1 
  for(w in unique(dflog$Week[which(dflog$User == inputPerson)])){
    result<-c(result, sum(length(which(dflog$User == inputPerson & dflog$Type == "Behaviour")) 
                          - length(which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated") & dflog$Week == w))))
  }
  result<-sum(result)*set_price
  return (result)
}
money_saved_res<-MoneySaved("Joseph Toussaint")
print(money_saved_res)

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

progress_over_all_period <- function(inputPerson){
  nb_weeks<- length(unique(dflog$Week[which(dflog$User == inputPerson)]))
  all_weeks <- unique(dflog$Week[which(dflog$User == inputPerson)])
  
  progress <- c()
  for (i in 1:nb_weeks){
    tmp_progress<- progressfunction(all_weeks[i], inputPerson)
    progress <- c(progress, tmp_progress)
  }
  result <- data.frame("Progress"= progress , "Week"= all_weeks)
  return(result)
}

res_pro <- progress_over_all_period("Renaud Courbis")
print(res_pro)

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
Cigarette_Consumption_perWD <- function(inputWeek, inputPerson) {
  result <- c()
  
  if (inputWeek ==0){
    dfWeek <- dflog[which(dflog$User == inputPerson & dflog$Week == inputWeek & dflog$Type == "Behaviour"),]
    
    #in case we are in behavior week 
    monday <- sum(stat_day(1, dfWeek ))
    tuesday <- sum(stat_day(2, dfWeek))
    wednesday<- sum(stat_day(3 , dfWeek))
    thursday <- sum(stat_day(4, dfWeek))
    friday <- sum(stat_day(5, dfWeek))
    saturday <- sum(stat_day(6, dfWeek))
    sunday <- sum(stat_day(7, dfWeek))
    
    result <- c(monday, tuesday, wednesday, thursday, friday, saturday, sunday )
    
  }else{
    
    dfWeek <- dflog[which(dflog$User == inputPerson & dflog$Week == inputWeek & (dflog$Type == "Cheated" | dflog$Type == "On time")),]
    
    monday <- sum(stat_day(1, dfWeek))
    tuesday <- sum(stat_day(2, dfWeek))
    wednesday<- sum(stat_day(3, dfWeek))
    thursday <- sum(stat_day(4, dfWeek))
    friday <- sum(stat_day(5, dfWeek))
    saturday <- sum(stat_day(6, dfWeek))
    sunday <- sum(stat_day(7, dfWeek))
    
    result <- c(monday, tuesday, wednesday, thursday, friday, saturday, sunday )
  }
  return(result)
}
res3 <- Cigarette_Consumption_perWD(1, "Armel Duret")
print(res3) 

### 4 Engagement tab 
### Engagement over all period 
### Engagement per day
Engagement_perDay <- function(inputPerson){
  nb_days <- length(unique(dflog$TimeInput[which(dflog$User == inputPerson)]))
  #List of the days WARNING first element at indice 1
  all_days <- unique(dflog$TimeInput[which(dflog$User == inputPerson)])
  result <- c()
  
  for (i in 1:nb_days){
    dfWeek <- dflog[which(dflog$User == inputPerson & dflog$TimeInput== all_days[i]),]
    if(dfWeek$Week >=1){
      nb_autoskip <- length(dfWeek$Type[which(dfWeek$Type == "Auto skipped")])
      normalize <- length(dfWeek$Type[which(dfWeek$Type == "Auto skipped" | dfWeek$Type == "On time" | dfWeek$Type == "Skipped" | dfWeek$Type == "Snoozed")]) 
      print(all_days[i])
      if(normalize != 0){
        engagement <- 1-(nb_autoskip/normalize)
        result <- c(result, engagement )
      }else{
        result <- c(result, 0)#the user doesn't use the lighter at all
      }
    } else {
      result <- c(result, 0)# when we are in behaviour mode
    }
  }
  Final_result<-list( result, all_days) 
  return(Final_result)
}
engagement_day_res<- Engagement_perDay ("Baptiste Mallet")
print(engagement_day_res)
print(length(engagement_day_res[[1]]))
print(engagement_day_res[[1]][1])
print(engagement_day_res[[2]][1])

### Engagement per week
Engagement_perWeek <- function(inputPerson){
  nb_weeks <- length(unique(dflog$Week[which(dflog$User == inputPerson)]))
  print(nb_weeks)
  #List of the days WARNING first element at indice 1
  all_weeks <- unique(dflog$Week[which(dflog$User == inputPerson)])
  result <- c()
  
  for (i in 1:nb_weeks){
    print(all_weeks[i])
    dfWeek <- dflog[which(dflog$User == inputPerson & dflog$Week== all_weeks[i]),]
    if(all_weeks[i] >=1){
      nb_autoskip <- length(dfWeek$Type[which(dfWeek$Type == "Auto skipped")])
      normalize <- length(dfWeek$Type[which(dfWeek$Type == "Auto skipped" | dfWeek$Type == "On time" | dfWeek$Type == "Skipped" | dfWeek$Type == "Snoozed")])
      if(normalize != 0){
        engagement <- 1-(nb_autoskip/normalize)
        if(engagement <0.4){
          engagement <- result[length(result)]
        }
        result <- c(result, engagement )
      }else{
        #When the lighter is not used at all we put the engagement value to 0 anyway
        result <- c(result, 0)#the user doesn't use the lighter at all
      }
    } else {
      result <- c(result, 0)# when we are in behaviour mode
    }
    
  }
  Final_result<-list( result, all_weeks) 
  return(Final_result)
}
engagement_week_res<- Engagement_perWeek ("Abel Sharpe")
print(engagement_week_res)
print(length(engagement_week_res[[1]]))
print(engagement_week_res[[1]][1])
print(engagement_week_res[[2]][1])

### 5 All days Tab
### Cigarettes consumption over all period 
Consumption_Over_All_Period <- function(inputPerson){
  nb_days <- length(unique(dflog$TimeInput[which(dflog$User == inputPerson)]))
  print(nb_days)
  #List of the days WARNING first element at indice 1
  all_days <- unique(dflog$TimeInput[which(dflog$User == inputPerson)])
  result <- c()
  
  for (i in 1:nb_days){
    sum_usages <-0
    dfWeek <- dflog[which(dflog$User == inputPerson & dflog$TimeInput== all_days[i] & (dflog$Type == "Cheated" | dflog$Type == "On time" | dflog$Type == "Behaviour")),]
    if (dfWeek$Week==0){
      sum_usages <-  length(dfWeek$Type[which(dfWeek$Type == "Behaviour")])
    }else{
      sum_usages <- length(dfWeek$Type[which(dfWeek$Type == "Cheated" | dfWeek$Type == "On time")])
    }
    result <- c(result, sum_usages)
    
  }
  Final_result<-list( result, all_days) 
  return(Final_result)
}

Consumption_res<- Consumption_Over_All_Period ("Baptiste Mallet")
print(Consumption_res)
print(Consumption_res[[1]][1])
print(Consumption_res[[2]][1])

### Mode usage over all period
Mode_Usage <- function(inputPerson, inputMode){
  nb_days <- length(unique(dflog$TimeInput[which(dflog$User == inputPerson & dflog$Type == inputMode)]))
  #List of the days WARNING first element at indice 1
  all_days <- unique(dflog$TimeInput[which(dflog$User == inputPerson & dflog$Type == inputMode)])
  result <- c()
  
  for (i in 1:nb_days){
    dfWeek <- dflog$Type[which(dflog$User == inputPerson & dflog$TimeInput== all_days[i] & dflog$Type == inputMode)]
    sum_usages <-0
    sum_usages <- sum_usages + length(dfWeek)
    result <- c(result, sum_usages)
    
  }
  Final_result<-list( result, all_days) 
  return(Final_result)
}

Mode_res<- Mode_Usage ("Baptiste Mallet", "Cheated")
print(Mode_res[[1]][1])
print(Mode_res[[2]][1])

### ALL USER PART
#------------------------------------------------------------------------------------------
### 1. Information Tab
### Total Number of saved cigarettes
total_number_of_cigarettes_saved <- function(){
  nb_users <- length(unique(dflog$User))
  all_users <- unique(dflog$User)
  sum_cig_saved <-0
  for (i in 1:nb_users){
    sum_cig_saved <- sum_cig_saved + CigarettesSaved(all_users[i])
  }
  return(sum_cig_saved)
}
Total_number_of_saved_cigarettes<-total_number_of_cigarettes_saved()
print(Total_number_of_saved_cigarettes)

### Total Number of Money saved 
total_number_of_money_saved <- function(){
  nb_users <- length(unique(dflog$User))
  all_users <- unique(dflog$User)
  sum_money_saved <-0
  for (i in 1:nb_users){
    sum_money_saved <- sum_money_saved + MoneySaved(all_users[i])
  }
  return(sum_money_saved)
}
Total_number_of_saved_money<-total_number_of_money_saved()
print(Total_number_of_saved_money)

### Avg number of saved cigarettes
avg_nb_cig <- function (){
  nb_users <- length(unique(dflog$User))
  total<-total_number_of_money_saved()
  
  avg <- round(total/nb_users)
  return(avg)
}
avg_cig_saved <- avg_nb_cig()
print(avg_cig_saved)

### Average amount of money saved 
avg_money <- function (){
  nb_users <- length(unique(dflog$User))
  total<-total_number_of_money_saved ()
  
  avg <- round(total/nb_users)
  return(avg)
}
avg_money_saved <- avg_money()
print(avg_money_saved )

### 2. Classic
### Mean and std of cigarette consumption per weekday

### Average progress of all users
avg_progress_all_user <- function (){
  nb_weeks<- length(unique(dflog$Week))
  all_weeks <- unique(dflog$Week)
  
  nb_users <- length(unique(dflog$User))
  all_users <- unique(dflog$User)
  avg <- c()
  for (i in 1:nb_weeks){
    nb_user_tmp <- 0
    sum_progress <- 0
    for (u in 1:nb_users){
      res_pro <- progress_over_all_period(all_users[u])
      if(i <= length(res_pro$Week)){
        nb_user_tmp <- nb_user_tmp + 1
        sum_progress  <- sum_progress + res_pro$Progress[i]
      }
      
    }
    avg_week <- sum_progress /nb_user_tmp
    avg <- c(avg, avg_week)
    
  }
  print(length(avg))
  result <- data.frame("Avgprogress"= avg , "Week"= all_weeks)
  
}

avg_progress <- avg_progress_all_user ()
print(avg_progress)

### Cigarettes per weekday per time slots
### Average rate of progress of all users

### 3. Engagement 
### Engagement over all period
total_engagement_all_user <- function (){
  nb_weeks<- length(unique(dflog$Week))
  all_weeks <- unique(dflog$Week)
  
  nb_users <- length(unique(dflog$User))
  all_users <- unique(dflog$User)
  avg <- c()
  for (i in 1:nb_weeks){
    nb_user_tmp <- 0
    sum_engagement <- 0
    for (u in 1:nb_users){
      engagement_week_res<- Engagement_perWeek (all_users[u])
      print(engagement_week_res)
      if(i <= length(engagement_week_res[[1]])){
        nb_user_tmp <- nb_user_tmp + 1
        sum_engagement <- sum_engagement + engagement_week_res[[1]][i]
      }
      
    }
    avg_week <- sum_engagement/nb_user_tmp
    avg <- c(avg, avg_week)
    
  }
  print(length(avg))
  result <- data.frame("AvgEngagement"= avg , "Week"= all_weeks)
  
}

engagement <- total_engagement_all_user ()
print(engagement)

