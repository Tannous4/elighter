### Engagement per week
Engagement_perWeek <- function(df,user){
  nb_weeks <- length(unique(dflog$Week[which(dflog$User == user)]))
  #List of the days WARNING first element at indice 1
  all_weeks <- unique(dflog$Week[which(dflog$User == user)])
  result <- c()
  for (i in 1:nb_weeks){
    dfWeek <- dflog[which(dflog$User == user & dflog$Week== all_weeks[i]),]
    if(all_weeks[i] >=1){
      nb_autoskip <- length(dfWeek$Type[which(dfWeek$Type == "Auto skipped")])
      normalize <- length(dfWeek$Type[which(dfWeek$Type == "Auto skipped" | dfWeek$Type == "On time" | dfWeek$Type == "Skipped" | dfWeek$Type == "Snoozed")])
      if(normalize != 0){
        engagement <- 1-(nb_autoskip/normalize)
        df$engagement[which(df$User == user & df$Week == all_weeks[i])]<-engagement
      }else{
        #When the lighter is not used at all we put the engagement value to 0 anyway
        df$engagement[which(df$User == user & df$Week == all_weeks[i])]<- 0#the user doesn't use the lighter at all
      }
    } else {
      df$engagement[which(df$User == user & df$Week == all_weeks[i])]<-0# when we are in behaviour mode
    }
    
  }
  return(df)
}

#number of cig
nbcigarettes<-function(dflog,dfstats){
  for(user in unique(dfstats$User)){
    for(week in unique(dfstats$Week[which(dfstats$User == user)])){
      dfstats$nbcig[which(dfstats$User == user & dfstats$Week == week)]<-length(which(dflog$User == user & (dflog$Type == "On time" | dflog$Type == "Cheated" | dflog$Type == "Behaviour") & dflog$Week == week))
    }
  }
  return(dfstats)
}

checkengagement<-function(dfstats){
  index<-which(dfstats$engagement<0.4)
  for(i in index){
    name<-dfstats$User[i]
    week<-dfstats$Week[i]
    dfsub<-dfstats[which(dfstats$User == name & dfstats$Week < week & dfstats$engagement>0.4),]
    if(length(dfsub$Week)>0){
      dfstats$nbcig[i]<-dfsub$nbcig[which(dfsub$Week == max(dfsub$Week))]
    }else{
      dfstats$nbcig[i]<-dfstats$nbcig[which(dfstats$Week == 0 & dfstats$User == dfstats$User[i])]
    }
    
  }
  return(dfstats)
}

# function progress
progressfunction <- function(dfstats){
  for(name in unique(dfstats$User)){
    for(week in dfstats$Week[which(dfstats$User == name)]){
      result <- 0
      if(week < 3){
        result <- ((dfstats$nbcig[which(dfstats$User==name & dfstats$Week == 0)]
                    - dfstats$nbcig[which(dfstats$User==name & dfstats$Week == week)])
                    /dfstats$nbcig[which(dfstats$User==name & dfstats$Week == 0)])
      }
      if(week > 2){
        temp <- sum(dfstats$nbcig[which(dfstats$User == name & (dfstats$Week == week-1 | dfstats$Week == week-2 | dfstats$Week == week-3))])/3
        if(temp>0){
          result <- ((temp
                      - dfstats$nbcig[which(dfstats$User == name & dfstats$Week == week)])
                     / temp)
        }
      }
      if(abs(result) < 0.001){
        result<-0
      }
      dfstats$progress[which(dfstats$User == name & dfstats$Week == week)]<-result
    }
  }
  
  
  return(dfstats)
}

# progress rate
progressratefunction <- function(dfstats){
  for(name in unique(dfstats$User)){
    for(week in dfstats$Week[which(dfstats$User == name)]){
      ProgressRate <- 0
      Progress<-dfstats$progress[which(dfstats$User == name & dfstats$Week == week)]
      if(length(Progress) > 0){
        ProgressLast <- dfstats$progress[which(dfstats$User == name & dfstats$Week == week-1)]
        if(length(ProgressLast)>0){
          if(abs(ProgressLast) > 0){
            ProgressRate <- (Progress - ProgressLast)/abs(ProgressLast)
          }else if(length(dfstats$progress[which(dfstats$User == name & dfstats$Week == week-2)]) > 0){
            if(abs(dfstats$progress[which(dfstats$User == name & dfstats$Week == week-2)]) > 0){
              ProgressLast <- dfstats$progress[which(dfstats$User == name & dfstats$Week == week-2)]
              ProgressRate <- (Progress - ProgressLast)/abs(ProgressLast)
            }
          }
        }
      }
      dfstats$progressrate[which(dfstats$User == name & dfstats$Week == week)]<-ProgressRate
    }
  }
  return(dfstats)
}

#Cigarettes Saved
CigarettesSaved <- function (dfstats,inputPerson){
  result<-c()
  for(w in unique(dfstats$Week[which(dfstats$User == inputPerson)])){
    result<-c(result, dfstats$nbcig[which(dfstats$User == inputPerson & dfstats$Week == 0)] - dfstats$nbcig[which(dfstats$User == inputPerson & dfstats$Week == w)])
  }
  result<-sum(result)
  return (result)
}

#Overall Progress Average
overallprogress <- function(dfstats, inputPerson){
  return(mean(dfstats$progress[which(dfstats$User == inputPerson & dfstats$Week != 0)]))
}

overallprogresscgy <- function(dfstats, inputPerson){
  op<-overallprogress(dfstats, inputPerson)
  opcgy <- "nothing"
  if(op <= 0.2){
    opcgy<-"Low"
  }else if(op >= 0.5){
    opcgy<-"High"
  }else{
    opcgy<-"Medium"
  }
  return(opcgy)
}

#Overall Engagement Average
overallengagement <- function(dfstats, inputPerson){
  return(mean(dfstats$engagement[which(dfstats$User == inputPerson & dfstats$Week != 0)]))
}

#Best Rate of Progress
bestprogressrate <- function(dfstats, inputPerson){
  return(max(dfstats$progressrate[which(dfstats$User == inputPerson & dfstats$Week != 0)]))
}

#Mean Per Day
meanperday<-function(dflog,inputPerson){
  dfsub<-dflog[which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated" | dflog$Type == "Behaviour")),]
  mpd<-length(dfsub$User)/length(unique(dfsub$TimeInput))
  return(mpd)
}

#Mean Per Week Day
meanweekday<-function(dflog,inputPerson){
  dfsub<-dflog[which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated" | dflog$Type == "Behaviour") & dflog$WDay < 6),]
  mpd<-length(dfsub$User)/length(unique(dfsub$TimeInput))
  return(mpd)
}

#Mean Per Week Ends
meanweekend<-function(dflog,inputPerson){
  dfsub<-dflog[which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated" | dflog$Type == "Behaviour") & dflog$WDay > 5),]
  mpd<-length(dfsub$User)/length(unique(dfsub$TimeInput))
  return(mpd)
}

# Most Smoking Intensity Slot
mostsmokingslot <- function(dflog, inputPerson){
  result <- c()
  slot<-c("OH - 2H","2H - 4H","4H - 6H","6H - 8H","8H - 10H","10H - 12H","12H - 14H","14H - 16H","16H - 18H","18H - 20H","20H - 22H","22H - 0H")
  result <-c(length(which(dflog$User == inputPerson & dflog$HourInput >= 0 & dflog$HourInput < 2))/length(unique(dflog$TimeInput[which(dflog$User == inputPerson & dflog$HourInput >= 0 & dflog$HourInput < 2)])),
             length(which(dflog$User == inputPerson & dflog$HourInput >= 2 & dflog$HourInput < 4))/length(unique(dflog$TimeInput[which(dflog$User == inputPerson & dflog$HourInput >= 2 & dflog$HourInput < 4)])),
             length(which(dflog$User == inputPerson & dflog$HourInput >= 4 & dflog$HourInput < 6))/length(unique(dflog$TimeInput[which(dflog$User == inputPerson & dflog$HourInput >= 4 & dflog$HourInput < 6)])),
             length(which(dflog$User == inputPerson & dflog$HourInput >= 6 & dflog$HourInput < 8))/length(unique(dflog$TimeInput[which(dflog$User == inputPerson & dflog$HourInput >= 6 & dflog$HourInput < 8)])),
             length(which(dflog$User == inputPerson & dflog$HourInput >= 8 & dflog$HourInput< 10))/length(unique(dflog$TimeInput[which(dflog$User == inputPerson & dflog$HourInput >= 8 & dflog$HourInput < 10)])),
             length(which(dflog$User == inputPerson & dflog$HourInput >= 10 & dflog$HourInput < 12))/length(unique(dflog$TimeInput[which(dflog$User == inputPerson & dflog$HourInput >= 10 & dflog$HourInput < 12)])),
             length(which(dflog$User == inputPerson & dflog$HourInput >= 12 & dflog$HourInput < 14))/length(unique(dflog$TimeInput[which(dflog$User == inputPerson & dflog$HourInput >= 12 & dflog$HourInput < 14)])),
             length(which(dflog$User == inputPerson & dflog$HourInput >= 14 & dflog$HourInput < 16))/length(unique(dflog$TimeInput[which(dflog$User == inputPerson & dflog$HourInput >= 14 & dflog$HourInput < 16)])),
             length(which(dflog$User == inputPerson & dflog$HourInput >= 16 & dflog$HourInput < 18))/length(unique(dflog$TimeInput[which(dflog$User == inputPerson & dflog$HourInput >= 16 & dflog$HourInput < 18)])),
             length(which(dflog$User == inputPerson & dflog$HourInput >= 18 & dflog$HourInput < 20))/length(unique(dflog$TimeInput[which(dflog$User == inputPerson & dflog$HourInput >= 18 & dflog$HourInput < 20)])),
             length(which(dflog$User == inputPerson & dflog$HourInput >= 20 & dflog$HourInput < 22))/length(unique(dflog$TimeInput[which(dflog$User == inputPerson & dflog$HourInput >= 20 & dflog$HourInput < 22)])),
             length(which(dflog$User == inputPerson & dflog$HourInput >= 22 & dflog$HourInput < 0))/length(unique(dflog$TimeInput[which(dflog$User == inputPerson & dflog$HourInput >= 22)])))
  dfslot<-data.frame(slot,result)
  dfslot<-na.omit(dfslot)
  return(dfslot)
}

#######################################

# Cigarettes consumption per weekday
cigconsumption<-function(dflog,inputPerson){
  weekday<-c(1,2,3,4,5,6,7)
  cons<-c()
  for(d in weekday){
    cons<-c(cons,length(which(dflog$User == inputPerson  & (dflog$Type == "On time" | dflog$Type == "Cheated") & dflog$WDay == d)))
  }
  dfcons<-data.frame(weekday,cons)
  return(dfcons)
}

# Cigarettes consumption weekday
cigconsumptionwday<-function(dflog,inputPerson){
  return(length(which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated") & dflog$WDay < 6)))
}

# Cigarettes consumption weekend
cigconsumptionwend<-function(dflog,inputPerson){
  return(length(which(dflog$User == inputPerson & (dflog$Type == "On time" | dflog$Type == "Cheated") & dflog$WDay > 5)))
}

# Cigarettes consumption in last seven days
cigconsumption7<-function(dfstats,inputPerson){
  return(dfstats$nbcig[which(dfstats$User == inputPerson & dfstats$Week == max(dfstats$Week[which(dfstats$User == inputPerson)]))])
}

# Mean of cigarette consumption per weekday
meancons <- function(dflog, inputPerson){
  weekday<-c(1,2,3,4,5,6,7)
  cons<-c()
  for(d in weekday){
    temp<-length(which(dflog$User == inputPerson  & (dflog$Type == "On time" | dflog$Type == "Cheated" | dflog$Type == "Behaviour") & dflog$WDay == d))/length(unique(dflog$TimeInput[which(dflog$User == inputPerson  & (dflog$Type == "On time" | dflog$Type == "Cheated") & dflog$WDay == d)]))
    cons<-c(cons,temp)
  }
  dfcons<-data.frame(weekday,cons)
  return(dfcons)
}

# Std of cigarette consumption per weekday
stdcons <- function(dflog, inputPerson){
  weekday<-c(1,2,3,4,5,6,7)
  cons<-c()
  for(d in weekday){
    temp<-c()
    for(date in unique(dflog$TimeInput[which(dflog$User == inputPerson  & (dflog$Type == "On time" | dflog$Type == "Cheated" | dflog$Type == "Behaviour") & dflog$WDay == d)])){
      temp<-c(temp,length(which(dflog$User == inputPerson  & (dflog$Type == "On time" | dflog$Type == "Cheated" | dflog$Type == "Behaviour") & dflog$WDay == d & dflog$TimeInput == date)))
    }
    cons<-c(cons,sd(temp))
  }
  dfcons<-data.frame(weekday,cons)
  return(dfcons)
}

###################################################################
#Cigarette per weekday per time slot

cigperwday <- function(dflog,inputPerson,weekday){
  slot<-c("OH - 2H","2H - 4H","4H - 6H","6H - 8H","8H - 10H","10H - 12H","12H - 14H","14H - 16H","16H - 18H","18H - 20H","20H - 22H","22H - 0H")
  dfWeek <- dflog[which(dflog$User == inputPerson & (dflog$Type == "Cheated" | dflog$Type == "On time" | dflog$Type == "Behaviour")),]
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
             length(dfWeek$User[which(dfWeek$WDay== weekday & dfWeek$HourInput >= 22 & dfWeek$HourInput <= 23)]))
  dfslot<-data.frame(slot,result)
  dfslot<-na.omit(dfslot)
  return(dfslot)
}

avgcigperweek<-function(dflog, inputUser){
  df<-dflog[which(dflog$User == inputUser),c("User","Week")]
  df$temp<-1
  df<-df%>%
    group_by(User, Week) %>%
    summarise(sum=sum(temp))
  df$sum<-NULL
  df$nbcig<-0
  df<-nbcigarettes(dflog,df)
  avgcons<-c()
  week<-sort(unique(df$Week))
  for(w in week){
    avgcons<-c(avgcons,(df$nbcig[which(df$Week == w)])/7)
  }
  dfcons<-data.frame(week,avgcons)
  return(dfcons)
}

modeusageperweek<-function(dflog,inputPerson,inputMode){
  week<-sort(unique(dflog$Week[which(dflog$User==inputPerson)]))
  nbmode<-c()
  for(w in week){
    nbmode<-c(nbmode,length(which(dflog$User == inputPerson & dflog$Type == inputMode & dflog$Week == w)))
  }
  dfmode<-data.frame(week,nbmode)
  return(dfmode)
}

consperwday<-function(dflog, inputPerson, inputWeek){
  weekday<-c(1,2,3,4,5,6,7)
  cons<-c()
  if(inputWeek == "ALL"){
    df<-dflog
  }else{
    df<-dflog[which(dflog$Week == as.numeric(inputWeek)),]
  }
  for(d in weekday){
    cons<-c(cons,length(which(df$User == inputPerson  & (df$Type == "On time" | df$Type == "Cheated" | df$Type == "Behaviour") & df$WDay == d)))
  }
  dfcons<-data.frame(weekday,cons)
  return(dfcons)
}

######################################################################################
### 4 Engagement tab 
### Engagement over all period 

### Engagement per day
Engagement_perDay <- function(dflog, inputPerson){
  nb_days <- length(unique(dflog$TimeInput[which(dflog$User == inputPerson)]))
  #List of the days WARNING first element at indice 1
  all_days <- unique(dflog$TimeInput[which(dflog$User == inputPerson)])
  result <- c()
  
  for (i in 1:nb_days){
    dfWeek <- dflog[which(dflog$User == inputPerson & dflog$TimeInput== all_days[i]),]
    if(dfWeek$Week >=1){
      nb_autoskip <- length(dfWeek$Type[which(dfWeek$Type == "Auto skipped")])
      normalize <- length(dfWeek$Type[which(dfWeek$Type == "Auto skipped" | dfWeek$Type == "On time" | dfWeek$Type == "Skipped" | dfWeek$Type == "Snoozed")]) 
      
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
  Final_result<- data.frame("Day"= all_days, "Engagement"= result ) 
  return(Final_result)
}

### Engagement per week
EngagementPerWeek <- function(dfstats, inputPerson){
  df <- dfstats[which(dfstats$User == inputPerson),]
  Final_result<- data.frame("Week"= df$Week, "Engagement"= df$engagement) 
  return(Final_result)
}

################################################################
### 5 All days Tab
### Cigarettes consumption over all period 
Consumption_Over_All_Period <- function(dflog, inputPerson){
  nb_days <- length(unique(dflog$TimeInput[which(dflog$User == inputPerson)]))
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
  Final<-data.frame("Day"= all_days ,"Consumption"=result) 
  return(Final)
}

### Mode usage over all period
Mode_Usage <- function(dflog,inputPerson, inputMode){
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
  Final_result<- data.frame("Day"= all_days, "ModeNumber"=result) 
  return(Final_result)
}

#All Users
##################################################################################
### 1. Information Tab
### Total Number of saved cigarettes
total_number_of_cigarettes_saved <- function(dfstats){
  nb_users <- length(unique(dfstats$User))
  all_users <- unique(dfstats$User)
  sum_cig_saved <-0
  for (i in 1:nb_users){
    sum_cig_saved <- sum_cig_saved + CigarettesSaved(dfstats,all_users[i])
  }
  return(sum_cig_saved)
}

### Avg number of saved cigarettes
avg_nb_cig <- function (dfstats){
  nb_users <- length(unique(dfstats$User))
  total<-total_number_of_cigarettes_saved(dfstats)

  avg <- round(total/nb_users)
  return(avg)
}

#######################################################################
### Classic
### Mean and std of cigarette consumption per weekday
meanstdconsweekday<-function(dflog){
  name<-unique(dflog$User)
  wday<-c(1,2,3,4,5,6,7)
  day<-c()
  meancon<-c()
  stdcon<-c()
  for(wd in wday){
    day<-c(day,wd)
    val<-c()
    for(u in name){
      val<-c(val,length(which(dflog$User==u & dflog$WDay == wd & (dflog$Type == "Cheated" | dflog$Type == "On time" | dflog$Type == "Behaviour")))/length(unique(dflog$Week[which(dflog$User==u & dflog$WDay == wd & (dflog$Type == "Cheated" | dflog$Type == "On time" | dflog$Type == "Behaviour"))])))
    }
    val<-na.omit(val)
    meancon<-c(meancon,mean(val))
    stdcon<-c(stdcon,sd(val))
  }
  df<-data.frame(day,meancon,stdcon)
  return(df)
}

cigperwdayall <- function(dflog,weekday){
  slot<-c("OH - 2H","2H - 4H","4H - 6H","6H - 8H","8H - 10H","10H - 12H","12H - 14H","14H - 16H","16H - 18H","18H - 20H","20H - 22H","22H - 0H")
  dfWeek <- dflog[which((dflog$Type == "Cheated" | dflog$Type == "On time" | dflog$Type == "Behaviour")),]
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
             length(dfWeek$User[which(dfWeek$WDay== weekday & dfWeek$HourInput >= 22 & dfWeek$HourInput <= 23)]))
  dfslot<-data.frame(slot,result)
  dfslot<-na.omit(dfslot)
  return(dfslot)
}

### Progress all user
progressalluser<-function(dfstats){
  week<-sort(unique(dfstats$Week))
  prog<-c()
  for(w in week){
    temp<-sum(dfstats$progress[which(dfstats$Week == w)])/length(unique(dfstats$User[which(dfstats$Week == w)]))
    prog<-c(prog,temp)
  }
  dfprogall<-data.frame(week,prog)
  dfprogall<-na.omit(dfprogall)
  dfprogall$prog[which(dfprogall$prog < 0.001)]<-0
  return(dfprogall)
}

### Progress rate all user
progressratealluser<-function(dfprog,dfstats){
  week<-sort(unique(dfstats$Week))
  prograte<-c()
  temp<-0
  for(w in week){
    prog<-dfprog$prog[which(dfprog$week == w)]
    proglast<-dfprog$prog[which(dfprog$week == w-1)]
    if(length(proglast)>0){
      if(abs(proglast)>0){
        temp<-(prog-proglast)/abs(proglast)
      }else if(length(dfprog$prog[which(dfprog$week == w-2)])>0){
        if(abs(dfprog$prog[which(dfprog$week == w-2)])>0){
          proglast<-dfprog$prog[which(dfprog$week == w-2)]
          temp<-(prog-proglast)/abs(proglast)
        }
      }
    }
    prograte<-c(prograte,temp)
  }
  dfprograte<-data.frame(week,prograte)
  dfprograte<-na.omit(dfprograte)
  return(dfprograte)
}

#######################################################################
#Engagement Overall

engagementoverall<-function(dfstats){
  week<-sort(unique(dfstats$Week))
  eng<-c()
  for(w in week){
    eng<-c(eng,mean(dfstats$engagement[which(dfstats$Week == w)]))
  }
  dfengall<-data.frame(week,"engagement" = eng)
  dfengall<-na.omit(dfengall)
  return(dfengall)
}