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
      print(length(which(dflog$User == user & (dflog$Type == "On time" | dflog$Type == "Cheated" | dflog$Type == "Behaviour") & dflog$Week == week)))
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

progressratefunction <- function(dfstats){
  for(name in unique(dfstats$User)){
    for(week in dfstats$Week[which(dfstats$User == name)]){
      ProgressRate <- 0
      Progress<-dfstats$progress[which(dfstats$User == name & dfstats$Week == week)]
      if(abs(Progress) > 0){
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