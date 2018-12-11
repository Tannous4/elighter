server <- function(input, output, session){
  
  observe({
    req(input$file1)
    req(input$file2)
    tryCatch(
      {
        dfsurvey <<- read_excel(input$file2$datapath)
        dflog<<- read.csv(input$file1$datapath, header = input$header, sep=";")
        #PRE-PROCESSING DATA
        dflog$User<<-gsub('\216',"e", dflog$User)
        dflog$User<<-gsub('\221',"e", dflog$User)
        dflog$User<<-gsub('\203',"E", dflog$User)
        dflog$User<<-gsub('\217',"??", dflog$User)
        #PRE-PROCESSING DATA
        dflog<<-dflog[! (is.na(dflog$Latitude) & is.na(dflog$Longitude)), ]
        dflog$TimeInput <<-as.Date(dflog$Time, format('%d/%m/%Y %H:%M'))
        dflog$TimeFormat <<-dmy_hm(dflog$Time)
        dflog$HourInput <<-hour(dflog$TimeFormat)
        dflog$MinuteInput <<- minute(dflog$TimeFormat)
        dflog$WDay <<- wday(dflog$TimeInput)
        dflog$PartDay <<- "morning"
        dflog$PartDay[which(dflog$HourInput >= 9 & dflog$HourInput < 12)] <<- "work"
        dflog$PartDay[which(dflog$HourInput >= 14 & dflog$HourInput < 18)] <<- "work"
        dflog$PartDay[which(dflog$HourInput >= 12 & dflog$HourInput < 14)] <<- "lunch"
        dflog$PartDay[which(dflog$HourInput >= 18 & dflog$HourInput <= 23)] <<- "home"
        dflog$PartDay[which(dflog$HourInput >= 0 & dflog$HourInput < 5)] <<- "night"
        dflog$Week <<- (-1)
        for(name in unique(dflog$User)){
          Date<-dflog$TimeInput[order(dflog$TimeInput) & dflog$User == name][1]
          dflog$Week[which(dflog$User == name)]<<- trunc(difftime(dflog$TimeInput[which(dflog$User == name)],dflog$TimeInput[which(dflog$User == name & dflog$TimeInput == Date)],units = "week"),0)
        }
        
        #We only select the logs of people who have responded to the survey 
        dfAll <<- merge(dflog, dfsurvey, by.x = "User", by.y = "Name")
        head(dfAll)
        
        # dfstats
        dfstats<<-dflog[,c("User","Week")]
        dfstats$temp<<-1
        dfstats<<-dfstats%>%
          group_by(User, Week) %>%
          summarise(sum=sum(temp))
        dfstats$sum<<-NULL
        dfstats$engagement<<-1
        dfstats$nbcig<<-0
        dfstats$progress<<-0
        dfstats$progressrate<<-0
        
        # source("C:/Users/Christopher/Desktop/ING5/data analytics/project_part_1/fonction.r")
        
        for(user in dfstats$User){
          dfstats<<-Engagement_perWeek(dfstats,user)
        }
        dfstats<<-nbcigarettes(dflog,dfstats)
        dfstats<<-checkengagement(dfstats)
        dfstats<<-progressfunction(dfstats)
        dfstats<<-progressratefunction(dfstats)
        
        updateSelectInput(session, "userChoice", choices = dfAll$User)
        updateSelectInput(session, "weekChoice", choices = sort(unique(dfstats$Week)))
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )})
  
  #ALL INFO FOR SINGLE USER ANALYSIS
  #General infos 
  output$UserName <- renderText({
    req(input$file1)
    req(input$file2)
    paste("", unique(input$userChoice))
  })
  
  output$age<- renderText({
    req(input$file1)
    req(input$file2)
    paste("Age:", unique(dfAll$Age[which(dfAll$User==input$userChoice)]))
  }) 
  
  output$familystatus<- renderText({
    req(input$file1)
    req(input$file2)
    paste("Family Status:", unique(dfAll$`Family status`[which(dfAll$User==input$userChoice)]))
  })
  
  output$consuption<- renderText({
    req(input$file1)
    req(input$file2)
    paste("Cigarette consumption per day:", unique(dfAll$`How many cigarettes do you smoke per day`[which(dfAll$User==input$userChoice)]))
  })
  
  output$health<- renderText({
    req(input$file1)
    req(input$file2)
    index<-which(dfAll$User==input$userChoice)
    if(length(index)==0){
      health<-"no information"
    }else{
      
      health<-" "
      if (!is.na(dfAll$health.condition.diabetes[which(dfAll$User==input$userChoice)])){
        health<-paste( health, unique(dfAll$health.condition.diabetes[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if (!is.na(dfAll$health.condition.Gastrointestinal.Reflux[which(dfAll$User==input$userChoice)])){
        health<-paste( health, unique(dfAll$health.condition.Gastrointestinal.Reflux[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if (!is.na(dfAll$`health.condition.Heart Problems`[which(dfAll$User==input$userChoice)])){
        health<-paste( health, unique(dfAll$`health.condition.Heart Problems`[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if (!is.na(dfAll$health.condition.high.blood.pressure[which(dfAll$User==input$userChoice)]) ){
        health<-paste( health, unique(dfAll$health.condition.high.blood.pressure[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if (!is.na(dfAll$health.condition.overweight[which(dfAll$User==input$userChoice)])){
        health<-paste( health, unique(dfAll$health.condition.overweight[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if (!is.na(dfAll$health.condition.psychological.or.psychiatric.conditions[which(dfAll$User==input$userChoice)])){
        health<-paste( health, unique(dfAll$health.condition.psychological.or.psychiatric.conditions[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if (!is.na(dfAll$health.condition.other[which(dfAll$User==input$userChoice)])){
        health<-paste( health, unique(dfAll$health.condition.other[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if(health==" "){
        health<- "nothing"
      }
      
    }
    paste("Health conditions:", health )
    
  })
  
  output$technicsQuit<- renderText({
    req(input$file1)
    req(input$file2)
    
    index<-which(dfAll$User==input$userChoice)
    if(length(index)==0){
      technics<-"no information"
    }else{
      technics=" "
      if (!is.na(dfAll$method.quit.smoking.acupuncture[which(dfAll$User==input$userChoice)])){
        technics=paste(technics, unique(dfAll$method.quit.smoking.acupuncture[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if (!is.na(dfAll$method.quit.smoking.cessation.program[which(dfAll$User==input$userChoice)])){
        technics=paste(technics, unique(dfAll$method.quit.smoking.cessation.program[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if (!is.na(dfAll$method.quit.smoking.changing.habits[which(dfAll$User==input$userChoice)])){
        technics=paste(technics, unique(dfAll$method.quit.smoking.changing.habits[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if (!is.na(dfAll$method.quit.smoking.cold.turkey[which(dfAll$User==input$userChoice)])){
        technics=paste(technics, unique(dfAll$method.quit.smoking.cold.turkey[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if (!is.na(dfAll$`method.quit.smoking.e-cigarettes`[which(dfAll$User==input$userChoice)])){
        technics=paste(technics, unique(dfAll$`method.quit.smoking.e-cigarettes`[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if (!is.na(dfAll$method.quit.smoking.exercise[which(dfAll$User==input$userChoice)])){
        technics=paste(technics, unique(dfAll$method.quit.smoking.exercise[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if (!is.na(dfAll$method.quit.smoking.medicine.based[which(dfAll$User==input$userChoice)])){
        technics=paste(technics, unique(dfAll$method.quit.smoking.medicine.based[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if (!is.na(dfAll$method.quit.smoking.nicotine.gum[which(dfAll$User==input$userChoice)])){
        technics=paste(technics, unique(dfAll$method.quit.smoking.nicotine.gum[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if (!is.na(dfAll$method.quit.smoking.nicotine.patch[which(dfAll$User==input$userChoice)])){
        technics=paste(technics, unique(dfAll$method.quit.smoking.nicotine.patch[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if (!is.na(dfAll$method.quit.smoking.nicotine.patch[which(dfAll$User==input$userChoice)])){
        technics=paste(technics, unique(dfAll$method.quit.smoking.nicotine.patch[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if (!is.na(dfAll$method.quit.smoking.willpower[which(dfAll$User==input$userChoice)])){
        technics=paste(technics, unique(dfAll$method.quit.smoking.willpower[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if (!is.na(dfAll$method.quit.smoking.other[which(dfAll$User==input$userChoice)])){
        technics=paste(technics, unique(dfAll$method.quit.smoking.other[which(dfAll$User==input$userChoice)]), seq=',')
      }
      if (!is.na(dfAll$method.quit.smoking.nothing[which(dfAll$User==input$userChoice)])){
        technics=paste(technics, unique(dfAll$method.quit.smoking.nothing[which(dfAll$User==input$userChoice)]), seq=',')
      }
    }
    
    paste("Quitting technics:", technics)
  })
  
  output$statModeUserPlot <- renderPlot({
    req(input$file1)
    req(input$file2)
    if (input$hourRange=="Indifferent"){
      # Render a barplot
      p<-ggplot(data=dflog[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2]),], aes(x=factor(Type))) +
        geom_bar(stat="count", width=0.7, fill="steelblue") +
        theme(text = element_text(size=15),
              axis.text.x = element_text(angle=45, hjust=1))
      
      p
    } else if (input$hourRange=="5 am to 9 am"){
      # Render a barplot
      p<-ggplot(data=dflog[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$HourInput >= 5 & dflog$HourInput < 9),], aes(x=factor(Type))) +
        geom_bar(stat="count", width=0.7, fill="steelblue") +
        theme(text = element_text(size=15),
              axis.text.x = element_text(angle=45, hjust=1))
      
      p
      
    }else if  (input$hourRange=="9 am to 12 am"){
      # Render a barplot
      p<-ggplot(data=dflog[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$HourInput >= 9 & dflog$HourInput < 12),], aes(x=factor(Type))) +
        geom_bar(stat="count", width=0.7, fill="steelblue") +
        theme(text = element_text(size=15),
              axis.text.x = element_text(angle=45, hjust=1))
      
      p
      
    }else if  (input$hourRange=="12 am to 2 pm"){
      # Render a barplot
      p<-ggplot(data=dflog[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$HourInput >= 12 & dflog$HourInput < 14),], aes(x=factor(Type))) +
        geom_bar(stat="count", width=0.7, fill="steelblue") +
        theme(text = element_text(size=15),
              axis.text.x = element_text(angle=45, hjust=1))
      
      p
      
    }else if  (input$hourRange=="2 pm to 6 pm"){
      # Render a barplot
      p<-ggplot(data=dflog[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$HourInput >= 14 & dflog$HourInput < 18),], aes(x=factor(Type))) +
        geom_bar(stat="count", width=0.7, fill="steelblue") +
        theme(text = element_text(size=15),
              axis.text.x = element_text(angle=45, hjust=1))
      
      p
      
    }else if  (input$hourRange=="6 pm to 12 pm"){
      # Render a barplot
      p<-ggplot(data=dflog[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$HourInput >=18 & dflog$HourInput <= 23),], aes(x=factor(Type))) +
        geom_bar(stat="count", width=0.7, fill="steelblue") +
        theme(text = element_text(size=15),
              axis.text.x = element_text(angle=45, hjust=1))
      
      p
      
    }else if  (input$hourRange=="12 pm to 5 am"){
      
      # Render a barplot
      p<-ggplot(data=dflog[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$HourInput >= 0 & dflog$HourInput < 5),], aes(x=factor(Type))) +
        geom_bar(stat="count", width=0.7, fill="steelblue") +
        theme(text = element_text(size=15),
              axis.text.x = element_text(angle=45, hjust=1))
      
      p
      
    }
    
  })
  
  #Fill statistics about number of cigarettes smoked
  output$NbMorningCigarettes <- renderInfoBox({
    req(input$file1)
    req(input$file2)
    if (input$hourRange=="Indifferent"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2])] 
    } else if (input$hourRange=="5 am to 9 am"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="morning")] 
    }else if  (input$hourRange=="9 am to 12 am"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="work" & dflog$HourInput >= 9 & dflog$HourInput < 12)] 
    }else if  (input$hourRange=="12 am to 2 pm"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="lunch")] 
    }else if  (input$hourRange=="2 pm to 6 pm"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="work" & dflog$HourInput >= 14 & dflog$HourInput < 18)] 
    }else if  (input$hourRange=="6 pm to 12 pm"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="home")] 
    }else if  (input$hourRange=="12 pm to 5 am"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="night")] 
    }
    
    infoBox(
      "Morning cigarettes", sum(v=="morning"), icon = icon("coffee"),
      color = "aqua", fill = TRUE
    )
  })
  
  output$NbLunchCigarettes <- renderInfoBox({
    req(input$file1)
    req(input$file2)
    if (input$hourRange=="Indifferent"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2])] 
    } else if (input$hourRange=="5 am to 9 am"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="morning")] 
    }else if  (input$hourRange=="9 am to 12 am"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="work" & dflog$HourInput >= 9 & dflog$HourInput < 12)] 
    }else if  (input$hourRange=="12 am to 2 pm"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="lunch")] 
    }else if  (input$hourRange=="2 pm to 6 pm"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="work" & dflog$HourInput >= 14 & dflog$HourInput < 18)] 
    }else if  (input$hourRange=="6 pm to 12 pm"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="home")] 
    }else if  (input$hourRange=="12 pm to 5 am"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="night")] 
    }
    infoBox(
      "Lunch cigarettes", sum(v=="lunch"), icon = icon("coffee"),
      color = "yellow", fill = TRUE
    )
  })
  
  output$NbNightCigarettes <- renderInfoBox({
    req(input$file1)
    req(input$file2)
    if (input$hourRange=="Indifferent"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2])] 
    } else if (input$hourRange=="5 am to 9 am"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="morning")] 
    }else if  (input$hourRange=="9 am to 12 am"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="work" & dflog$HourInput >= 9 & dflog$HourInput < 12)] 
    }else if  (input$hourRange=="12 am to 2 pm"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="lunch")] 
    }else if  (input$hourRange=="2 pm to 6 pm"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="work" & dflog$HourInput >= 14 & dflog$HourInput < 18)] 
    }else if  (input$hourRange=="6 pm to 12 pm"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="home")] 
    }else if  (input$hourRange=="12 pm to 5 am"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="night")] 
    }
    infoBox(
      "Night cigarettes", sum(v=="night"), icon = icon("star"),
      color = "navy", fill = TRUE
    )
  })
  
  output$NbWorkCigarettes <- renderInfoBox({
    req(input$file1)
    req(input$file2)
    if (input$hourRange=="Indifferent"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2])] 
    } else if (input$hourRange=="5 am to 9 am"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="morning")] 
    }else if  (input$hourRange=="9 am to 12 am"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="work" & dflog$HourInput >= 9 & dflog$HourInput < 12)] 
    }else if  (input$hourRange=="12 am to 2 pm"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="lunch")] 
    }else if  (input$hourRange=="2 pm to 6 pm"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="work" & dflog$HourInput >= 14 & dflog$HourInput < 18)] 
    }else if  (input$hourRange=="6 pm to 12 pm"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="home")] 
    }else if  (input$hourRange=="12 pm to 5 am"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="night")] 
    }
    infoBox(
      "Work cigarettes", sum(v=="work"), icon = icon("briefcase"),
      color = "purple", fill = TRUE
    )
  })
  
  output$NbHomeCigarettes <- renderInfoBox({
    req(input$file1)
    req(input$file2)
    if (input$hourRange=="Indifferent"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2])] 
    } else if (input$hourRange=="5 am to 9 am"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="morning")] 
    }else if  (input$hourRange=="9 am to 12 am"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="work" & dflog$HourInput >= 9 & dflog$HourInput < 12)] 
    }else if  (input$hourRange=="12 am to 2 pm"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="lunch")] 
    }else if  (input$hourRange=="2 pm to 6 pm"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="work" & dflog$HourInput >= 14 & dflog$HourInput < 18)] 
    }else if  (input$hourRange=="6 pm to 12 pm"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="home")] 
    }else if  (input$hourRange=="12 pm to 5 am"){
      v<-dflog$PartDay[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="night")] 
    }
    infoBox(
      "Home cigarettes", sum(v=="home") , icon = icon("home"),
      color = "olive", fill = TRUE
    )
  })
  
  #ALL INFO FOR ALL USERS ANALYSIS
  output$GenderRepartition <- renderPlot({
    req(input$file1)
    req(input$file2)
    p<-ggplot(data = dfsurvey, aes(x=factor(""), fill=Gender)) +
      geom_bar()+
      geom_text(stat='count',aes(label=..count..), size=(5),position = position_stack(vjust = 0.5))+
      coord_polar(theta = "y") +
      scale_x_discrete("") +
      scale_fill_manual(values = c("tomato","steelblue","olivedrab1"))+
      theme_bw() +
      ggtitle("Gender repartition of the survey") +
      theme(panel.border = element_blank(), 
            plot.title = element_text(face = "bold", size = (12)), 
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom")
    p
  })
  
  output$GenderAge <- renderPlot({
    req(input$file1)
    req(input$file2)
    subData<- subset(dfsurvey, select=c("Gender", "Age"))
    y<-tapply(subData$Age, subData$Gender, mean)
    p <- ggplot() +
      geom_boxplot(data = subData[subData$Gender=="Male",], aes(x=factor(Gender), y=Age), fill="steelblue") +
      geom_boxplot(data = subData[subData$Gender=="Female",], aes(x=factor(Gender), y=Age), fill="tomato") +
      ggtitle("Age repartition by gender") +
      xlab("Gender") +
      theme(plot.title = element_text(face = "bold", size = (12)))
    p
  })
  
  output$GenderMoney <- renderPlot({
    req(input$file1)
    req(input$file2)
    subData<-subset(dfsurvey, select=c("Gender", "main.motivator.money"))
    subData<-na.omit(subData)
    p<-ggplot(data = subData, aes(x=factor(""), fill=Gender)) +
      geom_bar()+
      geom_text(stat='count',aes(label=..count..), size=(5),position = position_stack(vjust = 0.5))+
      coord_polar(theta = "y") +
      scale_x_discrete("") +
      scale_fill_manual(values = c("steelblue","tomato","olivedrab1"))+
      theme_bw() +
      ggtitle("Gender repartition (Main motivator Money)") +
      theme(panel.border = element_blank(), 
            plot.title = element_text(face = "bold", size = (12)), 
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom")
    p
  })
  
  output$GenderWeight <- renderPlot({
    req(input$file1)
    req(input$file2)
    p<-ggplot(data = dfsurvey[which(dfsurvey$`Are you experiencing weight management issues?`=="I am severely overweight / underweight"),], aes(x=factor(""), fill=Gender)) +
      geom_bar()+
      geom_text(stat='count',aes(label=..count..), size=(5),position = position_stack(vjust = 0.5))+
      coord_polar(theta = "y") +
      scale_x_discrete("") +
      scale_fill_manual(values = c("steelblue","tomato","olivedrab1"))+
      theme_bw() +
      ggtitle("Gender repartition (severly over/underweight)") +
      theme(panel.border = element_blank(), 
            plot.title = element_text(face = "bold", size = (12)), 
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom")
    p
  })
  
  output$EducationRepartition <- renderPlot({
    req(input$file1)
    req(input$file2)
    subData<-subset(dfsurvey, select=c("Education"))
    subData$Education[startsWith(subData$Education,"Graduate")]<-"Graduate degree"
    subData$Education[startsWith(subData$Education,"Undergraduate")]<-"Undergraduate degree"
    subData$Education[startsWith(subData$Education,"High")]<-"HighSchool/vocational training"
    p<-ggplot(data = subData, aes(x=factor(""), fill=Education)) +
      geom_bar()+
      geom_text(stat='count',aes(label=..count..), size=(5),position = position_stack(vjust = 0.5))+
      coord_polar(theta = "y") +
      scale_x_discrete("") +
      scale_fill_manual(values = c("tomato","steelblue","chartreuse3"))+
      theme_bw() +
      ggtitle("Education repartition") +
      theme(panel.border = element_blank(), 
            plot.title = element_text(face = "bold", size = (12)), 
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom")
    p
  })
  
  
  output$EducationBrand <- renderPlot({
    req(input$file1)
    req(input$file2)
    subData<-dfsurvey
    subData$Education[startsWith(subData$Education,"Graduate")]<-"Graduate degree"
    subData$Education[startsWith(subData$Education,"Undergraduate")]<-"Undergraduate degree"
    subData$Education[startsWith(subData$Education,"High")]<-"HighSchool/vocational training"
    p<-ggplot(data = subData[which(dfsurvey$`What is the brand of your cigarettes?`=="Cedars"),], aes(x=factor(""), fill=Education)) +
      geom_bar()+
      geom_text(stat='count',aes(label=..count..), size=(5),position = position_stack(vjust = 0.5))+
      coord_polar(theta = "y") +
      scale_x_discrete("") +
      scale_fill_manual(values = c("chartreuse3","steelblue","tomato"))+
      theme_bw() +
      ggtitle("Education repartition (Cigarettes Brand Cedars)") +
      theme(panel.border = element_blank(), 
            plot.title = element_text(face = "bold", size = (12)), 
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom")
    p
  })
  
  output$EducationMotivation <- renderPlot({
    req(input$file1)
    req(input$file2)
    subData<-dfsurvey
    subData$Education[startsWith(subData$Education,"Graduate")]<-"Graduate degree"
    subData$Education[startsWith(subData$Education,"Undergraduate")]<-"Undergraduate degree"
    subData$Education[startsWith(subData$Education,"High")]<-"HighSchool/vocational training"
    p<-ggplot(data = subData[which(dfsurvey$`How motivated are you to change your smoking patterns (quit or reduce smoking)?`=="Motivated but I am afraid of failing"),], aes(x=factor(""), fill=Education)) +
      geom_bar()+
      geom_text(stat='count',aes(label=..count..), size=(5),position = position_stack(vjust = 0.5))+
      coord_polar(theta = "y") +
      scale_x_discrete("") +
      scale_fill_manual(values = c("steelblue","chartreuse3","tomato"))+
      theme_bw() +
      ggtitle("Education repartition (Motivated to reduce smoking but afraid of failing)") +
      theme(panel.border = element_blank(), 
            plot.title = element_text(face = "bold", size = (12)), 
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom")
    p
  })
  
  output$EducationNb <- renderPlot({
    req(input$file1)
    req(input$file2)
    subData<-dfsurvey
    subData$Education[startsWith(subData$Education,"Graduate")]<-"Graduate degree"
    subData$Education[startsWith(subData$Education,"Undergraduate")]<-"Undergraduate degree"
    subData$Education[startsWith(subData$Education,"High")]<-"HighSchool/vocational training"
    p<-ggplot(data = subData[which(dfsurvey$`How many cigarettes do you smoke per day`=="31 or more"),], aes(x=factor(""), fill=Education)) +
      geom_bar()+
      geom_text(stat='count',aes(label=..count..), size=(5),position = position_stack(vjust = 0.5))+
      coord_polar(theta = "y") +
      scale_x_discrete("") +
      scale_fill_manual(values = c("tomato","chartreuse3","steelblue"))+
      theme_bw() +
      ggtitle("Education repartition (31 or more cigarettes per day)") +
      theme(panel.border = element_blank(), 
            plot.title = element_text(face = "bold", size = (12)), 
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom")
    p
  })
  
  output$FsRepartition <- renderPlot({
    req(input$file1)
    req(input$file2)
    p<-ggplot(data = dfsurvey, aes(x=factor(""), fill=`Family status`)) +
      geom_bar()+
      geom_text(stat='count',aes(label=..count..), size=(5),position = position_stack(vjust = 0.5))+
      coord_polar(theta = "y") +
      scale_x_discrete("") +
      scale_fill_manual(values = c("tomato","steelblue","olivedrab1"))+
      theme_bw() +
      ggtitle("Family status repartition") +
      theme(panel.border = element_blank(), 
            plot.title = element_text(face = "bold", size = (12)), 
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom")
    p
  })
  
  output$FsFamily <- renderPlot({
    req(input$file1)
    req(input$file2)
    p<-ggplot(data = dfsurvey[which(dfsurvey$main.motivator.family=="Family"),], aes(x=factor(""), fill=`Family status`)) +
      geom_bar()+
      geom_text(stat='count',aes(label=..count..), size=(5),position = position_stack(vjust = 0.5))+
      coord_polar(theta = "y") +
      scale_x_discrete("") +
      scale_fill_manual(values = c("tomato","steelblue","olivedrab1"))+
      theme_bw() +
      ggtitle("Family status repartition (Main motivator Family)") +
      theme(panel.border = element_blank(), 
            plot.title = element_text(face = "bold", size = (12)), 
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom")
    p
  })
  
  output$FsHealth <- renderPlot({
    req(input$file1)
    req(input$file2)
    p<-ggplot(data = dfsurvey[which(dfsurvey$health.condition.high.blood.pressure=="High Blood Pressure"),], aes(x=factor(""), fill=`Family status`)) +
      geom_bar()+
      geom_text(stat='count',aes(label=..count..), size=(5),position = position_stack(vjust = 0.5))+
      coord_polar(theta = "y") +
      scale_x_discrete("") +
      scale_fill_manual(values = c("tomato","steelblue","olivedrab1"))+
      theme_bw() +
      ggtitle("Family status repartition (High blood pressure)") +
      theme(panel.border = element_blank(), 
            plot.title = element_text(face = "bold", size = (12)), 
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom")
    p
  })
  
  output$mymap <- renderLeaflet({
    req(input$file1)
    req(input$file2)
    if (input$hourRange=="Indifferent"){
      coord<-dflog[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2]),c("Latitude","Longitude") ]
    } else if (input$hourRange=="5 am to 9 am"){
      coord<-dflog[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="morning"),c("Latitude","Longitude")] 
    }else if  (input$hourRange=="9 am to 12 am"){
      coord<-dflog[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="work" & dflog$HourInput >= 9 & dflog$HourInput < 12),c("Latitude","Longitude")] 
    }else if  (input$hourRange=="12 am to 2 pm"){
      coord<-dflog[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="lunch"),c("Latitude","Longitude")] 
    }else if  (input$hourRange=="2 pm to 6 pm"){
      coord<-dflog[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="work" & dflog$HourInput >= 14 & dflog$HourInput < 18),c("Latitude","Longitude")] 
    }else if  (input$hourRange=="6 pm to 12 pm"){
      coord<-dflog[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="home"),c("Latitude","Longitude")] 
    }else if  (input$hourRange=="12 pm to 5 am"){
      coord<-dflog[which(dflog$User==input$userChoice & dflog$TimeInput >= input$dateRange[1] & dflog$TimeInput < input$dateRange[2] & dflog$PartDay=="night"),c("Latitude","Longitude")] 
    }
    
    #coord<-v[v$User==input$userChoice,c("Latitude","Longitude")]
    coord<-coord[! (is.na(coord$Latitude) & is.na(coord$Longitude)), ]
    
    coord1<-aggregate(x=coord$Latitude, by=list(coord$Longitude,coord$Latitude), FUN=length)
    head(coord1)
    
    longCenter<- mean(coord$Longitude)
    latCenter<- mean(coord$Latitude)
    
    points <- data.frame(longitudes = coord1$Group.1,
                         latitudes = coord1$Group.2,
                         labels = coord1$x)
    
    m <-leaflet(points) %>%
      addTiles() %>%
      setView(lng = longCenter, lat = latCenter, zoom = 8)  %>%
      addMarkers(lng = ~longitudes, lat = ~latitudes, popup = paste("nb cigarettes", coord1$x))
    m
  })
}