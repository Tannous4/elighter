server <- function(input, output, session){
  
  observe({
    req(input$file1)
    req(input$file2)
    tryCatch(
      {
        dfsurvey <<- read_excel(input$file2$datapath)
        dflog<<- read.csv(input$file1$datapath, header = TRUE, sep=";")
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
        updateSelectInput(session, "suadmodeChoice", choices = unique(dflog$Type))
        
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
  
  ###############################################################
  ## Info Tab Single User
  
  output$suname <- renderValueBox({
    req(input$file1)
    req(input$file2)
    valueBox(
      dfsurvey$Name[which(dfsurvey$Name == input$userChoice)], "Name", icon = icon("user"),
      color = "purple"
    )
  })
  
  output$suage <- renderValueBox({
    req(input$file1)
    req(input$file2)
    valueBox(
      dfsurvey$Age[which(dfsurvey$Name == input$userChoice)], "Age", icon = icon("user"),
      color = "purple"
    )
  })
  
  output$suagecgy <- renderValueBox({
    req(input$file1)
    req(input$file2)
    age<-dfsurvey$Age[which(dfsurvey$Name == input$userChoice)]
    cgy<-"Middle"
    if(length(age)>0){
      if(age<30){
        cgy<-"Young"
      }else if(age>=50){
        cgy<-"Old"
      }
    }
    valueBox(
      cgy, "Age Category", icon = icon("user"),
      color = "purple"
    )
  })
  
  output$sumoneysaved <- renderValueBox({
    req(input$file1)
    req(input$file2)
    valueBox(
      paste0(CigarettesSaved(dfstats,input$userChoice),"$"), "Money Saved", icon = icon("credit-card"),
      color = "olive", width = 6
    )
  })
  
  output$sucigsaved <- renderValueBox({
    req(input$file1)
    req(input$file2)
    valueBox(
      CigarettesSaved(dfstats,input$userChoice), "Cigarettes Saved", icon = icon("save"),
      color = "olive", width = 1/2
    )
  })
  
  output$suoverallprog <- renderValueBox({
    req(input$file1)
    req(input$file2)
    valueBox(
      round(overallprogress(dfstats, input$userChoice),2), "Overall Progress", icon = icon("list-ul"),
      color = "navy", width = 3
    )
  })
  
  output$suoverallprogcgy <- renderValueBox({
    req(input$file1)
    req(input$file2)
    valueBox(
      overallprogresscgy(dfstats, input$userChoice), "Overall Progress Category", icon = icon("list-ul"),
      color = "navy", width = 3
    )
  })
  
  output$suoverallengagement <- renderValueBox({
    req(input$file1)
    req(input$file2)
    valueBox(
      round(overallengagement(dfstats, input$userChoice),2), "Overall Engagement", icon = icon("list-ul"),
      color = "navy", width = 3
    )
  })
  
  output$subestprograte <- renderValueBox({
    req(input$file1)
    req(input$file2)
    valueBox(
      round(bestprogressrate(dfstats, input$userChoice),2), "Best Progress Rate", icon = icon("list-ul"),
      color = "navy", width = 3
    )
  })
  
  output$sumeanconscig <- renderValueBox({
    req(input$file1)
    req(input$file2)
    valueBox(
      round(meanperday(dflog, input$userChoice),2), "Mean Per Day", icon = icon("weight"),
      color = "teal", width = 3
    )
  })
  
  output$sumeanconscigwday <- renderValueBox({
    req(input$file1)
    req(input$file2)
    valueBox(
      round(meanweekday(dflog, input$userChoice),2), "Mean Per Week Day", icon = icon("weight"),
      color = "teal", width = 3
    )
  })
  
  output$sumeanconscigwend <- renderValueBox({
    req(input$file1)
    req(input$file2)
    valueBox(
      round(meanweekend(dflog, input$userChoice),2), "Mean Per Week End", icon = icon("weight"),
      color = "teal", width = 3
    )
  })
  
  output$sumeanconscigslot <- renderValueBox({
    req(input$file1)
    req(input$file2)
    dfslot<-mostsmokingslot(dflog, input$userChoice)
    valueBox(
      dfslot$slot[which(dfslot$result == max(dfslot$result))], "Max Smoking Slot", icon = icon("arrow-alt-circle-up"),
      color = "yellow", width = 3
    )
  })
  
  output$sumeanconscigslotval <- renderValueBox({
    req(input$file1)
    req(input$file2)
    dfslot<-mostsmokingslot(dflog, input$userChoice)
    valueBox(
      round(dfslot$result[which(dfslot$result == max(dfslot$result))],2), paste0("Avg Cigarettes Max Smoking Slot: ",dfslot$slot[which(dfslot$result == max(dfslot$result))]), icon = icon("arrow-alt-circle-up"),
      color = "yellow", width = 3
    )
  })
  
  ####################################################################
  ## Single User Classic Tab
  
  output$suclcigcons <- renderPlotly({
    req(input$file1)
    req(input$file2)
    df<-cigconsumption(dflog,input$userChoice)
    p<-plot_ly(df,
      x=~weekday,
      y=~cons,
      name="consumption",
      type="bar"
    )
    p
  })
  output$suclcigconsrep<-renderPlotly({
    req(input$file1)
    req(input$file2)
    days<-c("weekdays","weekends")
    cons<-c(cigconsumptionwday(dflog,input$userChoice),cigconsumptionwend(dflog,input$userChoice))
    p <- plot_ly(labels = days, values = cons, type = 'pie') %>%
      layout(
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p
  })
  
  output$suclcigcons7 <- renderValueBox({
    req(input$file1)
    req(input$file2)
    valueBox(
      cigconsumption7(dfstats,input$userChoice), "Consumption last Week", icon = icon("user"),
      color = "blue"
    )
  })
  
  output$suclcigconsmean <- renderPlotly({
    req(input$file1)
    req(input$file2)
    df<-meancons(dflog,input$userChoice)
    p<-plot_ly(df,
               x=~weekday,
               y=~cons,
               name="consumption",
               type="bar"
    )
    p
  })
  
  output$suclcigconsstd <- renderPlotly({
    req(input$file1)
    req(input$file2)
    df<-stdcons(dflog,input$userChoice)
    p<-plot_ly(df,
               x=~weekday,
               y=~cons,
               name="consumption",
               type="bar"
    )
    p
  })
  
  output$suclprogress <- renderPlotly({
    req(input$file1)
    req(input$file2)
    df<-dfstats[which(dfstats$User == input$userChoice), ]
    p<-plot_ly(df,
               x=~Week,
               y=~progress,
               name="consumption",
               type="bar"
    )
    p
  })
  
  output$suclprogressrate <- renderPlotly({
    req(input$file1)
    req(input$file2)
    df<-dfstats[which(dfstats$User == input$userChoice), ]
    p<-plot_ly(df,
               x=~Week,
               y=~progressrate,
               name="consumption",
               type="bar"
    )
    p
  })
  
  ####################################################################
  ## Single User Week Tab
  
  
  ####################################################################
  ## Single User Engagement Tab
  output$suenperday <- renderPlotly({
    req(input$file1)
    req(input$file2)
    df<-Engagement_perDay(dflog,input$userChoice)
    p<-plot_ly(df,
               x=~Day,
               y=~Engagement,
               name="engagement",
               mode="lines+markers"
    )
    p
  })
  
  output$suenperweek <- renderPlotly({
    req(input$file1)
    req(input$file2)
    df<-dfstats[which(dfstats$User == input$userChoice), ]
    p<-plot_ly(df,
               x=~Week,
               y=~engagement,
               name="engagement",
               type="bar"
    )
    p
  })
  
  #######################################################################
  ## Single User All Day
  
  output$suadcigcons <- renderPlotly({
    req(input$file1)
    req(input$file2)
    df<-Consumption_Over_All_Period(dflog,input$userChoice)
    p<-plot_ly(df,
               x=~Day,
               y=~Consumption,
               name="consumption",
               mode="lines+markers"
    )
    p
  })
  
  output$suadmode <- renderPlotly({
    req(input$file1)
    req(input$file2)
    df<-Mode_Usage(dflog,input$userChoice,input$suadmodeChoice)
    p<-plot_ly(df,
               x=~Day,
               y=~ModeNumber,
               name="consumption",
               mode="lines+markers"
    )
    p
  })
  
  #######################################################################
  ## All User Info Tab
  
  output$auinmoney <- renderValueBox({
    req(input$file1)
    req(input$file2)
    valueBox(
      paste0(total_number_of_cigarettes_saved(dfstats),"$"), "Money Saved", icon = icon("credit-card"),
      color = "olive", width = 6
    )
  })
  
  output$auinmoneyavg <- renderValueBox({
    req(input$file1)
    req(input$file2)
    valueBox(
      paste0(round(avg_nb_cig(dfstats),2),"$"), "Average Money Saved", icon = icon("credit-card"),
      color = "olive", width = 6
    )
  })
  output$auincons <- renderValueBox({
    req(input$file1)
    req(input$file2)
    valueBox(
      total_number_of_cigarettes_saved(dfstats), "Cigarettes Saved", icon = icon("save"),
      color = "maroon", width = 6
    )
  })
  
  output$auinconsavg <- renderValueBox({
    req(input$file1)
    req(input$file2)
    valueBox(
      round(avg_nb_cig(dfstats),2), "Average Cigarettes Saved", icon = icon("save"),
      color = "maroon", width = 6
    )
  })
  
  #######################################################################
  ## All User Classic Tab
  
  
  #######################################################################
  ## All User Engagement Tab
  
  output$auen <- renderPlotly({
    req(input$file1)
    req(input$file2)
    df<-engagementoverall(dfstats)
    p<-plot_ly(df,
               x=~week,
               y=~engagement,
               name="engagement",
               type="bar"
    )
    p
  })

}


