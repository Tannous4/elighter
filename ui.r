ui<- dashboardPage(
  skin= "purple",
  dashboardHeader(title = "E-lighter Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("All users", tabName = "AllUsers", icon = icon("users")),
      menuItem("Single user", tabName = "SingleUser", icon = icon("user"))
    ),
    fileInput("file1", "Choose CSV File",
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    fileInput("file2", "Choose Excel File",
              accept = c(".xlsx")
              
    ),
    selectInput("userChoice", "Please choose a user: ", "")
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "AllUsers",
        tabsetPanel(
          tabPanel(title = "General",
            fluidRow(
              box(
                title = "Gender stats", status = "primary",solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("GenderRepartition"),
                plotOutput("GenderAge"),
                plotOutput("GenderMoney"),
                plotOutput("GenderWeight")
              ),
              box(
                title = "Education stats", status = "primary",solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("EducationRepartition"),
                plotOutput("EducationBrand"),
                plotOutput("EducationMotivation"),
                plotOutput("EducationNb")
              ),
              box(
                title = "Family Status stats", status = "primary",solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("FsRepartition"),
                plotOutput("FsFamily"),
                plotOutput("FsHealth")
              )
            )
          ),
          tabPanel(title="Info",
                   fluidPage(
                     fluidRow(
                       valueBoxOutput("auinmoney", width = 6),
                       valueBoxOutput("auinmoneyavg", width = 6)
                     ),
                     fluidRow(
                       valueBoxOutput("auincons", width = 6),
                       valueBoxOutput("auinconsavg", width = 6)
                     )
                   )
          ),
          tabPanel(title="Classic",
                   fluidRow(box(title="mean & std consumption per weekday", status = "danger", solidHeader = TRUE, width = 12,
                                plotlyOutput("auclmeanstd"))),
                   fluidRow(
                     box(title = "Cigarette consumption per slot per weekday",status = "success", solidHeader = TRUE, width = 9,
                         plotlyOutput("auclslot")),
                     box(title="Select the Day", background = "green", width = 3,
                         selectInput("auclselectwday", "Please choose a day: ", "")
                     )
                   ),
                   fluidRow(box(title=" Average Progress", status = "warning", solidHeader = TRUE, width = 12,
                                plotlyOutput("auclprogress"))),
                   fluidRow(box(title="Average Progress Rate", status = "info", solidHeader = TRUE, width = 12,
                                plotlyOutput("auclprogressrate")))

          ),
          tabPanel(title="Engagement",
                   fluidRow(box(title= "Average Engagement per week", status = "warning", solidHeader = TRUE, width = 12,
                                plotlyOutput("auen")
                                
                   ))
          )
        )
      ),
      
      # Second tab content
      tabItem(tabName = "SingleUser",
        tabsetPanel(
          tabPanel(title="General",
            fluidRow(
              tabBox(
                title=tagList(shiny::icon("user"), "User Settings"),
                id = "tabset1", height = "200px",
                tabPanel("Time",
                         dateRangeInput('dateRange',
                                        label = 'Date range input:',
                                        start = "2017-06-02", end = "2017-11-02"),
                         selectInput("hourRange", "Hour range inout: ", c("Indifferent", 
                                                                          "5 am to 9 am",
                                                                          "9 am to 12 am",
                                                                          "12 am to 2 pm",
                                                                          "2 pm to 6 pm",
                                                                          "6 pm to 12 pm",
                                                                          "12 pm to 5 am"))
                )
              ),
              box(
                title = textOutput("UserName"), background = "light-blue", solidHeader = TRUE,
                textOutput("age"),
                textOutput("familystatus"),
                textOutput("consuption"),
                textOutput("health"),
                textOutput("technicsQuit")
              )
            ),
            fluidRow(
              box(
                title = "Mode repartition", status = "primary",solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("statModeUserPlot")
              ),
              infoBoxOutput("NbMorningCigarettes"),
              infoBoxOutput("NbLunchCigarettes"),
              infoBoxOutput("NbNightCigarettes"),
              infoBoxOutput("NbWorkCigarettes"),
              infoBoxOutput("NbHomeCigarettes")
              
            ), 
            fluidRow(
              box(
                title = "Cities visited by the user", status = "success",solidHeader = TRUE,
                collapsible = TRUE,
                leafletOutput("mymap")
              )
            )
          ),
          tabPanel(title="Info",
                   fluidPage(
                     fluidRow(
                       valueBoxOutput("suname"),
                       valueBoxOutput("suage"),
                       valueBoxOutput("suagecgy")
                     ),
                     fluidRow(
                       valueBoxOutput("sumoneysaved",width = 6),
                       valueBoxOutput("sucigsaved",width = 6)
                     ),
                     fluidRow(
                       valueBoxOutput("suoverallprog",width = 3),
                       valueBoxOutput("suoverallprogcgy",width = 3),
                       valueBoxOutput("subestprograte",width = 3),
                       valueBoxOutput("suoverallengagement",width = 3)
                     ),
                     fluidRow(
                       valueBoxOutput("sumeanconscig",width = 3),
                       valueBoxOutput("sumeanconscigwday",width = 3),
                       valueBoxOutput("sumeanconscigwend",width = 3),
                       valueBoxOutput("sumeanconscigslotval",width = 3)
                     )
                   )
            
          ),
          tabPanel(title="Classic",
                   fluidRow(
                     box(title = "Cigarettes consumption per weekday", status="primary", solidHeader = TRUE,
                         plotlyOutput("suclcigcons")
                     ),
                     box(title = "Cigarettes consumption repartition", status="primary", solidHeader = TRUE, 
                         plotlyOutput("suclcigconsrep")
                     ),
                     valueBoxOutput("suclcigcons7")
                   ),
                   fluidRow(
                     box(title = "Mean of Cigarettes consumption per weekday",status="warning", solidHeader = TRUE,
                         plotlyOutput("suclcigconsmean")
                     ),
                     box(title = "Std of Cigarettes consumption per weekday",status="warning", solidHeader = TRUE,
                         plotlyOutput("suclcigconsstd")
                     )
                   ),
                   fluidRow(
                     box(title = "Progress per week",status="danger", solidHeader = TRUE,
                         plotlyOutput("suclprogress")
                     ),
                     box(title = "Progress rate per week",status="danger", solidHeader = TRUE,
                         plotlyOutput("suclprogressrate")
                     )
                   )
          ),
          tabPanel(title="Week",
                   fluidRow(
                     box(title = "Cigarette consumption per time slot per weekday",status = "warning", solidHeader = TRUE, width = 9,
                         plotlyOutput("suweslot")),
                     box(title="Select the day", background = "yellow", width = 3,
                         selectInput("suweselectwday", "Please choose a day: ", "")
                     )
                   ),
                   fluidRow(
                     box(title = "Cigarette consumption per week",status="danger", solidHeader = TRUE, width = 12,
                         plotlyOutput("suweweek"))
                   ),

                   fluidRow(
                     box(title="Select the mode", background = "green", width = 3,
                         selectInput("suweselectmode", "Please choose a day: ", "")),
                     box(title = "Cigarette consumption per week per mode",status = "success", solidHeader = TRUE, width = 9,
                         plotlyOutput("suwemode"))
                   ),
                   fluidRow(
                     box(title = "Cigarette consumption per day per week",status = "info", solidHeader = TRUE, width = 9,
                         plotlyOutput("suweday")),
                     box(title="Select the week", background = "aqua", width = 3,
                         selectInput("suweselectweek", "Please choose a week: ", "")
                     )
                   )
          
                   
          ),
          tabPanel(title="Engagement",
                   fluidRow(box(title= "Engagement per day", status = "danger", solidHeader = TRUE, width = 12,
                                plotlyOutput("suenperday")
                     
                   )),
                   fluidRow(box(title= "Engagement per week", status = "warning", solidHeader = TRUE, width = 12,
                                plotlyOutput("suenperweek")
                                
                   ))
          ),
          tabPanel(title="AllDays",
                   fluidRow(box(title= "Cigarettes consumption over all period", status = "danger", solidHeader = TRUE, width = 12,
                                plotlyOutput("suadcigcons")
                                
                   )),
                   fluidRow(box(title= "Mode usage over all period", status = "warning", solidHeader = TRUE, width = 12,
                                plotlyOutput("suadmode")
                                
                   )),
                   fluidRow(
                     box(title="Select the mode", background = "yellow", width = 4,
                      selectInput("suadmodeChoice", "Please choose a mode: ", "")
                     )
                   )
          )
        )
      )
    )
  )
)