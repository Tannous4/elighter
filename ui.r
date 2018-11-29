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
    tags$hr(),
    checkboxInput("header", "Header", TRUE),
    fileInput("file2", "Choose Excel File",
              accept = c(".xlsx")
              
    ),
    tags$hr(),
    checkboxInput("header", "Header", TRUE)
  ),
  dashboardBody(
    
    tabItems(
      # First tab content
      tabItem(tabName = "AllUsers",
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
            # plotOutput("FsFood")
          )
        )
      ),
      
      # Second tab content
      tabItem(tabName = "SingleUser",
        fluidRow(
          tabBox(
            title=tagList(shiny::icon("user"), "User Settings"),
            id = "tabset1", height = "200px",
            tabPanel("User", 
                     selectInput("userChoice", "Please choose a user: ", "")
            ),
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
      )
    )
  )
)