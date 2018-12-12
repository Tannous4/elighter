# Import library
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggmap)
library(ggpubr)
library(readxl)
library(lubridate)
library(leaflet)
library(knitr)
library(plyr)
library(dplyr)
library(tidyr)
library(plotly)

# Shiny App

source("C:/Users/Christopher/Desktop/ING5/data analytics/project_part_1/project/fonction.r")
source("C:/Users/Christopher/Desktop/ING5/data analytics/project_part_1/project/ui.r")
source("C:/Users/Christopher/Desktop/ING5/data analytics/project_part_1/project/server.r")

shinyApp(ui, server)
