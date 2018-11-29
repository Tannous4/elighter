# Import library
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggmap)
library(ggpubr)
library(readxl)
library(lubridate)
library(arules)
library(arulesViz)
library(leaflet)

# Shiny App

source("C:/Users/Christopher/Desktop/ING5/data analytics/project_part_1/ui.r")
source("C:/Users/Christopher/Desktop/ING5/data analytics/project_part_1/server.r")

shinyApp(ui, server)
