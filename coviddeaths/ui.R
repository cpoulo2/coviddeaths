#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyBS)
library(htmltools)
library(DT)
library(leaflet)
require(scales)
library(plotly)
library(tidyverse)
library(ggthemes)

options(shiny.trace = TRUE)

covid <- read_csv("data/COVID-19_Outcomes_by_Vaccination_Status.csv",col_types = list(.default = col_character())) |>
  mutate(Date = as.Date(`Week End`, format = "%m/%d/%Y")) |>
  mutate_at(c(4:21), as.numeric) |>
  mutate("Vaccinated" = round((`Outcome Vaccinated`/`Population Vaccinated`)*10000, 2),
         "Unvaccinated" = round((`Outcome Unvaccinated`/`Population Unvaccinated`)*10000, 2),
         "Boosted" = round((`Outcome Boosted`/`Population Boosted`)*10000, 2))

# Define the UI function for a education finance simulator template application 

shinyUI(fluidPage(
  
  titlePanel("Vaccinations and covid outcomes in Chicago"),
  
  # Sidebar with a slider input for number of bins
  
  sidebarLayout(
    sidebarPanel(
    h3("Covid outcome"),
    # Select Justices name here
    selectizeInput("xvar",
                   label = "Select an outcome",
                   choices = unique(covid$Outcome),
                   selected = "Deaths"
                   
    ),
    
    h3("Date"),
    # Select Justices name here
    
    # selectizeInput("start",
    #                label = "Select a start date",
    #                choices = covid$`Week End`,
    #                selected = "12/25/2021"
    #                
    # ),
    
    # sliderInput("daterange",
    #             "Date range:",
    #             min = as.Date("2021-04-03","%Y-%m-%d"),
    #             max = as.Date("2022-12-03","%Y-%m-%d"),
    #             value=c(as.Date("2021-04-03"),as.Date("2022-12-03")),
    #             timeFormat="%m/%d/%Y"),
    # 
    # ),
    
    # ),
    
    dateRangeInput("daterange",
                   "Date range:",
                   start = "2021-04-03",
                   end = "2022-12-03",
                   min = "2021-04-03",
                   max = "2022-12-03",
                   format = "mm/dd/yy",
                   separator = " - "),
    
    h3("Age"),
    # Select Justices name here
    selectizeInput("xvar2",
                   label = "Select an age cohort",
                   choices = unique(covid$`Age Group`),
                   selected = "All"
                   
    )
    ),
    
    
    
    
  mainPanel(
    tabsetPanel(
      tabPanel("Rates", plotlyOutput('plot', height = "700px", width = "1000px")),
      tabPanel("Totals", plotlyOutput('plot2', height = "700px", width = "1000px")),
      tabPanel("Natural log", plotlyOutput('plot3', height = "700px", width = "1000px"))
  )
)
)
)
)

