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
# 
covid <- read_csv("data/COVID-19_Outcomes_by_Vaccination_Status.csv",col_types = list(.default = col_character())) |>
  mutate(Date = as.Date(`Week End`, format = "%m/%d/%Y")) |>
  mutate_at(c(4:21), as.numeric) |>
  mutate("Vaccinated rate (per 10,000)" = round((`Outcome Vaccinated`/`Population Vaccinated`)*10000, 2),
         "Unvaccinated rate (per 10,000)" = round((`Outcome Unvaccinated`/`Population Unvaccinated`)*10000, 2),
         "Boosted rate (per 10,000)" = round((`Outcome Boosted`/`Population Boosted`)*10000, 2),
         # "Vaccinated (logged)" = log(`Outcome Vaccinated`),
         # "Unvaccinated (logged)" = log(`Outcome Unvaccinated`),
         # "Boosted (logged)" = log(`Outcome Boosted`),
         "Total" = `Population Vaccinated` + `Population Unvaccinated` + `Population Boosted`,
         "Percent boosted" = round((`Population Boosted`/Total),2)) |>
  group_by(Outcome,`Age Group`) |>
  arrange(Date) |>
  mutate("Cumulative Vaccinated" = cumsum(replace_na(`Outcome Vaccinated`,0)),
         "Cumulative Unvaccinated" = cumsum(replace_na(`Outcome Unvaccinated`,0)),
         "Cumulative Boosted" = cumsum(replace_na(`Outcome Boosted`,0)))

# Define the UI function for a education finance simulator template application 

shinyUI(fluidPage(
  
  titlePanel("Vaccinations and covid outcomes, Chicago, Il"),
  h5("Source: City of Chicago Data Portal, https://data.cityofchicago.org/Health-Human-Services/COVID-19-Outcomes-by-Vaccination-Status/6irb-gasv/data, last updated: 12/18/2022"),
  
  # Sidebar with a slider input for number of bins
    
  mainPanel(
    tabsetPanel(
      tabPanel("Rates", plotlyOutput('plot', height = "700px", width = "1000px")),
      tabPanel("Weekly totals", plotlyOutput('plot2', height = "700px", width = "1000px")),
      tabPanel("Running total", plotlyOutput('plot3', height = "700px", width = "1000px")),
      tabPanel("Table",dataTableOutput("table")),
      
  )
),
hr(),
fluidRow(
  
  column(6,
         # sidebarLayout(
         #   sidebarPanel(
             # Select Justices name here
             selectizeInput("xvar",
                            label = "Select an outcome",
                            choices = unique(covid$Outcome),
                            selected = "Deaths"
                            
             ),),

  column(6,
         dateRangeInput("daterange",
                        "Select a date range:",
                        start = "2021-04-03",
                        end = "2022-12-03",
                        min = "2021-04-03",
                        max = "2022-12-03",
                        format = "mm/dd/yy",
                        separator = " - "),
         ),
  column(6,"."),
  column(6,
         selectizeInput("xvar2",
                        label = "Select an age cohort",
                        choices = unique(covid$`Age Group`),
                        selected = "All"
                        
         )
  ),
  column(6,
         helpText("Download a .csv of your current selections"),
         downloadButton(outputId = "download_data", 
                        label = "Download")
         
  ),
  column(width = 6)
        
             
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
             


)
)
)

