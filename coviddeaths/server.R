#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load -------
library(shiny)
library(shinythemes)
# library(shinydashboard)
# library(shinyBS)
# library(htmltools)
library(DT)
# library(leaflet)
require(scales)
library(plotly)
library(tidyverse)
library(base)
library(ggthemes)


options(shiny.trace = TRUE)

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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  covidout <- reactive({
    covid |>
      filter(`Age Group` %in% input$xvar2) |>
      filter(Outcome %in% input$xvar)
  })
  
  covidoutdate <- reactive({
    covidout() |>
      filter(Date >= as.character(input$daterange[1]) & Date <= as.character(input$daterange[2]))
  })
  
  rate <- reactive({
    covidoutdate() |>
      pivot_longer(cols = c(`Vaccinated rate (per 10,000)`,`Unvaccinated rate (per 10,000)`, `Boosted rate (per 10,000)`), names_to = "Vaccination status", values_to = "Rate per 10,000")
  })
  
  raw <- reactive({
    covidoutdate() |>
      pivot_longer(cols = c(`Outcome Unvaccinated`,`Outcome Vaccinated`,`Outcome Boosted`), names_to = "Vaccination status", values_to = "Totals")
  })
  
  deathtoll <- reactive({
    covidoutdate() |>
      pivot_longer(cols = c(`Cumulative Vaccinated`,`Cumulative Unvaccinated`,`Cumulative Boosted`), names_to = "Vaccination status", values_to = "Cumulative toll")
  })
  
  tbl <- reactive({
    covidout()[,-c(4:13,20,21)]
  })

  
  output$plot <- renderPlotly({
    plot_ly(
      data = rate(),
      x = ~ Date,
      y = ~`Rate per 10,000`,
      color = ~`Vaccination status`,
      type = 'bar',
      text = ~`Week End`,
      hoverinfo = "text",
      hovertext = paste("Date :", rate()$`Week End`,
                        "<br> Rate :", rate()$`Rate per 10,000`)
  ) 
    
  })
  
  output$plot2 <- renderPlotly({
    plot_ly(
      data = raw(),
      x = ~ Date,
      y = ~`Totals`,
      color = ~`Vaccination status`,
      type = 'bar',
      text = ~`Week End`,
      hoverinfo = "text",
      hovertext = paste("Date :", raw()$`Week End`,
                        "<br> Total :", raw()$`Totals`)
    ) 
    
  })
  
  output$plot3 <- renderPlotly({
    plot_ly(
      data = deathtoll(),
      x = ~ Date,
      y = ~`Cumulative toll`,
      color = ~`Vaccination status`,
      type = 'bar',
      text = ~`Week End`,
      hoverinfo = "text",
      hovertext = paste("Date :", deathtoll()$`Week End`,
                        "<br> Total :", deathtoll()$`Cumulative toll`)
    ) 
    
  })
  
  output$table <- renderDataTable({
    
    covidoutdate()[,-c(4:13,20,21)]
  })
    
    
  output$download_data <- downloadHandler(
    
    filename = function() {
      # this names the csv file with today's date
      paste('output-', Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      write_csv(covidoutdate()[,-c(4:13,20,21,22)], file)
    }
    
  )
  
})

