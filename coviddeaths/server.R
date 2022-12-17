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
  mutate("Vaccinated" = round((`Outcome Vaccinated`/`Population Vaccinated`)*10000, 2),
         "Unvaccinated" = round((`Outcome Unvaccinated`/`Population Unvaccinated`)*10000, 2),
         "Boosted" = round((`Outcome Boosted`/`Population Boosted`)*10000, 2),
         "Vaccinated (logged)" = log(`Outcome Vaccinated`),
         "Unvaccinated (logged)" = log(`Outcome Unvaccinated`),
         "Boosted (logged)" = log(`Outcome Boosted`))

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
      pivot_longer(cols = c(Vaccinated,Unvaccinated,Boosted), names_to = "Vaccination status", values_to = "Rate per 10,000")
  })
  
  raw <- reactive({
    covidoutdate() |>
      pivot_longer(cols = c(`Outcome Unvaccinated`,`Outcome Vaccinated`,`Outcome Boosted`), names_to = "Vaccination status", values_to = "Totals")
  })
  
  logged <- reactive({
    covidoutdate() |>
      pivot_longer(cols = c(`Outcome Unvaccinated`,`Outcome Vaccinated`,`Outcome Boosted`), names_to = "Vaccination status", values_to = "Logged")
  })
  
  logged <- reactive({
    covidoutdate() |>
      pivot_longer(cols = c(`Unvaccinated (logged)`,`Vaccinated (logged)`,`Boosted (logged)`), names_to = "Vaccination status", values_to = "Natural log")
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
      data = logged(),
      x = ~ Date,
      y = ~`Natural log`,
      color = ~`Vaccination status`,
      type = 'bar',
      text = ~`Week End`,
      hoverinfo = "text",
      hovertext = paste("Date :", raw()$`Week End`,
                        "<br> Total :", raw()$`Natural log`)
    ) 
    
  })
  
  
})
  
  
#   output$trendPlot <- renderGraph({
#     if (length(input$name)==0) print("Please select at least one country")
#     
#     else {
#       df_trend <- covid  %>%
#         filter(Outcome %in% input$xvar)
#       
#       ggideal_point <- ggplot(df_trend) +
#         geom_line(aes(x=Date, y=Vaccinated_rate_per1000, by=Outcome, color=Outcome)) +
#         labs(x = "Year") +
#         labs(y = "Ideology") +
#         labs(title = "Ideal Points for Countries") +
#         scale_colour_hue("clarity",l=70, c=150) +
#         theme_few()
#       
#       # Year range
#       min_Year <- min(df_trend$Date)
#       max_Year <- max(df_trend$Date)
#       
#       # use gg2list() to convert from ggplot->plotly
#       gg <- gg2list(ggideal_point)
#       
#       # Send this message up to the browser client, which will get fed through to
#       # Plotly's javascript graphing library embedded inside the graph
#       return(list(
#         list(
#           id="trendPlot",
#           task="newPlot",
#           data=gg$data,
#           layout=gg$layout
#         )
#       ))
#     }
#   })
# })
