# PURPOSE ---------

  # Cleans the City of Chicago dataset, creates input dataframe for R shiny 

# PREPARED BY ------

# Chris Poulos

# 2022-12-17

# load packages and setwd as ~GitHub/irrpp-ed-report ---------

options(scipen = 999)

library(tidyverse)
library(readxl)
library(gt)
library(scales)
library(ggrepel)
library(plyr)
library(base)

setwd("~/GitHub/coviddeaths")

# Read in CSV (downloaded on 2022-12-17 from https://data.cityofchicago.org/Health-Human-Services/COVID-19-Outcomes-by-Vaccination-Status/6irb-gasv/data)

covid <- read_csv("original_data/COVID-19_Outcomes_by_Vaccination_Status.csv",col_types = list(.default = col_character())) |>
  mutate(Date = as.Date(`Week End`, format = "%m/%d/%Y")) |>
  mutate_at(c(4:21), as.numeric) |>
  mutate("Vaccinated" = round((`Outcome Vaccinated`/`Population Vaccinated`)*10000, 2),
         "Unvaccinated" = round((`Outcome Unvaccinated`/`Population Unvaccinated`)*10000, 2),
         "Boosted" = round((`Outcome Boosted`/`Population Boosted`)*10000, 2))

covid <- covid[,-c(4:16,20:21)] |>
  pivot_longer(cols = c(Vaccinated,Unvaccinated,Boosted), names_to = "Vaccination status", values_to = "Rate per 10,000")
write_rds(covid,"coviddeaths/data/covid_deaths.rds")

a <- covid |>
  filter(`Age Group`== "All") |>
  filter(Outcome == "Deaths") |>
  group_by(`Vaccination status`)
