# Libraries
library(shiny)
library(shinydashboard)
library(shinyBS)
library(htmltools)
library(rintrojs)

library(tidyverse)

library(httr)
library(XML)
library(jsonlite)

library(EIAdata)
library(lubridate)

library(forecast)
library(tseries)

library(formattable)
library(DT)
library(leaflet)
library(plotly)

library(readr)
petrol_data <- read_csv("./data/petrol_data.csv")

dates <- petrol_data$StartDate %>%
  unlist() %>%
  unique()

## Get EIA Data
usgcReg <- read_csv("./data/usgcReg.csv")

usgcULSD <- read_csv("./data/usgcULSD.csv")

wti <- read_csv("./data/wti.csv")

### Get Petrojam Data
petrojam_data<-read_csv("./data/petrojam_data.csv")

# Get USD exchange rate
XRate_data <- read_csv("./data/XRate_data.csv")

# Get Grocery Data
grocerystores <- read_csv("./data/grocerystores.csv")
  
  