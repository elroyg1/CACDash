# Libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
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

petrol_dates <- petrol_data$StartDate %>%
  unlist() %>%
  unique()

## Get EIA Data
usgcReg <- read_csv("./data/usgcReg.csv")

usgcULSD <- read_csv("./data/usgcULSD.csv")

wti <- read_csv("./data/wti.csv")

# Get Petrojam Data
petrojam_data<-read_csv("./data/petrojam_data.csv")

# Get USD exchange rate
XRate_data <- read_csv("./data/XRate_data.csv")

# Get Grocery Data
grocerystores <- read_csv("./data/grocerystores.csv")

grocery_data <- read_csv("./data/grocery_data.csv")

# Get Grocery Survey dates  
grocery_dates <- grocery_data$SurveyDate %>%
  unlist() %>%
  unique()

# Get grocery products
grocery_products <- grocery_data %>%
  select(ProductDesc, BrandName, Description) %>%
  dplyr::arrange(ProductDesc) %>%
  unite(col = Products, ...=1:3, sep = " ") %>%
  unique()

# Get BOJ Data
bojData <- "CACDash/data/bojData.json" %>%
  read_json(simplifyVector = F)