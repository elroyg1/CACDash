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
grocery_items <- GET("http://cac.gov.jm/dev/SurveyEnquiry/Items.php?Key=e8189538-d0ca-4899-adae-18f454eca9f9") %>%
  content() %>%
  filter(SurveyType == 3) %>%
  select(ItemName, ItemID) 

# Get BOJ Data
bank_institutions <- read.csv("./data/institutions.csv", 
                              stringsAsFactors = F) %>%
  unname() %>%
  unlist()

currencies <- c("USD", "GBP", "EUR", "CAD")

bojData <- "http://boj.org.jm/autobot/market_summary/read.php" %>%
  GET() %>%
  content() %>%
  unlist() %>%
  as.data.frame() %>%
  na.omit() %>%
  rownames_to_column()
