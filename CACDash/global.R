# Libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
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

library(DT)
library(leaflet)
library(plotly)

library(readr)

# Get Petrol data
petrol_data <- GET("http://cac.gov.jm/dev/SurveyEnquiry/PetrolPrices.php",
                   query = list(Key="e8189538-d0ca-4899-adae-18f454eca9f9")) %>%
  content() %>%
  filter(!is.na(Price),
         Price > 0,
         Price < 400) %>%
  mutate(LocLongitude = as.numeric(LocLongitude),
         LocLatitude = as.numeric(LocLatitude))

petrol_dates <- GET("http://cac.gov.jm/api/surveys/read.php", 
                    query = list(Key="06c2b56c-0d8e-45e5-997c-59eff33eabc2",
                                 SurveyType = 4)) %>%
  content() %>%
  unlist() %>%
  enframe() %>%
  separate(name, into = c(paste0("x", 1:2))) %>%
  group_by_at(vars(-value)) %>%
  mutate(row_id=1:n()) %>%
  ungroup() %>%
  spread(x2, value) %>%
  select(startdate, enddate) %>%
  arrange(desc(startdate)) 

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

# Get grocery products
grocery_items <- GET("http://cac.gov.jm/dev/SurveyEnquiry/Items.php", 
                     query = list(Key="e8189538-d0ca-4899-adae-18f454eca9f9")) %>%
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
