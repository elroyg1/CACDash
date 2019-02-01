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

GET_petrol_items <- GET("http://cac.gov.jm/api/productitems/read.php", 
                        query = list(Key="06c2b56c-0d8e-45e5-997c-59eff33eabc2",
                                     SurveyType = 4)
) %>%
  content() %>%
  unlist() %>%
  enframe() %>%
  separate(name, into = c(paste0("x", 1:2))) %>%
  group_by_at(vars(-value)) %>%
  mutate(row_id=1:n()) %>%
  ungroup() %>%
  spread(x2, value) %>%
  select(itemid, itemname)

GET_outlet <- GET("http://cac.gov.jm/api/outlets/read.php",
                         query = list(Key = "06c2b56c-0d8e-45e5-997c-59eff33eabc2",
                                      Status = "Active")) %>%
  content() %>%
  unlist() %>%
  enframe() %>%
  separate(name, into = c(paste0("x", 1:2))) %>%
  group_by_at(vars(-value)) %>%
  mutate(row_id=1:n()) %>%
  ungroup() %>%
  spread(x2, value) %>%
  filter(status == "Active") %>%
  select(outletid, name, town, parish, 
         loclatitude, loclongitude)

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
GET_grocery_dates <- GET("http://cac.gov.jm/api/surveys/read.php", 
                         query = list(Key="06c2b56c-0d8e-45e5-997c-59eff33eabc2",
                                      SurveyType = 3)) %>%
  content() %>%
  unlist() %>%
  enframe() %>%
  separate(name, into = c(paste0("x", 1:2))) %>%
  group_by_at(vars(-value)) %>%
  mutate(row_id=1:n()) %>%
  ungroup() %>%
  spread(x2, value) %>%
  select(startdate) %>%
  arrange(desc(startdate))

grocery_items <- GET("http://cac.gov.jm/api/productitems/read.php", 
                     query = list(Key="06c2b56c-0d8e-45e5-997c-59eff33eabc2",
                                  SurveyType = 3)
) %>%
  content() %>%
  unlist() %>%
  enframe() %>%
  separate(name, into = c(paste0("x", 1:2))) %>%
  group_by_at(vars(-value)) %>%
  mutate(row_id=1:n()) %>%
  ungroup() %>%
  spread(x2, value) %>%
  select(-c(row_id, x1)) 

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
