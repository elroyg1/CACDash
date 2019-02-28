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
library(jsonlite)
library(lubridate)
library(rvest)

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
  select(surveytype, outletid, name, town, parish, 
         loclatitude, loclongitude)

# Get Petrojam Data
get_petrojam_data <- function(selecteddate) {
  
  #Get year of date
  sYear <- year(selecteddate)
  
  #Initialize empty dataframe
  petrojamdata <- NULL
  
  #Execute loop
  for (i in c(0,1,2)) {
    
    petrolHTMLTable <- paste0("http://www.petrojam.com/price-index?field_price_date_value%5Bvalue%5D%5Byear%5D=",
                              sYear,
                              "&page=",
                              i) %>%
      read_html() %>%
      html_table() 
    
    if(length(petrolHTMLTable) != 0) {
      
      petrolTable <- petrolHTMLTable[[1]] %>%
        select(1:4,10) %>%
        mutate(Date = mdy(Date))
      
      petrojamdata <- bind_rows(petrojamdata, petrolTable)
      
    } else {
      return(petrojamdata)
    }
    
    
  }
  
  return(petrojamdata)
}

# Get USD exchange rate
get_usd_rate <- function(selecteddate) {
  
  XRate_data = paste("http://www.boj.org.jm/foreign_exchange/searchfx.php?iUSD=1&rate=1&strFromDate=", selecteddate,"&strToDate=",selecteddate,"&Enter=") %>% 
    XML::readHTMLTable(as.data.frame=T, which=4) %>%
    rename("Sell" = 3) %>%
    summarise(Sell = as.numeric(as.character(Sell[!is.na(Sell)])))
  
  return(XRate_data$Sell[1])
  
}

forecastForexRate <- function(currency, selecteddate, rate) {
  
  XRate_data <- paste0("http://www.boj.org.jm/foreign_exchange/searchfx.php?i",
                       currency,
                       "=1&rate=1&strFromDate=",
                       as.character(as.Date(selecteddate) %m-% years(5)),
                       "&strToDate=",
                       selecteddate,
                       "&Enter=") %>% 
    XML::readHTMLTable(as.data.frame=T, which=4) %>%
    rename("sell" = 3, "buy" = 2, "Date"=1) %>%
    mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
           sell = as.numeric(as.character(lead(sell, 1))),
           buy = as.numeric(as.character(lead(buy, 1)))) %>%
    na.omit() %>%
    arrange(Date) %>%
    select(Date, rate)
  
}

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
