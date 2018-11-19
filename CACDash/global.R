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

# Get Petrol data
petrol_data <- GET("http://cac.gov.jm/dev/SurveyEnquiry/PetrolPrices.php?Key=e8189538-d0ca-4899-adae-18f454eca9f9") %>%
  content() %>%
  filter(!is.na(Price),
         Price > 0,
         Price < 400) %>%
  mutate(LocLongitude = as.numeric(LocLongitude),
         LocLatitude = as.numeric(LocLatitude))

dates <- petrol_data$StartDate %>%
  unlist() %>%
  unique()

# Get Grocery Data
getGroceryData <- GET("https://api.jamnav.com/v1/locations/",
                      add_headers(Authorization="Token ffe38843ae62cfd41c964c803f0a800acfa6485d")
)%>%
  content(as="parsed")%>%
  toJSON() %>%
  fromJSON()

grocerystores <- getGroceryData[["features"]] %>%
  flatten() %>%
  mutate(grocery = map(properties.categories, str_detect,
                       pattern = "Grocery Store"),
         grocery = map_lgl(grocery, any),
         lng = map(geometry.coordinates, pluck, 1) %>% 
           unlist(),
         lat = map(geometry.coordinates, pluck, 2) %>% 
           unlist(),
         store = unlist(properties.name)) %>%
  filter(grocery == T)

## Get EIA Data
usgcReg <- getEIA(key = "9292085e1004f26d04458213e09c3051",
                  ID = "PET.EER_EPMRU_PF4_RGC_DPG.D") %>%
  as.data.frame() %>%
  rownames_to_column("Date") %>%
  mutate(Date = as_date(Date)) %>%
  select(Date, Price = PET.EER_EPMRU_PF4_RGC_DPG.D)

usgcULSD <- getEIA(key = "9292085e1004f26d04458213e09c3051",
                   ID = "PET.EER_EPD2DXL0_PF4_RGC_DPG.D") %>%
  as.data.frame() %>%
  rownames_to_column("Date") %>%
  mutate(Date = as_date(Date)) %>%
  select(Date, Price = PET.EER_EPD2DXL0_PF4_RGC_DPG.D)

wti <- getEIA(key = "9292085e1004f26d04458213e09c3051",
              ID = "PET.RWTC.D") %>%
  as.data.frame() %>%
  rownames_to_column("Date") %>%
  mutate(Date = as_date(Date)) %>%
  select(Date, Price = PET.RWTC.D)

### Get Petrojam Data
# Create a list of petrojam urls
Dates <- seq(2009, year(Sys.Date()), 1)
Pages <- c(NA, 1, 2)
urlHeads <- rep("http://www.petrojam.com/price-index?field_price_date_value%5Bvalue%5D%5Byear%5D=", 
                times = length(Pages) * length(Dates))
urlDates <- sort(rep(Dates, times = length(Pages)))
urlMid <- rep("&page=", 
              times = length(Pages) * length(Dates))
urlPages <- rep(Pages, times = length(Dates))
petrojam_urls <- pmap_chr(list(urlHeads, urlDates, urlMid, urlPages), paste, sep = "")
#Creates Petrojam Data table
petrojam_tables <- map(petrojam_urls, readHTMLTable, as.data.frame = T, which = 1)
petrojam_data <- reduce(petrojam_tables, bind_rows)
#Clean petrojam data table
petrojam_data <- petrojam_data[ ,-c(5:9)]
names(petrojam_data) <- str_trim(str_replace(names(petrojam_data), "\n",""))
petrojam_data$Date <- mdy(petrojam_data$Date)
petrojam_data[, 2:5] <- lapply(petrojam_data[, 2:5], as.numeric)

# Get USD exchange rate

BOJ_url <- paste("http://www.boj.org.jm/foreign_exchange/searchfx.php?iUSD=1&rate=1&strFromDate=2000-12-01&strToDate=",Sys.Date(),"&Enter=")
XRate_data <- readHTMLTable(BOJ_url, as.data.frame=T, which=4)

## Clean XRate_data
names(XRate_data) <- c("Date", "Buy", "USD Sell")
rowsIDs <- seq(1, nrow(XRate_data)-1, 2)
for (r in rowsIDs){
  XRate_data$Date[r+1] <- XRate_data$Date[r]
}
XRate_data <- na.omit(XRate_data)
XRate_data <- XRate_data[ , -2]
XRate_data$Date <- ymd(XRate_data$Date)
