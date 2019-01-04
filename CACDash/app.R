
source("global.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  skin = "black",
  
  dashboardHeader(
    title = "CAC Consumer Data Dashboard",
    titleWidth = 350
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home",
               tabName= "home",
               icon = icon("home")),
      menuItem("Petrol", 
               tabName = "petrol",
               icon = icon("dashboard")),
      menuItem("Grocery",
               tabName = "grocery",
               icon = icon("shopping-cart")),
      menuItem("Hardware",
               tabName = "hardware",
               icon = icon("building")),
      menuItem("Textbook",
               tabName = "textbook",
               icon = icon("book")),
      menuItem("Forex",
               tabName = "forex",
               icon = icon("usd"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        img(
          src = "caclogo.jpg",
          height = 50,
          width = 50
        ),
        h1("CAC Consumer Data Dashboard"),
        hr(),
        p("This dashboard is designed to display data on the consumer economy of Jamaica.
          Users can see the latest data collected and collated by the CAC and its Data Partners
          on the Petrol, Grocery, Hardware, Textbook and Forex markets. We believe that knowledge
          is power and that Jamaica will only become the place of choice to do business if we 
          are all empowered to make informed decisions."),
        br(),
        p("To get started, select one of the menu options."),
        br(),
        h4(
          HTML('&copy'),
          '2018 By Consumer Affairs Commission. '
        )
        ),
      tabItem(
        tabName = "petrol",
        
        fluidRow(
          introBox(column(width = 3,
                          box(
                            width = NULL,
                            title = "",
                            collapsible = F,
                            introjsUI(),
                            actionButton("petrolHelp","Take the Tour"),
                            br(),
                            introBox(
                              selectInput("date",
                                          "Choose a date",
                                          petrol_dates),
                              data.step = 2,
                              data.intro = "Choose your survey date. Try it out",
                              data.position = "right"),
                            introBox(radioButtons(
                              inputId = "fuel",
                              label = "Select the fuel type",
                              choices = c("87" = "E-10 GASOLENE - 87 OCTANE NONE 1 L",
                                          "90" = "E-10 GASOLENE - 90 OCTANE NONE 1 L",
                                          "Diesel" = "AUTO DIESEL NONE 1 L",
                                          "ULSD" = "ULTRA LOW SULPHUR DIESEL(ULSD) NONE 1 L")
                            ),
                            data.step = 3,
                            data.intro = "Select your fuel type here. Make your choice."),
                            
                            introBox(actionButton("view_Petrol_Prices","View Prices"),
                                     data.step = 4,
                                     data.intro = "Now click here. Give it a try."),
                            
                            br(),
                            
                            introBox(downloadButton("downloadPetrolData", "Download Data"),
                                     data.step = 12,
                                     data.intro = "When you're done you can download the data.")
                          )
          ),
          data.step = 1,
          data.intro = "Welcome to the Petrol Prices Section of the CAC Consumer Data Dashboard. Click Next to continue the Tour.",
          data.position = "right"
          ),
          column(width = 9,
                 introBox(box(
                   width = NULL,
                   title = "Station Locations and Prices",
                   collapsible = F,
                   tags$script('
                               $(document).ready(function () {
                               navigator.geolocation.getCurrentPosition(onSuccess, onError);
                               
                               function onError (err) {
                               Shiny.onInputChange("geolocation", false);
                               }
                               
                               function onSuccess (position) {
                               setTimeout(function () {
                               var coords = position.coords;
                               console.log(coords.latitude + ", " + coords.longitude);
                               Shiny.onInputChange("geolocation", true);
                               Shiny.onInputChange("lat", coords.latitude);
                               Shiny.onInputChange("long", coords.longitude);
                               }, 1100)
                               }
                               });
                               '),
                   introBox(leafletOutput("mapPlot"),
                            data.step = 6,
                            data.intro = "Go ahead and click a dot, see what happens. 
                            You can zoom in and out using the controls in the upper left, and the Legend in the bottom right tells you how this price compares to the whole island.")
                   ),
                   data.step = 5,
                   data.intro = "A map of Jamaica will appear here with gas stations identified")
                 )
      ),
      fluidRow(
        column(
          width = 9,
          introBox(tabBox(
            width = NULL,
            title = "Select a point on the map",
            id = "charts",
            tabPanel("Past",
                     plotlyOutput("chart")),
            tabPanel("Present",
                     dataTableOutput("table")),
            tabPanel("Future",
                     plotOutput("forecast"))
          ),
          data.step = 7,
          data.intro = "Click the Past Tab to see the where price is coming from, the Present Tab to see where price is and the Future Tab to see where price (possibly) is going.")
        ),
        column(
          width = 3,
          box(
            width = NULL,
            introBox(infoBoxOutput(width = NULL,
                                   "WTI"),
                     bsTooltip(
                       "WTI",
                       "Displays the West Texas Index spot price for 1 barrel of crude oil converted to J$/litre using the USD sell rate for the selected date noted by the BOJ.",
                       placement = "top",
                       trigger = "hover"
                     ),
                     data.step = 8,
                     data.intro = "Here's the price for 1 barrel of crude oil according to the West Texas Index. It's been converted to J$/litre, with the US$/bbl displayed below."
            ),
            introBox(infoBoxOutput(width = NULL,
                                   "USGC"),
                     bsTooltip(
                       "USGC",
                       "Displays the Us Gulf Coast price for the selected product, converted to J$/litre using the USD sell rate for the selected date noted by the BOJ.",
                       placement = "top",
                       trigger = "hover"
                     ),
                     data.step = 9,
                     data.intro = "Here's the price for the selected product in the US Gulf Coast at that time converted to J$/l with the US$/gal displayed below."),
            introBox(infoBoxOutput(
              width = NULL,
              "petrojam"),
              bsTooltip(
                "petrojam",
                "Displays the ex-refinery price sold to the trade by Petrojam in J$/litre",
                placement = "top",
                trigger = "hover"
              ),
              data.step = 10,
              data.intro = "Here's what Petrojam sold this product for. The price is JA$ per litre"),
            introBox(infoBoxOutput(width = NULL,
                                   "markup"),
                     bsTooltip(
                       "markup",
                       "Displays the difference between the ex-refinery price at which the station purchased the petrol and the price at which they sold the consumer.",
                       placement = "top",
                       trigger = "hover"
                     ),
                     data.step = 11,
                     data.intro = "And here's the Mark-up: the difference between what the station bought the product for and what they sold it for.") 
          )
        )
      )
      ),
      tabItem(
        tabName = "grocery",
        
        fluidRow(
          column(
            width = 4,
            box(
              width = NULL,
              pickerInput("grocery_products",
                          "Choose a product",
                          grocery_items$ItemName,
                          options = list(size = 10,
                                         "live-search" = TRUE,
                                         "actions-box" = TRUE,
                                         `selected-text-format` = "count > 3"),
                          multiple = T
                          ),
              actionButton("grocery_add","Select"),
              downloadButton("grocery_data_download", "Download")
            )
          ),
          column(
            width = 8,
            box(
              width = NULL,
              height = NULL,
              collapsible = TRUE,
              title = "Enter product volumes",
              uiOutput("items"),
              actionButton("grocery_items_vol", "Enter volumes")
            )
            )
        ),
        fluidRow(
          box(
            title = "Choose stores",
            width = NULL,
            withSpinner(leafletOutput("groceryMapPlot")),
            uiOutput("grocMapDates")  
          )
        ),
        fluidRow(
          tabBox(
              width = NULL,
              title = "Product Analysis",
              tabPanel("Stats",
                       tags$div(id="groceryProductStats")),
              tabPanel("Table",
                       dataTableOutput("grocery_store_comp")),
              tabPanel("Chart",
                       plotlyOutput("groceryChart"))
            )
        )
      ),
      
      tabItem(
        tabName = "hardware",
        h1("COMING SOON!")
      ),
      tabItem(
        tabName = "textbook",
        h1("COMING SOON!")
      ),
      tabItem(
        tabName = "forex",
        fluidRow(
          column(
            width = 4,
            box(
              width = NULL,
              pickerInput("forex_inst",
                          "Choose an institution",
                          bank_institutions,
                          options = list("live-search" = TRUE
              )),
              radioGroupButtons("forex_currencies",
                                "Choose your currency",
                                choices = currencies),
              radioGroupButtons("forex_txn",
                                "Buy or Sell",
                                choices = c("buy", 
                                            "sell")),
              actionButton("forex_show","Show"),
              downloadButton("forex_data_download", "Download")
            )
          ),
          column(
            width = 8,
            box(
              width = NULL,
              leafletOutput("forexMapPlot")))
        ),
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL,
              infoBoxOutput("forex_output")
            )
          )
        )
      )
      )
      )
  )

# Define server logic required 
server <- function(input, output, session) {
  
  session$allowReconnect(TRUE)
  
  # initiate hints on startup with custom button and event
  hintjs(session, options = list("hintButtonLabel"="Hope this hint was helpful"),
         events = list("onhintclose"=I('alert("We hope you find this app helpful. And remember: KNOWLEDGE IS POWER.")')))
  
  # start introjs when button is pressed with custom options and events
  observeEvent(input$petrolHelp,{
    introjs(session, 
            options = list("nextLabel"="Next",
                           "prevLabel"="Prev",
                           "skipLabel"="Skip"),
            events = list("oncomplete"=I('alert("We hope you find this app helpful. And remember: KNOWLEDGE IS POWER.")')))
  })
  
  # Petrol Section
  
  ## Subset Petrol data based on user selection
  map_data <- eventReactive(input$view_Petrol_Prices,{
    map_data <- petrol_data %>%
      filter(year(StartDate) == year(input$date) &
               month(StartDate) == month(input$date),
             ItemName == input$fuel,
             Price > 0)
    return(map_data)
  })
  
  ## Map Creation
  output$mapPlot <- renderLeaflet({
    # set colours for legend
    pal <- colorBin(
      palette = c("#00FF00", "#FFFF00", "#FF0000"),
      domain = map_data()$Price,
      bins = 5,
      pretty = T
    )
    
    #draw map
    leaflet(width = 400) %>%
      setView(lng = -77.29751,
              lat = 18.10958,
              zoom = 9) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addCircleMarkers(lng = as.numeric(map_data()$LocLongitude),
                       lat = as.numeric(map_data()$LocLatitude),
                       layerId = map_data()$MerchantID,
                       stroke =T,
                       fillOpacity = 0.8,
                       fillColor = pal(map_data()$Price),
                       popup = htmlEscape(paste(map_data()$MerchantName,
                                                map_data()$MerchantTown,
                                                " sold ",
                                                map_data()$ItemName,
                                                " for $",
                                                as.character(map_data()$Price),
                                                "/L",
                                                ", on ",
                                                map_data()$StartDate))
      )%>%
      addLegend("bottomleft",
                pal = pal,
                values = map_data()$Price,
                title = "Observed Price (Ja$/L)")
  })
  
  ## Zoom in on user location if given
  observe({
    if(!is.null(input$lat)){
      mapPlot <- leafletProxy("mapPlot")
      dist <- 0.1
      lat <- input$lat
      lng <- input$long
      mapPlot %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    }
  })

  output$WTI <- renderInfoBox({
    
    wtiDPB <- as.numeric(wti$Price[wti$Date == first(map_data()$StartDate)]) / 158.987 * as.numeric(as.character(XRate_data$`USD Sell`[XRate_data$Date == first(map_data()$StartDate)]))
    
    infoBox(
      title = "WTI (crude oil)",
      subtitle = paste0("(US$",as.numeric(wti$Price[wti$Date == first(map_data()$StartDate)]), "/bbl)"),
      href = "https://www.eia.gov/dnav/pet/pet_pri_spt_s1_d.htm",
      value =  round(wtiDPB, 2),
      icon = icon("usd")
    )
    
  })
  
  output$USGC <- renderInfoBox({
    
    usgcDPG <- ifelse(map_data()$ItemName[1] %in% c("E-10 GASOLENE - 87 OCTANE NONE 1 L",
                                        "E-10 GASOLENE - 90 OCTANE NONE 1 L",
                                        "AUTO DIESEL NONE 1 L"),
                      as.numeric(usgcReg$Price[usgcReg$Date == first(map_data()$StartDate)]),
                      as.numeric(usgcULSD$Price[usgcULSD$Date == first(map_data()$StartDate)]))
    
    infoBox(
      title = "USGC",
      subtitle = paste0("(US$",usgcDPG,"/gal)"),
      value = round(usgcDPG / 3.785 *  as.numeric(as.character(XRate_data$`USD Sell`[XRate_data$Date == first(map_data()$StartDate)])),2),
      href = "https://www.eia.gov/dnav/pet/pet_pri_spt_s1_d.htm",
      icon = icon("usd")
    )
  })
  
  output$petrojam <- renderInfoBox({
    
    petrEXP <- ifelse(map_data()$ItemName[1] == "E-10 GASOLENE - 87 OCTANE NONE 1 L",
                      petrojam_data$`Gasolene  87`[year(petrojam_data$Date)==year(first(map_data()$StartDate)) &
                                                     week(petrojam_data$Date) == week(first(map_data()$StartDate))-1],
                      ifelse(map_data()$ItemName[1] == "E-10 GASOLENE - 90 OCTANE NONE 1 L",
                             petrojam_data$`Gasolene 90`[year(petrojam_data$Date)==year(first(map_data()$StartDate)) &
                                                           week(petrojam_data$Date) == week(first(map_data()$StartDate))-1],
                             ifelse(map_data()$ItemName[1] == "AUTO DIESEL NONE 1 L",
                                    petrojam_data$`Auto Diesel`[year(petrojam_data$Date)==year(first(map_data()$StartDate)) &
                                                                  week(petrojam_data$Date) == week(first(map_data()$StartDate))-1],
                                    petrojam_data$ULSD[year(petrojam_data$Date)==year(first(map_data()$StartDate)) &
                                                         week(petrojam_data$Date) == week(first(map_data()$StartDate))-1]
                             )
                      )
    ) 
    
    
    infoBox(
      title = "Petrojam ex-refinery",
      value = round(petrEXP, 2),
      href = "http://www.petrojam.com/price-index",
      icon = icon("usd")
    )
    
  })
  
  observeEvent(input$mapPlot_marker_click, {
    
    clickedMarker<-input$mapPlot_marker_click
    
    # Chart analysis
    output$chart <- renderPlotly({
      
      petrol_data %>%
        filter(StartDate <= first(map_data()$StartDate),
               ItemName == map_data()$ItemName[1],
               Price > 20,
               Price < 200) %>%
        group_by(StartDate) %>%
        mutate(`National Average` = mean(Price, na.rm = T),
               `National Max` = max(Price, na.rm=T),
               `National Min` = min(Price, na.rm = T)) %>%
        ungroup() %>%
        filter(MerchantID == clickedMarker$id) %>%
        plot_ly(x = ~StartDate) %>%
        add_lines(y = ~`National Average`, 
                  name = "National Average") %>%
        add_lines(y = ~`National Max`, 
                  name = "National Max") %>%
        add_lines(y = ~`National Min`, 
                  name = "National Min") %>%
        add_trace(y = ~Price,
                  type = "scatter",
                  mode = "markers",
                  color = ~Parish,
                  name = ~MerchantName,
                  text = ~paste(MerchantName,
                                MerchantTown,
                                "$",Price, "/l")) %>%
        layout(yaxis = list(title = "Price (Ja$/L)"),
               xaxis = list(title = "Date",
                            rangeslider = list(
                              type = "date"
                            )
               )
        )
    })
    
    # Forecast Analysis
    output$forecast <- renderPlot({
      
      forecastPetrolData <- petrol_data%>%
        filter(Price !=0 & 
                 MerchantID == clickedMarker$id & 
                 ItemName == map_data()$ItemName[1])
      
      price_ts = ts(forecastPetrolData[,c("Price")])
      
      forecastPetrolData$clean_price = tsclean(price_ts)
      
      forecastPetrolData$clean_price_ma = ma(forecastPetrolData$clean_price,
                                             order = 4)
      
      price_ma = ts(na.omit(forecastPetrolData$clean_price_ma),
                    frequency = 4)
      decomp = stl(price_ma, s.window = "periodic")
      deseasonal_price <- seasadj(decomp)
      
      fit <- auto.arima(deseasonal_price, 
                        seasonal = T,
                        approximation = F)
      
      seas_fcast <- forecast(fit, h=24)
      
      autoplot(seas_fcast) + 
        labs(title = paste(clickedMarker$id, "forecasted ",
                           map_data()$ItemName[1], " price"),
             y = "J$/L")
    })
    
    output$markup <- renderInfoBox({
      
      markedup <- ifelse(map_data()$ItemName[1] == "E-10 GASOLENE - 87 OCTANE NONE 1 L",
                         map_data()$Price[map_data()$MerchantID == clickedMarker$id] - petrojam_data$`Gasolene  87`[year(petrojam_data$Date)==year(first(map_data()$StartDate)) &
                                                                                                                      week(petrojam_data$Date) == week(first(map_data()$StartDate))-1],
                         ifelse(map_data()$ItemName[1] == "E-10 GASOLENE - 90 OCTANE NONE 1 L",
                                map_data()$Price[map_data()$MerchantID == clickedMarker$id] - petrojam_data$`Gasolene 90`[year(petrojam_data$Date)==year(first(map_data()$StartDate)) &
                                                                                                                            week(petrojam_data$Date) == week(first(map_data()$StartDate))-1],
                                ifelse(map_data()$ItemName[1] == "AUTO DIESEL NONE 1 L",
                                       map_data()$Price[map_data()$MerchantID == clickedMarker$id] - petrojam_data$`Auto Diesel`[year(petrojam_data$Date)==year(first(map_data()$StartDate)) &
                                                                                                                                   week(petrojam_data$Date) == week(first(map_data()$StartDate))-1],
                                       map_data()$Price[map_data()$MerchantID == clickedMarker$id] - petrojam_data$ULSD[year(petrojam_data$Date)==year(first(map_data()$StartDate)) &
                                                                                                                          week(petrojam_data$Date) == week(first(map_data()$StartDate))-1]
                                )
                         )
      ) 
      
      infoBox(
        title = "Mark-up (J$/litre)",
        value = round(markedup, 2),
        icon = icon("usd")
      )
      
    })
    
  })
  
  ## Table Tab output
  output$table <- renderDataTable({
    map_data() %>%
      select(MerchantName, MerchantTown, Parish, Price) %>%
      group_by(MerchantTown) %>%
      formattable() %>%
      as.datatable()
  })
  

  

  # Groceries Section
  
  ## Save the selected items
  selected_items <- eventReactive(input$grocery_add,{
    
    # creates a list of the items the user selected
      
      selected_items <- NULL
    
    for (i in 1:length(input$grocery_products) ) {
      
      selected_items[i] <- grocery_items$ItemID[input$grocery_products[i] == grocery_items$ItemName]
    }
      
      return(selected_items)

  })
  
  ## List selected items for user to indicate volumes
  output$items <- renderUI({
    
    lapply(1:length(selected_items()), function(i){
      
      if (length(selected_items()) != 0){
        box(
          textInput(
            inputId = paste0(grocery_items$ItemName[grocery_items$ItemID == selected_items()[i]]),
            label = grocery_items$ItemName[grocery_items$ItemID == selected_items()[i]],
            value = "1"
          )  
        )
      }
      else{
        renderText({"Please choose some items"})
      }
    })
    
  })
  
  ## Create data frame of grocery products, prices and totals based on user selections
  grocery_prices <- eventReactive(input$grocery_items_vol,{
    
    grocery_prices <- NULL
    
    for (ID in selected_items()) {
      
      volume <- as.numeric(input[[paste0(grocery_items$ItemName[grocery_items$ItemID == ID])]])
      
      # Get Grocery data
      grocery_data <- GET("http://cac.gov.jm/dev/SurveyEnquiry/GroceryPrices.php",
                          query = list(Key="e8189538-d0ca-4899-adae-18f454eca9f9",
                                       ItemID = ID)) %>%
        content() %>%
        filter(!is.na(Price) & Price != 0)   
      
      grocery_prices <- bind_rows(grocery_prices, grocery_data) %>%
        filter(StartDate == first(StartDate)) %>%
        group_by(MerchantID) %>%
        dplyr::mutate(itemTot = Price * volume) %>%
        dplyr::mutate(Tot = sum(itemTot, na.rm = T)) %>%
        dplyr::mutate(grandTot = Tot + (.165 * Tot)) %>%
        ungroup()
    
    }
    
    return(grocery_prices)
  })
  
  ## Table Tab output
  output$grocery_store_comp <- renderDataTable({
    grocery_prices() %>%
      select(StartDate, MerchantName, MerchantTown, Parish, ItemName, Price) %>%
      group_by(MerchantTown) %>%
      formattable() %>%
      as.datatable()
  })
  
  ## Map Creation
  output$groceryMapPlot <- renderLeaflet({
    # set colours for legend
    pal <- colorBin(
      palette = c("#00FF00", "#FFFF00", "#FF0000"),
      domain = grocery_prices()$grandTot,
      bins = 5,
      pretty = T
    )
    
    #draw map
    leaflet(width = 400) %>%
      setView(lng = -77.29751,
              lat = 18.10958,
              zoom = 9) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addCircleMarkers(lng = as.numeric(grocery_prices()$LocLongitude),
                       lat = as.numeric(grocery_prices()$LocLatitude),
                       layerId = grocery_prices()$MerchantID,
                       stroke = T,
                       fillOpacity = 0.8,
                       fillColor = pal(grocery_prices()$grandTot),
                       popup = htmlEscape(paste("You can get your basket for about $",
                                                as.character(grocery_prices()$grandTot),
                                                " from ",
                                                grocery_prices()$MerchantName,
                                                grocery_prices()$MerchantTown,
                                                ", on ",
                                                grocery_prices()$StartDate))
      ) %>%
      addLegend("bottomleft",
                pal = pal,
                values = grocery_prices()$grandTot,
                title = "Total Estimated Cost (Tax incl.) (Ja$)") 
  })
  
  ## Zoom in on user location if given
  observe({
    if(!is.null(input$lat)){
      groceryMapPlot <- leafletProxy("groceryMapPlot")
      dist <- 0.1
      lat <- input$lat
      lng <- input$long
      groceryMapPlot %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    }
  })

  # Forex Section
  observeEvent(input$forex_show,{
    
    forex_data <- bojData %>%
      dplyr::filter(str_detect(rowname, input$forex_inst) &
                      str_detect(rowname, input$forex_currencies) &
                      str_detect(rowname, input$forex_txn))

    output$forex_output <- renderInfoBox(
      infoBox(
        title = "Rate",
        value = forex_data[1,2],
        subtitle = paste0(input$forex_txn, " volume ",forex_data[2,2]) 
      )
    )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

