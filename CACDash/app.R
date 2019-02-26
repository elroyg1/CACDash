
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
                                          petrol_dates$startdate),
                              data.step = 2,
                              data.intro = "Choose your survey date. Try it out",
                              data.position = "right"),
                            introBox(radioButtons(
                              inputId = "fuel",
                              label = "Select the fuel type",
                              choices = c(unique(unlist(GET_petrol_items$itemname)))
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
              selectInput("grocdate",
                          "Choose a date",
                          GET_grocery_dates$startdate),
              pickerInput("grocery_products",
                          "Choose a product",
                          grocery_items$itemname,
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
                       uiOutput("groceryStats")),
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
    
    GETResults <- GET("http://cac.gov.jm/api/productprices/read.php",
                    query = list(Key="06c2b56c-0d8e-45e5-997c-59eff33eabc2",
                                 SurveyType = 4,
                                 StartDate = input$date,
                                 EndDate = as.character(as.Date(input$date) + 1),
                                 ItemID = GET_petrol_items$itemid[GET_petrol_items$itemname == input$fuel])) %>%
      content() %>%
      toJSON() %>%
      fromJSON()
    
    ResultsDAta <- GETResults$records %>%
      mutate(price = as.numeric(price)) %>%
      unnest(itemid, outletid, price) %>%
      select(itemid, outletid, price) %>%
      inner_join(GET_outlet, "outletid") %>%
      mutate(price = as.numeric(price),
             loclongitude = as.numeric(loclongitude),
             loclatitude = as.numeric(loclatitude)) %>%
      filter(price > 0)
    
    return(ResultsDAta)
    
  })
  
  petrol_data <- eventReactive(input$view_Petrol_Prices,{
    
    GETResults <- GET("http://cac.gov.jm/api/productprices/read.php", 
                    query = list(Key="06c2b56c-0d8e-45e5-997c-59eff33eabc2",
                                 SurveyType = 4,
                                 ItemID = GET_petrol_items$itemid[GET_petrol_items$itemname == input$fuel],
                                 StartDate = as.Date(input$date) %m-% years(10),
                                 EndDate = as.Date(input$date) + 1,
                                 Page = 1,
                                 PageSize = 4000)) %>%
      content() %>%
      toJSON() %>%
      fromJSON()
    
    ResultsData <- GETResults$records %>%
      mutate(price = as.numeric(price)) %>%
      unnest(startdate, itemid, itemname, outletid, price) %>%
      select(startdate, itemid, itemname, outletid, price) %>%
      inner_join(GET_outlet, "outletid") %>%
      filter(price > 0) %>%
      group_by(startdate) %>%
      mutate(natlMean = mean(price, na.rm = T),
             natlMin = min(price, na.rm = T),
             natlMax = max(price, na.rm = T)) %>%
      ungroup()  
    
    return(ResultsData)
    
  })
  
  petrojam_data <-eventReactive(input$view_Petrol_Prices,{
    
    get_petrojam_data(input$date)
    
  })  
  
  ## Map Creation
  output$mapPlot <- renderLeaflet({
    # set colours for legend
    values <- as.numeric(map_data()$price)
    
    pal <- colorBin(
      palette = c("#00FF00", "#FFFF00", "#FF0000"),
      domain = values,
      bins = 5,
      pretty = FALSE
    )
    
    #draw map
    leaflet(width = 400) %>%
      setView(lng = -77.29751,
              lat = 18.10958,
              zoom = 9) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addCircleMarkers(lng = as.numeric(map_data()$loclongitude),
                       lat = as.numeric(map_data()$loclatitude),
                       layerId = map_data()$outletid,
                       stroke =T,
                       fillOpacity = 0.8,
                       fillColor = pal(as.numeric(map_data()$price)),
                       popup = htmlEscape(paste(map_data()$name,
                                                map_data()$town,
                                                " sold ",
                                                input$fuel,
                                                " for $",
                                                as.character(as.numeric(map_data()$price)),
                                                "/L",
                                                ", on ",
                                                input$date))
      )%>%
      addLegend("bottomleft",
                pal = pal,
                values = values,
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
  
  ## Petrol Stats
  observeEvent(input$view_Petrol_Prices,{
    
    output$WTI <- renderInfoBox({
      
      usdxrate = get_usd_rate(input$date)
      
      wti <- GET("http://api.eia.gov/series/",
                 query = list(
                   series_id = "PET.RWTC.D",
                   api_key = "9292085e1004f26d04458213e09c3051",
                   start = format(as.Date(input$date,format = "%Y-%m-%d"),"%Y%m%d"),
                   end = format(as.Date(input$date,format = "%Y-%m-%d"),"%Y%m%d")
                 )) %>%
        content() %>%
        unlist() %>%
        enframe() %>%
        filter(name == "series.data2")
      
      wtiDPB <- as.numeric(wti$value) / 158.987 * usdxrate
      
      
      infoBox(
        title = "WTI (crude oil)",
        subtitle = paste0("(US$", wti$value, "/bbl)"),
        href = "https://www.eia.gov/dnav/pet/pet_pri_spt_s1_d.htm",
        value =  round(wtiDPB, 2),
        icon = icon("usd")
      )
      
    })
    
    output$USGC <- renderInfoBox({
      
      usgc <- GET("http://api.eia.gov/series/",
                  query = list(
                    series_id = ifelse(input$fuel %in% c("E-10 GASOLENE - 87 OCTANE NONE 1 L",
                                                         "E-10 GASOLENE - 90 OCTANE NONE 1 L",
                                                         "AUTO DIESEL NONE 1 L"),
                                       "PET.EER_EPMRU_PF4_RGC_DPG.D",
                                       "PET.EER_EPD2DXL0_PF4_RGC_DPG.D"),
                    api_key = "9292085e1004f26d04458213e09c3051",
                    start = format(as.Date(input$date,format = "%Y-%m-%d"),"%Y%m%d"),
                    end = format(as.Date(input$date,format = "%Y-%m-%d"),"%Y%m%d")
                  )) %>%
        content() %>%
        unlist() %>%
        enframe() %>%
        filter(name == "series.data2")
      
      usgcDPG <- as.numeric(usgc$value)
      
      usdxrate = get_usd_rate(input$date)
      
      infoBox(
        title = "USGC",
        subtitle = paste0("(US$",usgcDPG,"/gal)"),
        value = round(usgcDPG / 3.785 *  usdxrate,2),
        href = "https://www.eia.gov/dnav/pet/pet_pri_spt_s1_d.htm",
        icon = icon("usd")
      )
    })
    
    output$petrojam <- renderInfoBox({
      
      petrEXP <- ifelse(input$fuel == "E-10 GASOLENE - 87 OCTANE NONE 1 L",
                        petrojam_data()$`Gasolene  87`[year(petrojam_data()$Date)==year(input$date) &
                                                       week(petrojam_data()$Date) == week(input$date)-1],
                        ifelse(input$fuel == "E-10 GASOLENE - 90 OCTANE NONE 1 L",
                               petrojam_data()$`Gasolene 90`[year(petrojam_data()$Date)==year(input$date) &
                                                             week(petrojam_data()$Date) == week(input$date)-1],
                               ifelse(input$fuel == "AUTO DIESEL NONE 1 L",
                                      petrojam_data()$`Auto Diesel`[year(petrojam_data()$Date)==year(input$date) &
                                                                    week(petrojam_data()$Date) == week(input$date)-1],
                                      petrojam_data()$ULSD[year(petrojam_data()$Date)==year(input$date) &
                                                           week(petrojam_data()$Date) == week(input$date)-1]
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
    
  })
  
  ## Chart creation and markup calculation
  observeEvent(input$mapPlot_marker_click, {
    
    clickedMarker<-input$mapPlot_marker_click
    
    # Chart analysis
    output$chart <- renderPlotly({
      
      petrol_data() %>%
        filter(outletid == clickedMarker$id) %>%
        plot_ly(x = ~as.Date(startdate)) %>%
        add_lines(y = ~natlMean,
                  name = "National Mean") %>%
        add_lines(y = ~natlMin,
                  name = "National Min") %>%
        add_lines(y = ~natlMax,
                  name = "National Max") %>%
        add_trace(y = ~price,
                  type = "scatter",
                  mode = "markers",
                  name = ~name,
                  text = ~paste(name,
                                town,
                                "$",
                                price, 
                                "/l")) %>%
        layout(title = ~name,
               yaxis = list(title = "Price (Ja$/L)"),
               xaxis = list(title = "Date"),
               showlegend = FALSE
        )
    })
    
    # Forecast Analysis
    output$forecast <- renderPlot({
      
      forecastPetrolData <- petrol_data() %>%
        filter(price !=0 & 
                 outletid == clickedMarker$id & 
                 itemname == input$fuel)
      
      forecastPetrolData$price %>%
        ts(frequency = 12) %>%
        ets() %>%
        forecast() %>%
        autoplot()
      
    })
    
    output$markup <- renderInfoBox({
      
      markedup <- ifelse(input$fuel == "E-10 GASOLENE - 87 OCTANE NONE 1 L",
                         as.numeric(map_data()$price[map_data()$outletid == clickedMarker$id]) - petrojam_data()$`Gasolene  87`[year(petrojam_data()$Date)==year(input$date) &
                                                                                                                      week(petrojam_data()$Date) == week(input$date)-1],
                         ifelse(input$fuel == "E-10 GASOLENE - 90 OCTANE NONE 1 L",
                                as.numeric(map_data()$price[map_data()$outletid == clickedMarker$id]) - petrojam_data()$`Gasolene 90`[year(petrojam_data()$Date)==year(input$date) &
                                                                                                                            week(petrojam_data()$Date) == week(input$date)-1],
                                ifelse(input$fuel == "AUTO DIESEL NONE 1 L",
                                       as.numeric(map_data()$price[map_data()$outletid == clickedMarker$id]) - petrojam_data()$`Auto Diesel`[year(petrojam_data()$Date)==year(input$date) &
                                                                                                                                   week(petrojam_data()$Date) == week(input$date)-1],
                                       as.numeric(map_data()$price[map_data()$outletid == clickedMarker$id]) - petrojam_data()$ULSD[year(petrojam_data()$Date)==year(input$date) &
                                                                                                                          week(petrojam_data()$Date) == week(input$date)-1]
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
    
    petrolDatatable <- map_data() %>%
      select(name, town, parish, price)
    
    datatable(petrolDatatable,
              options = list(
                dom = "lftp")
              )
      
  })
  
  ## Download Petrol Data
  output$downloadPetrolData<- downloadHandler(
    filename = function(){
      paste0("CACPetrolData_", map_data()$StartDate,".csv")
      },
    content = function(file) {
      write.csv(map_data(),file,row.names=F)
    },
    contentType = "csv"
  )

  # Groceries Section
  
  ## Save the selected items
  selected_items <- eventReactive(input$grocery_add,{
    
    # creates a list of the items the user selected
      
      selected_items <- NULL
    
    for (i in 1:length(input$grocery_products) ) {
      
      selected_items[i] <- as.numeric(grocery_items$itemid[input$grocery_products[i] == grocery_items$itemname])
    }
      
      return(selected_items)

  })
  
  ## List selected items for user to indicate volumes
  output$items <- renderUI({
    
    lapply(1:length(selected_items()), function(i){
      
      if (length(selected_items()) != 0){
        box(
          textInput(
            inputId = paste0(grocery_items$itemname[grocery_items$itemid == selected_items()[i]]),
            label = grocery_items$itemname[grocery_items$itemid == selected_items()[i]],
            value = "1"
          )  
        )
      }
      else{
        renderText({"Please choose some items"})
      }
    })
    
  })

  ## Map Creation
  output$groceryMapPlot <- renderLeaflet({
    
    pal <- colorBin(
      palette = c("#00FF00", "#FFFF00", "#FF0000"),
      domain = as.numeric(grocery_prices()$grandTot),
      bins = 5,
      pretty = F
    )
    
    #draw map
    leaflet(width = 400) %>%
      setView(lng = -77.29751,
              lat = 18.10958,
              zoom = 9) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addCircleMarkers(lng = as.numeric(grocery_prices()$loclongitude),
                       lat = as.numeric(grocery_prices()$loclatitude),
                       layerId = grocery_prices()$outletid,
                       stroke = T,
                       fillOpacity = 0.8,
                       fillColor = pal(as.numeric(grocery_prices()$grandTot)),
                       popup = htmlEscape(paste("You can get your basket for about $",
                                                as.character(grocery_prices()$grandTot),
                                                " from ",
                                                grocery_prices()$name,
                                                grocery_prices()$town,
                                                ", on ",
                                                grocery_prices()$startdate))
      ) %>%
      addLegend("bottomleft",
                pal = pal,
                values = as.numeric(grocery_prices()$grandTot),
                title = "Total Estimated Cost (Tax incl.) (Ja$)") 
    
  })  

  ## Create data frame of grocery products, prices and totals based on user selections
  grocery_prices <- eventReactive(input$grocery_items_vol,{
    
    GroceryUrlResults <- paste0("http://www.cac.gov.jm/api/productprices/read.php?Key=06c2b56c-0d8e-45e5-997c-59eff33eabc2&SurveyType=3",
                         "&StartDate=",
                         input$grocdate,
                         "&EndDate=",
                         as.character(as.Date(input$grocdate) + 1),
                         "&ItemID=",
                         paste(selected_items(),collapse = ","),
                         "&Page=1&PageSize=4000") %>%
      GET() %>%
      content() %>%
      toJSON() %>%
      fromJSON()
    
    
    grocery_prices <- GroceryUrlResults$records %>%
      mutate(price = as.numeric(price)) %>%
      unnest(startdate, itemid, itemname, outletid) %>%
      select(startdate, itemid, itemname, outletid, price) %>%
      inner_join(GET_outlet, "outletid")  %>%
      filter(price != "0", !is.na(price))
    
    groceryData <- NULL
    
    for (ID in selected_items()) {
      
      volume <- as.numeric(input[[paste0(grocery_items$itemname[grocery_items$itemid == ID])]])
      
      groceryData <-  grocery_prices %>%
        filter(itemid == ID) %>%
        group_by(outletid) %>%
        dplyr::mutate(itemTot = as.numeric(price) * volume) %>%
        dplyr::mutate(Tot = sum(itemTot, na.rm = T)) %>%
        dplyr::mutate(grandTot = Tot + (.165 * Tot)) %>%
        ungroup() %>%
        filter(grandTot > 0) %>%
        bind_rows(groceryData)
      
    }
    
    return(groceryData)

  })
  
  ## List stats for selected items
  output$groceryStats <-  renderUI({
    
    statsData <-  grocery_prices() %>%
      group_by(itemid, itemname) %>%
      summarise(maxPrice = max(as.numeric(price), na.rm = T),
                minPrice = min(as.numeric(price), na.rm = T),
                meanPrice = mean(as.numeric(price), na.rm = T)) %>%
      ungroup
    
    print(statsData)
    
    lapply(1:length(selected_items()), function(i){
      
      if (length(selected_items()) != 0){
        
        box(
          infoBox(
            title = paste0("National Max $", as.character(round(statsData$maxPrice[statsData$itemid == selected_items()[i]],2))),
            value = paste0("National AVG $", as.character(round(statsData$meanPrice[statsData$itemid == selected_items()[i]],2))),
            subtitle = paste0("National Min $", as.character(round(statsData$meanPrice[statsData$itemid == selected_items()[i]],2))),
            width = NULL
          ),
          title = statsData$itemname[statsData$itemid == selected_items()[i]],
          width = 4
        )
      }
      else{
        renderText({"Please choose some items"})
      }
    })
    
  })
  
  ## Table Tab output
  output$grocery_store_comp <- renderDataTable({
    
    groceryDatatable <- grocery_prices() %>%
      select(name, town, itemname, price) %>%
      group_by_at(vars(-price)) %>%
      mutate(row_id = 1:n()) %>%
      ungroup() %>%
      spread(key= itemname, value = price) %>%
      select(-row_id) 
    
    datatable(groceryDatatable,
              extensions = list("FixedColumns"),
              options = list(
                dom = "lftp",
                scrollX = TRUE,
                fixedColumns = list(leftColumns = 2))
              )
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
  
  ## Download Grocery Data
  output$grocery_data_download <- downloadHandler(
    filename = function(){
      paste0("CACGroceryData_", grocery_prices()$StartDate[1],".csv")
    },
    content = function(file) {
      write.csv(grocery_prices(),file,row.names=F)
    },
    contentType = "csv"
  )

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

