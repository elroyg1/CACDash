
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
              selectInput("grocery_date",
                          "Choose a date",
                          grocery_dates),
              selectizeInput("grocery_products",
                          "Choose a product",
                          grocery_products
                          ),
              textInput("grocery_product_vol",
                        "Enter the product volume"),
              actionButton("grocery_add","Add"),
              downloadButton("grocery_data_download", "Download")
            )
          ),
          column(
            width = 8,
            box(
              width = NULL,
              leafletOutput("groceryMapPlot")))
        ),
        fluidRow(
          column(
            width = 6,
            box(
              width = NULL,
              title = "Store Comparison",
              tags$div(id="storebills")
            )
          ),
          column(
            width = 6,
            tabBox(
              width = NULL,
              title = "Product Analysis",
              tabPanel("Stats",
                       tags$div(id="groceryProductStats")),
              tabPanel("Chart",
                       plotlyOutput("groceryChart"))
            )
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
        h1("COMING SOON!")
      )
      )
      )
  )

# Define server logic required to draw a histogram
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
  
  #Prices app
  observeEvent(input$view_Petrol_Prices,{
    
    #Generate reactive data for mapPlot
    map_data <- reactive({
      map_data <- petrol_data %>%
        filter(year(StartDate) == year(input$date) &
                 month(StartDate) == month(input$date),
               ItemName == input$fuel,
               Price > 0)
      return(map_data)
    })
    
    #Map Creation
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
    
    # Zoom in on user location if given
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
      
      wtiDPB <- as.numeric(wti$Price[wti$Date == input$date]) / 158.987 * as.numeric(as.character(XRate_data$`USD Sell`[XRate_data$Date == input$date]))
      
      infoBox(
        title = "WTI (crude oil)",
        subtitle = paste0("(US$",as.numeric(wti$Price[wti$Date == input$date]), "/bbl)"),
        href = "https://www.eia.gov/dnav/pet/pet_pri_spt_s1_d.htm",
        value =  round(wtiDPB, 2),
        icon = icon("usd")
      )
      
    })
    
    output$USGC <- renderInfoBox({
      
      usgcDPG <- ifelse(input$fuel %in% c("E-10 GASOLENE - 87 OCTANE NONE 1 L",
                                          "E-10 GASOLENE - 90 OCTANE NONE 1 L",
                                          "AUTO DIESEL NONE 1 L"),
                        as.numeric(usgcReg$Price[usgcReg$Date == input$date]),
                        as.numeric(usgcULSD$Price[usgcULSD$Date == input$date]))
      
      infoBox(
        title = "USGC",
        subtitle = paste0("(US$",usgcDPG,"/gal)"),
        value = round(usgcDPG / 3.785 *  as.numeric(as.character(XRate_data$`USD Sell`[XRate_data$Date == input$date])),2),
        href = "https://www.eia.gov/dnav/pet/pet_pri_spt_s1_d.htm",
        icon = icon("usd")
      )
    })
    
    output$petrojam <- renderInfoBox({
      
      petrEXP <- ifelse(input$fuel == "E-10 GASOLENE - 87 OCTANE NONE 1 L",
                        petrojam_data$`Gasolene  87`[year(petrojam_data$Date)==year(input$date) &
                                                       week(petrojam_data$Date) == week(input$date)-1],
                        ifelse(input$fuel == "E-10 GASOLENE - 90 OCTANE NONE 1 L",
                               petrojam_data$`Gasolene 90`[year(petrojam_data$Date)==year(input$date) &
                                                             week(petrojam_data$Date) == week(input$date)-1],
                               ifelse(input$fuel == "AUTO DIESEL NONE 1 L",
                                      petrojam_data$`Auto Diesel`[year(petrojam_data$Date)==year(input$date) &
                                                                    week(petrojam_data$Date) == week(input$date)-1],
                                      petrojam_data$ULSD[year(petrojam_data$Date)==year(input$date) &
                                                           week(petrojam_data$Date) == week(input$date)-1]
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
      
      #Chart analysis
      output$chart <- renderPlotly({
        
        petrol_data %>%
          filter(StartDate <= input$date,
                 ItemName == input$fuel,
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
                   ItemName == input$fuel)
        
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
                             input$fuel, " price"),
               y = "J$/L")
      })
      
      output$markup <- renderInfoBox({
        
        markedup <- ifelse(input$fuel == "E-10 GASOLENE - 87 OCTANE NONE 1 L",
                           map_data()$Price[map_data()$MerchantID == clickedMarker$id] - petrojam_data$`Gasolene  87`[year(petrojam_data$Date)==year(input$date) &
                                                                                                                        week(petrojam_data$Date) == week(input$date)-1],
                           ifelse(input$fuel == "E-10 GASOLENE - 90 OCTANE NONE 1 L",
                                  map_data()$Price[map_data()$MerchantID == clickedMarker$id] - petrojam_data$`Gasolene 90`[year(petrojam_data$Date)==year(input$date) &
                                                                                                                              week(petrojam_data$Date) == week(input$date)-1],
                                  ifelse(input$fuel == "AUTO DIESEL NONE 1 L",
                                         map_data()$Price[map_data()$MerchantID == clickedMarker$id] - petrojam_data$`Auto Diesel`[year(petrojam_data$Date)==year(input$date) &
                                                                                                                                     week(petrojam_data$Date) == week(input$date)-1],
                                         map_data()$Price[map_data()$MerchantID == clickedMarker$id] - petrojam_data$ULSD[year(petrojam_data$Date)==year(input$date) &
                                                                                                                            week(petrojam_data$Date) == week(input$date)-1]
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
    
    #Table Tab output
    output$table <- renderDataTable({
      map_data() %>%
        select(MerchantName, MerchantTown, Parish, Price) %>%
        group_by(MerchantTown) %>%
        formattable() %>%
        as.datatable()
    })
    
    output$downloadPetrolData<- downloadHandler(
      filename = paste0("CACPetrolData_", map_data()$StartDate,".csv"),
      content = function(file) {
        write.csv(map_data(),file,row.names=F)
      },
      contentType = "csv"
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

