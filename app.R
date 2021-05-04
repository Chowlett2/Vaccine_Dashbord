library(shiny)
library(shinydashboard)
library(shinythemes)
library(readr)
library(dplyr)
library(lubridate)
library(tsibble)
library(plotly)
library(tidyverse)
library(viridis)
library(RColorBrewer)

pfizer <- read_csv("data/COVID-19_Vaccine_Distribution_Allocations_by_Jurisdiction_-_Pfizer.csv")
janssen <- read_csv("data/COVID-19_Vaccine_Distribution_Allocations_by_Jurisdiction_-_Janssen.csv")
moderna <- read_csv("data/COVID-19_Vaccine_Distribution_Allocations_by_Jurisdiction_-_Moderna.csv")
populations <- read_csv("data/State_Populations.csv")
#
pfizer$Code <- state.abb[match(pfizer$Jurisdiction,state.name)]
pfizer$Pop <- as.vector(populations[match(pfizer$Jurisdiction, populations$State), 2]$`2018 Population`)
pfizer$AllocationsPer1k <- pfizer$`1st Dose Allocations`/pfizer$Pop *1000
pfizer$`Week of Allocations` <- gsub('/', '-', pfizer$`Week of Allocations`)
#
janssen$Code <- state.abb[match(janssen$Jurisdiction,state.name)]
janssen$Pop <- as.vector(populations[match(janssen$Jurisdiction, populations$State), 2]$`2018 Population`)
janssen$AllocationsPer1k <- janssen$`1st Dose Allocations`/janssen$Pop *1000
janssen$`Week of Allocations` <- gsub('/', '-', janssen$`Week of Allocations`)
#
moderna$Code <- state.abb[match(moderna$Jurisdiction,state.name)]
moderna$Pop <- as.vector(populations[match(moderna$Jurisdiction, populations$State), 2]$`2018 Population`)
moderna$AllocationsPer1k <- moderna$`1st Dose Allocations`/moderna$Pop *1000
moderna$`Week of Allocations` <- gsub('/', '-', moderna$`Week of Allocations`)
#
new <- merge(moderna, janssen, by=c("Jurisdiction","Week of Allocations","Code","Pop"), all=T)
all_brands <- merge(new, pfizer, by=c("Jurisdiction","Week of Allocations","Code","Pop"), all=T)
all_brands[is.na(all_brands)] <- 0
all_brands$total <- all_brands$AllocationsPer1k.y + all_brands$AllocationsPer1k.x + all_brands$AllocationsPer1k
all_brands$`Week of Allocations` <- mdy(all_brands$`Week of Allocations`)

ui <- navbarPage('COVID-19 App',
          
          theme = shinytheme("flatly"),
          
          tabPanel('Vaccine Prevalence',
                   fluidPage(
                       titlePanel(
                           h1(
                             'Number of Vaccinated Residents',
                             align='center'
                           )
                       ),
                     
                       sidebarLayout(
                           sidebarPanel(
                               selectInput(
                                   "location", 
                                   "Select a State:",
                                   choices = "Alabama"
                               )
                           ),
                       
                           mainPanel(
                               box(
                                   plotlyOutput("plot1"), 
                                   width = 12
                               )
                           )   
                       ),
                     
                       titlePanel(
                           h1(
                               'Percent of Residents Vaccinated',
                               align='center'
                           )
                       ),
                     
                       box(
                           plotlyOutput("prevPlot"), 
                           width = 12
                       ),
                       "Source: https://data.cdc.gov/Vaccinations/COVID-19-County-Hesitancy/c4bi-8ytd"
                   )
          ),
          
          tabPanel('Vaccine Allocation',
                   fluidPage(
                       titlePanel(
                           h1(
                               "State Vaccine Allocations",
                               align='center'
                           )
          
                       ),
                       
                       titlePanel(
                         h3(
                           "(per 1,000 residents)",
                           align='center'
                         )
                         
                       ),
                       
                       sidebarLayout(
                           sidebarPanel(
                               dateRangeInput(
                                   "dateChoice", 
                                   "Filter by Date Range:", 
                                   start = min(all_brands$`Week of Allocations`), 
                                   end = max(all_brands$`Week of Allocations`), 
                                   min = min(all_brands$`Week of Allocations`),
                                   max = max(all_brands$`Week of Allocations`)
                               )
                           ),
                           
                           mainPanel(
                               box(
                                   plotlyOutput("allocationPlot"),
                                   width = 12
                               ),
                               "Source: https://data.cdc.gov/browse?q=COVID-19+vaccine&sortBy=relevance&page=1"
                          )
                       )
                   )
          ),
          
          tabPanel('Vaccine Hesitancy',
              fluidPage(
                  titlePanel(
                      h1(
                          'Vaccine Hesitancy',
                          align='center'
                      )
                  ),
                       
                  box(
                      plotlyOutput("plot2"), 
                      width = 12
                  ),
                  
                  "Source: https://github.com/tonmcg/US_County_Level_Election_Results_08-20",
                  
                  titlePanel(
                       h1(
                         'Vaccine Hesitancy Map',
                         align='center'
                       ),
                  ),
                   box(
                       plotlyOutput("hesPlot"), 
                       width = 12
                   ),
                  "Source: https://aspe.hhs.gov/pdf-report/vaccine-hesitancy"
              )
          )
      )

server <- function(input, output, session) {
    
    ############################################################################
    # DATA INGESTION                                                           #
    ############################################################################
    
    # Plot Data
    
    covid.data <- reactiveFileReader(
        intervalMillis = 1e+10,
        session = session,
        filePath = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv",
        readFunc = read_csv
    )
    
    covid.data.edit <- reactive({covid.data() %>% 
        filter(!(location %in% c("Bureau of Prisons", "Dept of Defense", 
                                 "Federated States of Micronesia", "Indian Health Svc", 
                                 "Long Term Care", "Marshall Islands","Northern Mariana Islands",
                                 "Republic of Palau", "Veterans Health", "Virgin Islands")))})
    
    statedata <- eventReactive(input$location, {
        mystate <- input$location
        df <- covid.data.edit() %>%
            filter(location==mystate) %>%
            as_tsibble(index=date) %>%
            fill_gaps()
    })
    
    plot2Data <- reactiveFileReader(
        intervalMillis = 1e+10,
        session = session,
        filePath = "data/plotdata.csv",
        readFunc = read_csv
    )
    
    ############################################################################
    # PLOT / MAP OBJECTS                                                       #
    ############################################################################
    
    observe({
        updateSelectInput(session, "location",
                          choices = sort(unique(req(covid.data.edit())$location)))
    })
    
    output$allocationPlot<- renderPlotly({
        
        begin <- input$dateChoice[1] - 1 #need the cumulative count the day _before_
        end <- input$dateChoice[2]
        df2 <- all_brands %>%
            filter(`Week of Allocations` <=  end & `Week of Allocations` > begin) %>%
            as_tibble() %>%
            group_by(Code, Jurisdiction, Pop) %>%
            summarize(total = sum(total, na.rm=T), 
                      `AllocationsPer1k.x` = sum(`AllocationsPer1k.x`, na.rm=T),
                      `AllocationsPer1k.y` = sum(`AllocationsPer1k.y`, na.rm=T),
                      `AllocationsPer1k` = sum(`AllocationsPer1k`, na.rm=T))
        
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = TRUE,
            lakecolor = toRGB('white')
        )
        
        fig <- plot_ly(data = df2, type="choropleth", locationmode = 'USA-states', 
                       z = ~`total`, locations = ~Code,
                       text = paste0("<b>State:</b> ", df2$Jurisdiction, "<br>",
                                     "<b>Population:</b> ", round(df2$Pop/1000000, 2), "M<br><br>",
                                     "<b>Allocations Per 1k</b> <br><b>Total: </b>", round(df2$`total`, 2),
                                     "<br><b>Moderna: </b>", round(df2$`AllocationsPer1k.x`, 2),
                                     "<br><b>Pfizer: </b>", round(df2$`AllocationsPer1k`, 2),
                                     "<br><b>J&J: </b>", round(df2$`AllocationsPer1k.y`, 2), "<br>"),
                       hoverinfo = 'text', zmin = 0, zmax = 600) %>% 
            layout(geo = g) %>%
            config(displayModeBar = F)
    })
    
    df <- read_csv('data/Vaccine_Hesitancy_for_COVID-19__County_and_local_estimates (1).csv')
    
    df$fips <- as.character(df$`FIPS Code`)
    
    df$fips <- ifelse(nchar(df$fips) < 5, paste0('0',df$fips), df$fips)
    
    url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
    counties <- rjson::fromJSON(file=url)
    
    output$hesPlot <- renderPlotly({
      fig <- plot_ly(data = df,
                     text = paste0(
                       "County: ", df$`County Name`, "<br>",
                       "Percent Hesitant: ", df$`Estimated hesitant`*100,"%<br>"),
                     hoverinfo = 'text')
      
      fig <- fig %>% add_trace(
        type="choroplethmapbox",
        geojson=counties,
        locations=df$fips,
        z=df$`Estimated hesitant`,
        colorscale="Viridis",
        marker=list(line=list(
          width=0),
          opacity=0.5
        )
      )
      
      fig <- fig %>% layout(
        mapbox=list(
          style="carto-positron",
          zoom =2,
          center=list(lon= -95.71, lat=37.09)),
        title='Vaccine Hesitancy by County'
      ) %>%
        config(displayModeBar = F)
    
    })
    
    output$prevPlot <- renderPlotly({
      fig2 <- plot_ly(data = df,
                      text = paste0(
                        "County: ", df$`County Name`, "<br>",
                        "Adults Fully Vaccinated: ", df$`Percent adults fully vaccinated against COVID-19`*100,"%<br>"),
                      hoverinfo = 'text')
      
      fig2 <- fig2 %>% add_trace(
        type="choroplethmapbox",
        geojson=counties,
        locations=df$fips,
        z=df$`Percent adults fully vaccinated against COVID-19`,
        colorscale="Viridis",
        marker=list(line=list(
          width=0),
          opacity=0.5
        )
      )
      
      fig2 <- fig2 %>% layout(
        mapbox=list(
          style="carto-positron",
          zoom =2,
          center=list(lon= -95.71, lat=37.09))
      ) %>%
        config(displayModeBar = F)
      
      
    })
    
  
    output$plot1 <- renderPlotly({
        
        statedata() %>% na.omit(statedata()) %>%  
            plot_ly(
                type="scatter",
                mode="lines",
                x = ~date,
                y = ~people_vaccinated,
                name = "At Least 1 Dose",
                fill = "tozeroy",
                fillcolor = "#97F3AD",
                line = list(color = "#000000")
                
            ) %>% 
            add_lines( x=~date, 
                       y=~people_fully_vaccinated, 
                       mode = "lines", 
                       name="Fully Vaccinated",
                       fill = "tozeroy",
                       fillcolor = "#8CCF83") %>%
            layout(
                xaxis = list(title="Date", showgrid=FALSE),
                yaxis = list(title="Vaccinations"),
                hovermode = "x unified",
                legend = list(orientation = 'h')
            ) %>%
            config(displayModeBar = F)
        
    })
    
    output$plot2 <- renderPlotly({
        
        plot2Data() %>%
            plot_ly(
                type = "scatter",
                x = ~per_diff,
                y = ~hesis,
                color = ~per_diff,
                colors = c("blue","grey50", "red"),
                mode = "markers",
                marker = (list(size = 15,              
                               line = list(width = 1,
                                           color = '#000000'))),
                text = ~paste('State: ', state_name),
                hoverinfo = 'text'
            ) %>%
            layout(xaxis = list(title = "Democratic Votes               Republican Votes",zeroline=FALSE),
                   yaxis = list(title = "Vaccine Hesitancy among States (%)"),
                   title = "Vaccine Hesitancy (%) vs. 2020 Presidential vote proportion per State") %>%
            hide_colorbar() %>%
            config(displayModeBar = F)
    })

}

shinyApp(ui = ui, server = server)