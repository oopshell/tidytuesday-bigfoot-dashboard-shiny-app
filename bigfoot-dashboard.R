install.packages("shiny")
install.packages("tidytuesdayR")
install.packages("maptools")
install.packages("rgdal")
install.packages("ggplot2")
install.packages("plotly")
install.packages("mapproj")
install.packages("leaflet")
install.packages("lubridate")
install.packages("classInt")
install.packages("RColorBrewer")
install.packages("tidytext")
install.packages("wordcloud")


library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(tidytext)
library(wordcloud)



tuesdata <- tidytuesdayR::tt_load(2022, week = 37)
bigfoot <- tuesdata$bigfoot

set.seed(90007) # for reproducibility


# bigfoot <- read.csv("bigfoot.csv")
# bigfoot <- bigfoot %>%
#   mutate(date = ymd(date))




##################
# USER INTERFACE #
##################
# 1. Tab 1 - Background (intro) tab
intro_tab <- tabPanel(
  "Background",
  
  HTML('<center>
       <h1 style="ffont-family: "Times New Roman", Times, serif; font-size:42px">
       Bigfoot Sightings in the North America
       <img src="https://m.media-amazon.com/images/I/61neeJlAymL._AC_SX679_.jpg" alt="logo" width="40" /> 
       
       </h1>
       </center>'),
  

  HTML('<p align="justify" style="ffont-family: "Times New Roman", Times, serif; font-size:50px">
       Bigfoot, also commonly referred to as Sasquatch, is a purported ape-like 
       creature said to inhabit the forests of North America. Many dubious 
       articles have been offered in attempts to prove the existence of Bigfoot, 
       including anecdotal claims of visual observations as well as alleged 
       video and audio recordings, photographs, and casts of large footprints. 
       Some are known or admitted hoaxes. Tales of wild, hairy humanoids exist 
       throughout the world, and such creatures appear in the folklore of North 
       America, including the mythologies of indigenous people. Bigfoot is an 
       icon within the fringe subculture of cryptozoology, and an enduring 
       element of popular culture.
       </p>
       <p>
       Retrieved from: 
       <a href="https://en.wikipedia.org/w/index.php?title=Bigfoot&oldid=1111497352">
       Wikipedia-Bigfoot
       </a>
       </p>'),

  br(),
  HTML('<center>
       <img src="https://news.rochesternh.gov/wp-content/uploads/2022/08/bigfoot-film-patterson-gimlin.webp" width="256">
       </center>'),
  HTML('<center>
       <p>
       Source: 
       <a href="https://news.rochesternh.gov/mysterious-family-of-bigfoot-spotted-in-downtown-rochester">
       Mysterious Family of Bigfoot Spotted in Downtown Rochester
       </a>
       </p>
       </center>'),
  
  # br(),
  HTML('<center>
       <img src="https://traveloregon.com/wp-content/uploads/2019/07/2019_bigfoot-center_slide_01.jpg" width="256">
       </center>'),
  HTML('<center>
       <p>
       Source: 
       <a href="https://traveloregon.com/things-to-do/trip-ideas/favorite-trips/bigfoot-lovers-guide-to-oregon/">
       Bigfoot Lover’s Guide To Oregon
       </a>
       </p>
       </center>'),
  
)




# 2. Tab 2 - Bigfoot Sightings Distribution

distr_tab <- tabPanel(
  
  "Sighting Distribution",
  
  titlePanel("Bigfoot Sightings Distribution"),
  
  br(),
  
  fluidRow(
    column(5, offset = 1,
           leafletOutput("map_bigfoot_sightings", height = 390, width = 520),
    ),
    column(5, offset = 0,
           plotlyOutput("plot_year", height = 390, width = 520),
    ),
  ),
  
  br(),
  hr(),
  
  fluidRow(
    column(2, offset = 4,
           checkboxGroupInput(
             "selected_class_d",
             label = "Report Classification",
             choices = c("Class A", "Class B", "Class C"),
             selected = c("Class A", "Class B", "Class C")),
    ),
    column(3, offset = 0,
           sliderInput(
             "selected_year_range",
             label = "Year Range",
             min = 1950,
             max = 2021,
             value = c(1950, 2021),
             step = 1,
             sep = ""),
    ),
  ),
  
)




# 3. Tab 3 - Sighting Statistics

season_month_tab <- tabPanel(
  
  "Sighting Statistics",
  
  titlePanel("Sighting Statistics Based on Season and Month"),
  
  sidebarLayout(
    
    # Component 1 - side bar
    sidebarPanel(
      selectInput(
        "selected_state",
        label = "State",
        choices = c("All States", sort(unique(bigfoot$state))),
        selected = "All States"
      ),
      
      hr(),
      
      sliderInput(
        "selected_temperature_range", 
        label = "Temperature Range",
        min = -9, 
        max = 95,
        value = c(-9, 95),
        round = FALSE,
        sep = ""),
      
      sliderInput(
        "selected_humidity_range", 
        label = "Humidity Range",
        min = 0, 
        max = 1,
        value = c(0, 1),
        round = FALSE,
        sep = ""),
      
      sliderInput(
        "selected_moon_phase_range", 
        label = "Moon Phase Range",
        min = 0, 
        max = 1,
        value = c(0, 1),
        round = FALSE,
        sep = ""),
      
    ), 
    
    # Component 2 - content panel
    mainPanel(
      tabsetPanel(
        tabPanel("Season", 
                 br(),
                 plotlyOutput("plot_season", height = 390, width = 520)), 
        tabPanel("Month", 
                 br(),
                 plotlyOutput("plot_month", height = 390, width = 520)), 
      )
    )
  )
)





# 4. Tab 4 - Climate Factors

moon_tab <- tabPanel(
  
  "Climate Factors",
  
  titlePanel("Climate Related Factors"),
  
  sidebarLayout(
    
    # Component 1 - side bar
    sidebarPanel(
      sliderInput(
        "selected_year_range_m", 
        label = "Year Range",
        min = 1950, 
        max = 2021,
        value = c(1950, 2021),
        step = 1,
        sep = ""),
      
      hr(),
      
      radioButtons(
        "selected_season_m",
        label = "Season",
        choices = c("All Year", "Spring", "Summer", "Fall", "Winter"),
        selected = "All Year"
      ),
    ),
    
    
    # Component 2 - content panel
    mainPanel(
      
      tabsetPanel(
        tabPanel("MoonPhase", 
                 br(),
                 # h3("Sightings number according to moon phase"),
                 plotlyOutput("plot_moon", height = 390, width = 520)), 
        tabPanel("Temperature-Low", 
                 br(),
                 # h3("Sightings number according to temperature (low)"),
                 plotlyOutput("plot_temperature_low", height = 390, width = 520)),
        tabPanel("Temperature-High", 
                 br(),
                 # h3("Sightings number according to temperature (high)"),
                 plotlyOutput("plot_temperature_high", height = 390, width = 520)),
        tabPanel("Precipitation", 
                 br(),
                 # h3("Sightings number according to temperature (high)"),
                 plotlyOutput("plot_precipitation", height = 390, width = 520)),
      )
    )
  )
)



# 5. Tab 5 - Frequent Words

word_tab <- tabPanel(
  
  "Frequent Words",
  
  titlePanel("Most Frequent Words in Reports"),
  
  sidebarLayout(
    
    # Component 1 - side bar
    sidebarPanel(

      radioButtons(
        "selected_class_w",
        label = "Report Classification",
        choices = c("Class A", "Class B", "Class C"),
        selected = "Class A"
      ),
      hr(),
      sliderInput("selected_word_num",
                  label = "Maximum Number of Words",
                  min = 10,  max = 120,  value = 120),
      br(),
      hr(),
      br(),
      radioButtons(
        "selected_season_w",
        label = "Season",
        choices = c("All Year", "Spring", "Summer", "Fall", "Winter"),
        selected = "All Year"
      ),
    ),
    
    # Component 2 - content panel
    mainPanel(
      plotOutput("plot_word", height = 500, width = 520),
    )
    
  )
  
)




# 6. Overall Shiny app UI

ui <- navbarPage(
  "Bigfoot",
  intro_tab,
  distr_tab,
  season_month_tab,
  moon_tab,
  word_tab
)




################
# SHINY SERVER #
################

# Server function
server <- function(input, output, session) {
  
  ###########
  #  Tab 2  #
  ###########
  
  # Data filter
  filteredData <- reactive({
    bigfoot %>%
      mutate(year = year(date)) %>%
      filter(!is.na(year)) %>%
      filter(between(longitude, -130, -60)) %>%  # some sightings outside the mainland
      filter(state != "Alaska") %>%
      filter(between(year, input$selected_year_range[1], input$selected_year_range[2])) %>%
      filter(
        if (length(input$selected_class_d) == 3) TRUE 
        else classification %in% input$selected_class_d
      )
  })
  
  # Load icon
  bigfootIcon <- makeIcon(
    iconUrl = "icon/bigfoot.jpg",
    iconWidth = 20, iconHeight = 20,
  )
  
  # Sightings map
  output$map_bigfoot_sightings <- renderLeaflet({
    leaflet(filteredData()) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, options = providerTileOptions(opacity = 0.8)) %>% 
      addMarkers(~longitude, ~latitude, 
                 icon = bigfootIcon,
                 popup = ~title, #~popup,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 75, 
                                                       iconCreateFunction = JS("function (cluster){    
                                                          var childCount = cluster.getChildCount(); 
                                                          var c = ' marker-cluster-';  
                                                          if (childCount < 10) {  
                                                            c += 'small';  
                                                          } else if (childCount < 100) {  
                                                            c += 'medium';  
                                                          } else { 
                                                            c += 'large';  
                                                          }    
                                                          return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(0, 0) });
                                                       }"))) %>%
      addCircles(lng = ~longitude, lat = ~latitude, 
                 weight = 0.5, color = c('#7B68EE'))
    
  })
  
  output$plot_year <- renderPlotly({
    p <- bigfoot %>%
      mutate(year = year(date)) %>%
      filter(!is.na(year)) %>%
      filter(between(longitude, -130, -60)) %>%  # some sightings outside the mainland
      filter(state != "Alaska") %>%
      filter(between(year, input$selected_year_range[1], input$selected_year_range[2])) %>%
      filter(
        if (length(input$selected_class_d) == 3) TRUE 
        else classification %in% input$selected_class_d
      ) %>%
      count(year) %>%
      ggplot(aes(x = year, y = n)) +  # 5 * year %/% 5
      geom_line(linetype = "soild", color = '#FFD700') +
      geom_point(size=0.8, color='#FF8C00') +
      ggtitle("Sighting Number by Year") +
      xlab("") +
      ylab("") +
      theme(axis.text = element_text(size=10),
            axis.title = element_text(size=12),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(size=12, hjust = 0.5))
 
    ggplotly(p, source="plot_year")
  })
  
  
  
  
  ###########
  #  Tab 3  #
  ###########
  
  values1 <- reactiveValues()
  
  # Sightings by season
  output$plot_season <- renderPlotly({
    p <- bigfoot %>%
      mutate(season = na_if(season, "Unknown")) %>%
      filter(!is.na(season)) %>%
      filter(case_when(input$selected_state == "All States" ~ TRUE,
                       TRUE ~ state == input$selected_state)) %>%
      filter(between(temperature_mid, 
                     input$selected_temperature_range[1], 
                     input$selected_temperature_range[2])) %>%
      filter(between(humidity, 
                     input$selected_humidity_range[1], 
                     input$selected_humidity_range[2])) %>%
      filter(between(moon_phase, 
                     input$selected_moon_phase_range[1], 
                     input$selected_moon_phase_range[2])) %>%

      ggplot(aes(season, fill = classification)) +
      geom_bar(width = 0.55) +
      ggtitle("Sighting Number by Season") +
      scale_fill_brewer(palette="Set2") + 
      xlab("") + 
      ylab("") + 
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10), expand = c(0.2,0)) + 
      scale_y_continuous(labels = function(y) format(y, big.mark=","), expand = c(0,0)) +
      theme(panel.background = element_rect(fill="white"),
            axis.text = element_text(size=10),
            axis.title = element_text(size=12),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(size=12, hjust = 0.5))
    
    values1$loaded <- TRUE
    ggplotly(p, source="plot_season")
  })
  
  
  values2 <- reactiveValues()
  
  # Sightings by month
  output$plot_month <- renderPlotly({
    p <- bigfoot %>%
      mutate(month = month(date, label = TRUE)) %>%
      filter(!is.na(month)) %>%
      filter(case_when(input$selected_state == "All States" ~ TRUE,
                       TRUE ~ state == input$selected_state)) %>%
      filter(between(temperature_mid, 
                     input$selected_temperature_range[1], 
                     input$selected_temperature_range[2])) %>%
      filter(between(humidity, 
                     input$selected_humidity_range[1], 
                     input$selected_humidity_range[2])) %>%
      filter(between(moon_phase, 
                     input$selected_moon_phase_range[1], 
                     input$selected_moon_phase_range[2])) %>%

      ggplot(aes(month, fill = classification)) +
      geom_bar() +
      ggtitle("Sighting Number by Month") +
      scale_fill_brewer(palette="Set2") + 
      xlab("") + 
      ylab("") +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10), expand = c(0.1,0)) + 
      scale_y_continuous(labels = function(y) format(y, big.mark=","), expand = c(0,0)) +
      theme(panel.background = element_rect(fill="white"),
            axis.text = element_text(size=10),
            axis.title = element_text(size=12),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(size=12, hjust = 0.5))
    
    
    values2$loaded <- TRUE
    ggplotly(p, source="plot_month")
  })
  
  # Observes events
  observe({
    req(values1$loaded)
    req(values2$loaded)
    d <- event_data("plotly_click", source="plot_season")
    if (is.null(d)) return()
    stateName <- c("All States", sort(unique(bigfoot$state)))[d$pointNumber + 1]
    updateSelectInput(session, "selected_state", selected=stateName)
  })
  
  
  
  
  ###########
  #  Tab 4  #
  ###########
  
  # Sightings based on moon phase
  output$plot_moon <- renderPlotly({
    p <- bigfoot %>%
      mutate(year = year(date)) %>%
      filter(!is.na(year)) %>%
      filter(between(year, input$selected_year_range_m[1], input$selected_year_range_m[2])) %>%
      filter(!is.na(classification)) %>%
      filter(!is.na(moon_phase)) %>%
      filter(
        if (input$selected_season_m == "All Year") TRUE 
        else season == input$selected_season_m
      ) %>%
      ggplot(aes(x = classification, y = moon_phase, fill = classification)) +
      geom_boxplot() +
      ggtitle("Sighting Number According to Moon Phase") +
      scale_fill_brewer(palette="Set2") +
      xlab("") + 
      ylab("") +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10), expand = c(0,0)) + 
      scale_y_continuous(labels = function(y) format(y, big.mark=","), expand = c(0,0)) +
      theme(axis.text = element_text(size=10),
            axis.title = element_text(size=12),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(size=12, hjust = 0.5))
    
    ggplotly(p, source="plot_moon")
  })
  
  # Sightings based on temperature (low)
  output$plot_temperature_low <- renderPlotly({
    p <- bigfoot %>%
      mutate(year = year(date)) %>%
      filter(!is.na(year)) %>%
      filter(between(year, input$selected_year_range_m[1], input$selected_year_range_m[2])) %>%
      filter(!is.na(classification)) %>%
      filter(!is.na(temperature_low)) %>%
      filter(
        if (input$selected_season_m == "All Year") TRUE 
        else season == input$selected_season_m
      ) %>%
      ggplot(aes(x = classification, y = temperature_low, fill = classification)) +
      geom_boxplot() +
      ggtitle("Sighting Number According to Temperature-Low") +
      scale_fill_brewer(palette="Set2") +
      xlab("") + 
      ylab("") +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10), expand = c(0,0)) + 
      scale_y_continuous(labels = function(y) format(y, big.mark=","), expand = c(0,0)) +
      theme(axis.text = element_text(size=10),
            axis.title = element_text(size=12),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(size=12, hjust = 0.5))
    
    ggplotly(p, source="plot_temperature_low")
  })
  
  # Sightings based on temperature (high)
  output$plot_temperature_high <- renderPlotly({
    p <- bigfoot %>%
      mutate(year = year(date)) %>%
      filter(!is.na(year)) %>%
      filter(between(year, input$selected_year_range_m[1], input$selected_year_range_m[2])) %>%
      filter(!is.na(classification)) %>%
      filter(!is.na(temperature_high)) %>%
      filter(
        if (input$selected_season_m == "All Year") TRUE 
        else season == input$selected_season_m
      ) %>%
      ggplot(aes(x = classification, y = temperature_high, fill = classification)) +
      geom_boxplot() +
      ggtitle("Sighting Number According to Temperature-High") +
      scale_fill_brewer(palette="Set2") +
      xlab("") + 
      ylab("") +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10), expand = c(0,0)) + 
      scale_y_continuous(labels = function(y) format(y, big.mark=","), expand = c(0,0)) +
      theme(axis.text = element_text(size=10),
            axis.title = element_text(size=12),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(size=12, hjust = 0.5))
    
    ggplotly(p, source="plot_temperature_high")
  })
  
  # Sightings based on the probability of precipitation
  output$plot_precipitation <- renderPlotly({
    p <- bigfoot %>%
      mutate(year = year(date)) %>%
      filter(!is.na(year)) %>%
      filter(between(year, input$selected_year_range_m[1], input$selected_year_range_m[2])) %>%
      filter(!is.na(classification)) %>%
      filter(!is.na(precip_probability)) %>%
      filter(
        if (input$selected_season_m == "All Year") TRUE 
        else season == input$selected_season_m
      ) %>%
      ggplot(aes(x = classification, y = precip_probability, fill = classification)) +
      geom_boxplot() +
      ggtitle("Sighting Number According to the Probability of Precipitation") +
      scale_fill_brewer(palette="Set2") +
      xlab("") + 
      ylab("") +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10), expand = c(0,0)) + 
      scale_y_continuous(labels = function(y) format(y, big.mark=","), expand = c(0,0)) +
      theme(axis.text = element_text(size=10),
            axis.title = element_text(size=12),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(size=10, hjust = 0.5))
    
    ggplotly(p, source="plot_precipitation")
  })
  

  
  
  
  
  
  
  
  
  
  ###########
  #  Tab 5  #
  ###########
  
  # Clean text data
  
  # Get the text column
  text <- bigfoot$observed
  # Set the text to lowercase
  text <- tolower(text)
  # Remove mentions, urls, emojis, numbers, punctuations, etc.
  text <- gsub("@\\w+", "", text)
  text <- gsub("https?://.+", "", text)
  text <- gsub("\\d+\\w*\\d*", "", text)
  text <- gsub("#\\w+", "", text)
  text <- gsub("[^\x01-\x7F]", "", text)
  text <- gsub("[[:punct:]]", " ", text)
  # Remove spaces and newlines
  text <- gsub("\n", " ", text)
  text <- gsub("^\\s+", "", text)
  text <- gsub("\\s+$", "", text)
  text <- gsub("[ |\t]+", " ", text)
  # Put the data to a new column
  bigfoot["text"] <- text
  
  
  # Data filter
  filteredData_word <- reactive({
    word_count <- bigfoot %>%
      filter(
        if (input$selected_season_w == "All Year") TRUE 
        else season == input$selected_season_w
      ) %>%
      filter(classification == input$selected_class_w) %>%
      unnest_tokens(word, text) %>%
      filter(!is.na(word)) %>%
      count(word, sort = TRUE) %>%
      anti_join(stop_words, by="word")
    
    head(word_count, 200)
  })
  
  
  # Word cloud
  output$plot_word <- renderPlot({
    wordcloud(words = filteredData_word()$word,
              freq = filteredData_word()$n,
              min.freq = 1,
              max.words = input$selected_word_num,
              random.order = FALSE,
              scale=c(3.5, 0.25),
              colors = brewer.pal(8, "Dark2"))
    
  })
  
}





#############
# RUN SHINY #
#############


shinyApp(ui = ui, server = server)


