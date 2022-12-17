# Libraries
library(mapsapi)
library(leaflet)

library(shiny)
library(dplyr)
library(DT)
library(RColorBrewer)
library(magrittr)
library(tidyverse)

#open the cleaned dataset after part1 EDA
data_1 <- read_csv("data.csv")

data_1 %>% 
  mutate(route_type = case_when(str_detect(stop_desc, "Commuter Rail") ~ "Commuter Rail",
                                str_detect(stop_desc, "Red Line") ~ "Red Line",
                                str_detect(stop_desc, "Orange Line") ~ "Orange Line",
                                str_detect(stop_desc, "Busway") ~ "ExpressBus",
                                str_detect(stop_desc, "busway") ~ "ExpressBus",
                                str_detect(stop_desc, "Green Line") ~ "Green Line",
                                str_detect(stop_desc, "Blue Line") ~ "Blue Line",
                                str_detect(stop_desc, "Silver Line") ~ "Silver Line",
                                str_detect(stop_desc, "boat") ~ "Ferry",
                                TRUE ~ "others")) -> data_shiny

data_shiny %>% 
  filter(route_type == "Green Line") -> Green

data_shiny %>% 
  filter(route_type == "Red Line") -> Red

data_shiny %>% 
  filter(route_type == "Blue Line") -> Blue

data_shiny %>% 
  filter(route_type == "Orange Line") -> Orange

data_shiny %>% 
  filter(route_type == "Commuter Rail") -> Com

data_shiny %>% 
  filter(route_type == "ExpressBus") -> Bus

Red %>% 
  group_by(stop_lat) %>% 
  select(stop_lon, stop_name) %>% 
  unique() -> Red_1

Red_1[1:14, ] -> Red_1


Orange$stop_lat = round(Orange$stop_lat, 2)
Orange$stop_lon = round(Orange$stop_lon, 2)

Orange%>% 
  group_by(stop_lat) %>% 
  select(stop_lon, stop_name) %>% 
  unique() -> Orange_1

Blue%>% 
  group_by(stop_lat) %>% 
  select(stop_lon, stop_name) %>% 
  unique() -> Blue_1

Blue_1[1:12, ] -> Blue_1

Green%>% 
  group_by(stop_lat) %>% 
  select(stop_lon, stop_name) %>% 
  unique() -> Green_1

Green_1[1:40, ] -> Green_1

Com$stop_lat = round(Com$stop_lat, 2)
Com$stop_lon = round(Com$stop_lon, 2)
Com%>% 
  group_by(stop_lat) %>% 
  select(stop_lon, stop_name) %>% 
  unique() -> Com_1


Bus$stop_lat = round(Bus$stop_lat, 2)
Bus$stop_lon = round(Bus$stop_lon, 2)
Bus%>% 
  group_by(stop_lat) %>% 
  select(stop_lon, stop_name) %>% 
  unique() -> Bus_1

Bus_1[1:8, ] -> Bus_1

Origin <- c("Hynes Convention Center", "Chestnut Hill Avenue", "Ruggles", "Riverside", "Harvard", "Suffolk Downs")
Destination <- c("Boston University Central", "Boston University East", "Forest Hills", "Ashmont", "North Quincy", "Orient Heights")

# Your private key
key = "AIzaSyBcr7DhDu0MF1DXoY11_alhp9BfsYyyS14"
data_1 %>% 
  select(stop_name) %>% 
  unique() ->stop_shiny

stop_shiny <- stop_shiny[-2,]
stop_shiny <- stop_shiny[1:50, ]
stop_shiny_1 <- stop_shiny[46:1, ]

mode <- c("transit", "driving", "walking", "bicycling")
day <- c("Weekday", "Saturday", "Sunday")
holiday <- c("No", "Yes")
Holidays <- c("Christmas Eve", "Presidents' Day" ,  "Martin Luther King Day", "Christmas Day" )
time <- c("morning", "afternoon", "evening")

ui <-
  navbarPage("Massachusetts Bay Transportation Authority", collapsible = TRUE, inverse = TRUE,
             tabPanel("MBTA Route Map", leafletOutput("mbtamap", height=500)),
             tabPanel("Design your trip",
                      fluidPage(
                        fluidRow(
                          column(6, selectInput("Origin", "Specify your origin stop", Origin)),
                          column(6, selectInput("Destination", "Specify your destination stop", Destination)
                                 )
                        ),
                        fluidRow(
                          column(3,selectInput("Mode", "Specify your mode", mode)),
                          column(3,selectInput("Day", "Specify your travel day", day)),
                          column(3,selectInput("Holiday", "Leaving on holiday?", holiday)),
                          column(3,selectInput("time", "When are you leaving?", time))
                        ),
                        textOutput("reminder"),
                        textOutput("text"),
                        leafletOutput("tripmap",height=500)
                      )
             )
             
  )



server <- function(input, output) {
  
  output$text <- renderText({
    data_shiny %>%
      filter(stop_name == input$Origin) %>%
      arrange(trip_id) -> start
    
    data_shiny %>%
      filter(stop_name == input$Destination) %>%
      arrange(trip_id)-> end
    
    end %>% right_join(start, by = "trip_id")-> start_end
    
    
    route <- unique(start_end$route_type.y)
    
    if (input$Holiday == "No" && input$Day == "Weekday") {
      
      start_end %>%
        filter(service_schedule_type.x == "Weekday" & is.na(holiday_name.x)) -> start_end
      
      travel_time <- round(mean(abs(as.numeric(start_end$arrival_time.x - start_end$departure_time.y)), na.rm=TRUE)/60,2)
      
      paste("You can take: ", route,  " and your Travel time taking MBTA would be:",  travel_time, " minutes")
      
    }
    else if (input$Holiday == "No" && input$Day == "Saturday") {
      start_end %>%
        filter(service_schedule_type.x == "Saturday" & is.na(holiday_name.x)) -> start_end
      
      travel_time <- round(mean(abs(as.numeric(start_end$arrival_time.x - start_end$departure_time.y)), na.rm=TRUE)/60,2)
      
      paste("You can take: ", route,  " and your Travel time taking MBTA would be:",  travel_time, " minutes")
      
    }
    
    else if (input$Holiday == "No" && input$Day == "Sunday") {
      start_end %>%
        filter(service_schedule_type.x == "Sunday" & is.na(holiday_name.x)) -> start_end
      
      travel_time <- round(mean(abs(as.numeric(start_end$arrival_time.x - start_end$departure_time.y)), na.rm=TRUE)/60,2)
      
      paste("You can take: ", route,  " and your Travel time taking MBTA would be:",  travel_time, " minutes")
      
    }
    
    else if (input$Holiday == "Yes" && input$Day == "Sunday") {
      start_end %>%
        filter(service_schedule_type.x == "Sunday" & holiday_name.x %in% Holidays) -> start_end
      
      travel_time <- round(mean(abs(as.numeric(start_end$arrival_time.x - start_end$departure_time.y)), na.rm=TRUE)/60,2)
      
      paste("You can take: ", route,  " and your Travel time taking MBTA would be:",  travel_time, " minutes")
      
    }
    
    else if (input$Holiday == "Yes" && input$Day == "Saturday") {
      start_end %>%
        filter(service_schedule_type.x == "Saturday" & holiday_name.x %in% Holidays) -> start_end
      
      travel_time <- round(mean(abs(as.numeric(start_end$arrival_time.x - start_end$departure_time.y)), na.rm=TRUE)/60,2)
      
      paste("You can take: ", route,  " and your Travel time taking MBTA would be:",  travel_time, " minutes")
      
    }
    
    else if (input$Holiday == "Yes" && input$Day == "Weekday") {
      start_end %>%
        filter(service_schedule_type.x == "Weekday" & holiday_name.x %in% Holidays) -> start_end
      
      travel_time <- round(mean(abs(as.numeric(start_end$arrival_time.x - start_end$departure_time.y)), na.rm=TRUE)/60,2)
      
      paste("You can take: ", route,  " and your Travel time taking MBTA would be:",  travel_time, " minutes")
      
    }
    
    
  })
  
  output$mbtamap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -71.0589, lat = 42.3601, zoom = 11) %>%
      
      addPolylines(as.numeric(Red_1$stop_lon),as.numeric(Red_1$stop_lat),color="red") %>%
      addCircleMarkers(as.numeric(Red_1$stop_lon),as.numeric(Red_1$stop_lat),radius=1,popup = Red_1$stop_name) %>%
      
      addPolylines(as.numeric(Orange_1$stop_lon),as.numeric(Orange_1$stop_lat),color="orange") %>%
      addCircleMarkers(as.numeric(Orange_1$stop_lon),as.numeric(Orange_1$stop_lat),radius=1,popup = Orange_1$stop_name) %>%
      
      addPolylines(as.numeric(Blue_1$stop_lon),as.numeric(Blue_1$stop_lat),color="blue") %>%
      addCircleMarkers(as.numeric(Blue_1$stop_lon),as.numeric(Blue_1$stop_lat),radius=1,popup = Blue_1$stop_name) %>%
      
      addPolylines(as.numeric(Green_1$stop_lon),as.numeric(Green_1$stop_lat),color="green") %>%
      addCircleMarkers(as.numeric(Green_1$stop_lon),as.numeric(Green_1$stop_lat),radius=1,popup = Green_1$stop_name)
  })
  
  output$reminder = renderText("Please select corresponding number orrgin and destination")
  output$tripmap <- renderLeaflet({
    
    # Getting the directions from the Google Maps Directions API
    doc = mp_directions(
      origin = input$Origin,
      destination = input$Destination,
      mode = input$Mode,
      transit_mode = c("bus", "subway", "train", "tram"),
      departure_time = as.POSIXct("2022-12-14"),
      traffic_model = "best_guess",
      alternatives = TRUE,
      key = key,
      quiet = TRUE     )
    
    # Extract routes from the response
    routes = mp_get_routes(doc)
    travel_time_minutes = doc$summary$duration / 60
    travel_time_string = paste("Travel time:", travel_time_minutes, "minutes")
    
    # Visualize using leaflet
    palette = colorFactor(palette = "Set2", domain = routes$alternative_id)
    
    leaflet() %>%
      addProviderTiles(provider = providers$CartoDB.Positron) %>%
      
      addPolylines(data = routes,
                   opacity = 1,
                   weight = 7,
                   color = ~palette(alternative_id),
                   label = ~duration_text,
                   labelOptions = labelOptions(noHide = TRUE)

      )
    
  })
}

shinyApp(ui,server)





