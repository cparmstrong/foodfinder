# install.packages("dplyr")
# install.packages("googleway")
# install.packages("shiny")
library(dplyr)
library(googleway)
library(leaflet)
library(shiny)


source("C:/Users/carmst18/Desktop/CPA_JHU_CRRE/R/goog_key.R"); print("you have to comment out the line that source the key to get it to deploy to shiny successfully; see https://gist.github.com/derzorngottes/3b57edc1f996dddcab25 to fix")

# key <- goog_key
set_key(key = goog_key)
# google_keys()

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("foodfinder"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput(
                inputId = "keywords",
                label   = "What would you like to eat?",
                value   = "tacos"
            ),
            textInput(
                inputId = "location",
                label   = "Where are we located?",
                value   = "Highlandtown Baltimore"
                # for now just add city to search
                # in future, search google for the address (does that even make sense?)
                # possible to have user drop pin on map?
            ),
            sliderInput(
                inputId = "radius",
                label   = "How far are we willing to drive?",
                min     = 0, 
                max     = 50000,
                value   = 10000
            ),
            sliderInput(  # have this pop up after results
                inputId = "rating",
                label   = "Rating",
                min     = 0, 
                max     = 5,
                value   = 4,
                step    = 0.1
            ),

            sliderInput(  # have this pop up after results
                inputId = "reviews",
                label   = "Minimum Number of Reviews",
                min     = 0, 
                max     = 2000,  # make this dynamic based on results
                value   = 10
            ),
            actionButton(
                inputId = "update",
                label   = "Update"
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("themap"),
            # h1(textOutput("n_results")),  # why is this not working
            dataTableOutput("table_results")#,
            # dataTableOutput("table_deets")
        )
    )
)

server <- function(input, output) {
    # https://stackoverflow.com/questions/53016404/advantages-of-reactive-vs-observe-vs-observeevent
    # do query in event reactive for keywords, location, radius
    # do filter (reviews/rating) in observe
    
    
    results <- eventReactive(input$update, {
        query <- google_places(search_string = paste(input$keywords,
                                                     input$location),
                               # location      = xyz, # latlong
                               radius = input$radius,
                               # keyword = "", # how is this different from search_string?
                               place_type = "restaurant", # maybe open this up in the future? ; food, meal_takeaway, bar
                               # price, opennow, 
                               key = goog_key)
    })
    
    data <- observe({

        
        results <- query$results %>%
            filter(rating > input$rating,
                   user_ratings_total > input$reviews)   # add distance subset
    
            sites <- results %>%  
                select(place_id)# %>%
            for(i in seq_along(sites$place_id)) {
                temp <- google_place_details(place_id = sites$place_id[i], key = goog_key)
                
                if(!is.null(temp$result$website)) {
                    sites[i,"website"] <- temp$result$website
                    sites[i,"website"] <- paste0("<a href='",
                                                 temp$result$website,
                                                 "'>",
                                                 temp$result$website,
                                                 "</a>")
                    # links working; change to be just a "button" (all the same text (="Link"))
                    # create backup search if is.null(website)
                }
                sites[i,"url"] <- temp$result$url
                sites[i,"lat"] <- temp$result$geometry$location$lat
                sites[i,"lng"] <- temp$result$geometry$location$lng
            }
    
            full <- merge(results, sites, by = c("place_id"))
            
            show <- full %>% 
                select(Name    = name, 
                       Address = formatted_address,  # can this be a link?
                       Rating  = rating, 
                       Reviews = user_ratings_total,
                       Link    = website)
                
            
            # out
            output$table_results <- renderDataTable({show}, 
                                                    escape = FALSE)  # how does this work? 
            
            output$themap <- renderLeaflet({
                m <- leaflet(data = full) %>%
                    addTiles() %>%
                    setView(lng  = sites$lng[i],  
                            # bad bc search isrelative to closesst place latlong
                            # better search: fetch latlong of search_query/location
                            lat  = sites$lat[i],
                            zoom = 13) %>%  # make this dynamic & dependent on radius
                    addMarkers(lng = ~lng,
                               lat = ~lat,
                               popup = paste(show$Name))
                m
            })
            
            
            })
  
    
}

shinyApp(ui = ui, server = server)


# deploy to shinyapps.io
# rsconnect::deployApp("C:/Users/carmst18/Desktop/CPA_JHU_CRRE/R/foodfinder")
