# install.packages("dplyr")
# install.packages("googleway")
# install.packages("shiny")
library(dplyr)
library(googleway)
library(shiny)


source("C:/Users/carmst18/Desktop/CPA_JHU_CRRE/R/goog_key.R")
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
            actionButton(
                inputId = "update",
                label   = "Update"
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
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
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
            select(formatted_address, # geometry.location.lat, geometry.location.lng,
                   name, price_level, rating, user_ratings_total, 
                   place_id) %>%
            filter(rating > input$rating,
                   user_ratings_total > input$reviews)   # add distance subset
            
            
            # sites <- data.frame(matrix(ncol = 2, 
            sites <- results %>%  
                select(place_id)# %>%
                # mutate()
            for(i in seq_along(sites$place_id)) {
                temp <- google_place_details(place_id = sites$place_id[i], key = goog_key)
                
                if(!is.null(temp$result$website)) {
                    sites[i,"website"] <- temp$result$website
                    sites[i,"website"] <- paste0("<a href='",
                                                 temp$result$website,
                                                 "'>",
                                                 temp$result$website,
                                                 "</a>")
                    # why isn't the link update working
                    }
                sites[i,"url"] <- temp$result$url
                sites[i,"lat"] <- temp$result$geometry$location$lat
                sites[i,"lng"] <- temp$result$geometry$location$lng
            }
    
            full <- merge(results, sites, by = c("place_id"))
            
            show <- full %>% 
                select(name, formatted_address, 
                       rating, user_ratings_total,
                       website)
                
            
            # out
            output$table_results <- renderDataTable({show})  # how does this work? 
            
            })
    
    # output$n_results <- nrow(out)

    
}

shinyApp(ui = ui, server = server)
