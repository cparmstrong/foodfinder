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







# x <- google_places(search_string = "tacos highlandtown baltimore",
#               radius = 10000,
#               place_type = "restaurant",
#               key = goog_key)              
# 
# y <- google_place_details(place_id = x$results$place_id[1], key = goog_key)
# y <- y$results
# View(as.data.frame(y$results))

# sites <- data.frame(matrix(ncol = 2, nrow = 0))
# for(i in seq_along(sites$cutvark))
#     temp <- google_place_details(place_id = y$results$place_id[i], key = goog_key)
#     temp <- data.frame(place_id = temp$result$place_id, 
#                        website  = temp$result$website) 
#     sites <- rbind(sites, unlist(temp))
# } 


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
            # h1(textOutput("n_results")),  # why is this not working
            dataTableOutput("table_results")#,
            # dataTableOutput("table_deets")
        )
    )
)

server <- function(input, output) {
    
    
    
    results <- eventReactive(input$update, {
        query <- google_places(search_string = paste(input$keywords,
                                                     input$location),
                               # location      = xyz, # latlong
                               radius = input$radius,
                               # keyword = "", # how is this different from search_string?
                               place_type = "restaurant", # maybe open this up in the future? ; food, meal_takeaway, bar
                               # price, opennow, 
                               key = goog_key)
        
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
                
                }
            sites[i,"url"] <- temp$result$url
            sites[i,"lat"] <- temp$result$geometry$location$lat
            sites[i,"lng"] <- temp$result$geometry$location$lng
        }

        out <- merge(results, sites, by = c("place_id"))
        
        out
        
    })
    output$table_results <- renderDataTable({out})
    
    # output$table_deets <- renderDataTable({deets()})
    # output$n_results <- nrow(results())

    
}

shinyApp(ui = ui, server = server)
