library(dplyr)
library(googleway)
library(leaflet)
library(shiny)

source("C:/Users/carmst18/Desktop/CPA_JHU_CRRE/R/foodfinder/goog_key.R")


input <- list()
input$keywords <- "tacos"
input$location <- "highlandtown baltimore"
input$radius   <- 10000
input$rating   <- 3.0
input$reviews  <- 100



i <- 1