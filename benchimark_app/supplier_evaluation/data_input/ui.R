library(shiny)
library(bslib)
library(DT)
library(tidyverse)
library(fontawesome)

# Sample city database (removed zip_code)
cities_db <- data.frame(
  city = c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix")
)

ui <- page_sidebar(
  theme = bs_theme(
    bg = "#FBFBFB",
    fg = "#274e13",
    primary = "#38761d"
  ),
  
  title = "Supplier Database Management",
  
  sidebar = sidebar(
    dateInput("date", "Date", format = "dd/mm/yyyy"),
    textInput("company", "Company Name"),
    
    sliderInput("cost", "Cost Rating", 
                min = 1, max = 10, value = 5),
    
    sliderInput("quality", "Quality Rating",
                min = 1, max = 10, value = 5),
    
    sliderInput("delivery", "Delivery Rating",
                min = 1, max = 10, value = 5),
    
    sliderInput("service", "Service Rating",
                min = 1, max = 10, value = 5),
    
    sliderInput("technology", "Technology Rating",
                min = 1, max = 10, value = 5),
    
    selectInput("city", "City",
                choices = cities_db$city),
    
    actionButton("add", "Add Supplier",
                 class = "btn-primary"),
    
    hr(),
    
    fileInput("upload", "Upload CSV File",
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    
    downloadButton("download", "Download Data")
  ),
  
  card(
    card_header("Supplier Database"),
    DTOutput("supplier_table")
  )
)
