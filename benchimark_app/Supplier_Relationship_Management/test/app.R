library(shiny)
library(plotly)
library(dplyr)

# Sample data: Replace this with your actual data
city_data <- data.frame(
  city = c("Rome", "Milan", "Naples"),
  lon = c(12.4964, 9.1900, 14.2681),
  lat = c(41.9028, 45.4642, 40.8518)
)

# Load existing notes if any
if (file.exists("note.rds")) {
  notes <- readRDS("note.rds")
} else {
  notes <- data.frame(supplier = character(), city = character(), lon = numeric(), lat = numeric(), stringsAsFactors = FALSE)
}

# Define UI
ui <- fluidPage(
  titlePanel("Supplier Location Mapping"),
  sidebarLayout(
    sidebarPanel(
      textInput("supplier", "Supplier Name:"),
      selectInput("city", "Select City:", choices = city_data$city),
      actionButton("save", "Save Data")
    ),
    mainPanel(
      plotlyOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$save, {
    # Get the selected city details
    city_details <- city_data %>% filter(city == input$city)
    
    # Create a new entry
    new_entry <- data.frame(
      supplier = input$supplier,
      city = input$city,
      lon = city_details$lon,
      lat = city_details$lat,
      stringsAsFactors = FALSE
    )
    
    # Add the new entry to the existing notes
    notes <<- rbind(notes, new_entry)
    
    # Save to note.rds file
    saveRDS(notes, "note.rds")
    
    # Update the map
    output$map <- renderPlotly({
      plot_ly() %>%
        add_trace(
          type = 'scattermapbox',
          mode = 'markers',
          lon = notes$lon,
          lat = notes$lat,
          text = paste("Supplier:", notes$supplier, "<br>City:", notes$city),
          marker = list(size = 10, color = 'red')
        ) %>%
        layout(
          mapbox = list(
            style = "open-street-map",
            zoom = 5,
            center = list(lon = 12.4964, lat = 41.9028)
          )
        )
    })
  })
  
  # Initial map rendering
  output$map <- renderPlotly({
    plot_ly() %>%
      add_trace(
        type = 'scattermapbox',
        mode = 'markers',
        lon = notes$lon,
        lat = notes$lat,
        text = paste("Supplier:", notes$supplier, "<br>City:", notes$city),
        marker = list(size = 10, color = 'red')
      ) %>%
      layout(
        mapbox = list(
          style = "open-street-map",
          zoom = 5,
          center = list(lon = 12.4964, lat = 41.9028)
        )
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
