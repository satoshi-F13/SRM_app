library(shiny)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Numeric Data Analysis"),
  
  # Input panel
  sidebarLayout(
    sidebarPanel(
      textInput("data_input", "Enter numeric data (comma-separated):"),
      actionButton("submit_button", "Submit")
    ),
    
    # Chart panel
    mainPanel(
      plotOutput("data_plot")
    )
  ),
  
  # Report button
  actionButton("generate_report_button", "Generate and Publish Report")
)

# Define server logic
server <- function(input, output) {
  # Reactive values to store input data
  input_data <- reactiveValues(data = NULL)
  
  # Store input data when submit button is clicked
  observeEvent(input$submit_button, {
    input_data$data <- as.numeric(unlist(strsplit(input$data_input, ",")))
  })
  
  # Generate and render plot based on input data
  output$data_plot <- renderPlot({
    if (!is.null(input_data$data)) {
      hist(input_data$data, main = "Histogram of Input Data", xlab = "Values")
    }
  })
  
  # Generate and publish report when the report button is clicked
  observeEvent(input$generate_report_button, {
    if (!is.null(input_data$data)) {
      # Generate HTML report
      report_content <- paste("<h1>Report</h1>",
                              "<p>Summary statistics:</p>",
                              "<ul>",
                              paste("<li>Mean:", mean(input_data$data), "</li>"),
                              paste("<li>Median:", median(input_data$data), "</li>"),
                              paste("<li>Standard Deviation:", sd(input_data$data), "</li>"),
                              "</ul>")
      
      # Write report to a temporary file
      temp_report_file <- tempfile(fileext = ".html")
      writeLines(report_content, temp_report_file)
      
      # Open report in browser
      browseURL(temp_report_file)
    } else {
      # If no data is available, show an alert
      showModal(
        modalDialog(
          title = "Error",
          "No data available. Please input data first.",
          easyClose = TRUE
        )
      )
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)