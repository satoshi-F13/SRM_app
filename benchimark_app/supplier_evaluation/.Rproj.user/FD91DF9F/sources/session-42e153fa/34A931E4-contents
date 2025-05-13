library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)

# Sample supplier data
suppliers <- data.frame(
  supplier_id = 1:5,
  supplier_name = c("Supplier A", "Supplier B", "Supplier C", "Supplier D", "Supplier E"),
  quality_score = c(85, 92, 78, 88, 95),
  delivery_score = c(90, 85, 82, 94, 88),
  price_score = c(75, 82, 90, 85, 78),
  service_score = c(88, 86, 84, 92, 90)
)

ui <- page_sidebar(
  title = "Supplier Evaluation & Benchmark System",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Sidebar with input controls
  sidebar = sidebar(
    title = "Evaluation Controls",
    
    selectInput("supplier", "Select Supplier:",
                choices = suppliers$supplier_name),
    
    sliderInput("quality_weight", "Quality Weight:",
                min = 0, max = 1, value = 0.3, step = 0.1),
    
    sliderInput("delivery_weight", "Delivery Weight:",
                min = 0, max = 1, value = 0.25, step = 0.1),
    
    sliderInput("price_weight", "Price Weight:",
                min = 0, max = 1, value = 0.25, step = 0.1),
    
    sliderInput("service_weight", "Service Weight:",
                min = 0, max = 1, value = 0.2, step = 0.1)
  ),
  
  # Main panel with cards
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Overall Score",
      value = textOutput("selected_score"),
      theme = "primary"
    ),
    value_box(
      title = "Ranking",
      value = textOutput("supplier_rank"),
      theme = "secondary"
    )
  ),
  
  card(
    card_header("Performance Radar Chart"),
    plotOutput("radar_plot")
  ),
  
  card(
    card_header("Supplier Rankings"),
    DTOutput("ranking_table")
  )
)

server <- function(input, output, session) {
  
  # Calculate weighted scores
  weighted_scores <- reactive({
    suppliers %>%
      mutate(
        weighted_score = quality_score * input$quality_weight +
          delivery_score * input$delivery_weight +
          price_score * input$price_weight +
          service_score * input$service_weight
      ) %>%
      arrange(desc(weighted_score))
  })
  
  # Selected supplier score
  output$selected_score <- renderText({
    score <- weighted_scores() %>%
      filter(supplier_name == input$supplier) %>%
      pull(weighted_score)
    round(score, 1)
  })
  
  # Supplier rank
  output$supplier_rank <- renderText({
    rank <- which(weighted_scores()$supplier_name == input$supplier)
    paste(rank, "of", nrow(suppliers))
  })
  
  # Radar plot
  output$radar_plot <- renderPlot({
    selected_data <- suppliers %>%
      filter(supplier_name == input$supplier)
    
    # Prepare data for radar plot
    scores <- c(selected_data$quality_score, selected_data$delivery_score,
                selected_data$price_score, selected_data$service_score)
    metrics <- c("Quality", "Delivery", "Price", "Service")
    
    # Create data frame for plotting
    plot_data <- data.frame(
      metric = metrics,
      score = scores,
      angle = seq(0, 2*pi, length.out = length(metrics) + 1)[1:length(metrics)]
    )
    
    ggplot(plot_data, aes(x = metric, y = score)) +
      geom_polygon(aes(group = 1), fill = "lightblue", alpha = 0.5) +
      geom_point() +
      coord_polar() +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      labs(title = paste("Performance Metrics for", input$supplier))
  })
  
  # Rankings table
  output$ranking_table <- renderDT({
    weighted_scores() %>%
      select(supplier_name, quality_score, delivery_score, price_score, 
             service_score, weighted_score) %>%
      rename(
        Supplier = supplier_name,
        Quality = quality_score,
        Delivery = delivery_score,
        Price = price_score,
        Service = service_score,
        "Overall Score" = weighted_score
      ) %>%
      datatable(options = list(pageLength = 5))
  })
  
  # Validate that weights sum to 1
  observe({
    total_weight <- input$quality_weight + input$delivery_weight +
      input$price_weight + input$service_weight
    
    if (abs(total_weight - 1) > 0.01) {
      showNotification(
        "Warning: Weights should sum to 1",
        type = "warning"
      )
    }
  })
}

shinyApp(ui, server)
