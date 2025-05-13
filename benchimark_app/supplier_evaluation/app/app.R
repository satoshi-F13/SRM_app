#Try adding this line at the top of your app.R to set a user agent:
#apply fonts from googlefont 
options(HTTPUserAgent = "Mozilla/5.0")

library(shiny)
library(shinyjs)
library(bslib)
library(bsicons)
library(DT)
library(data.table)
library(lubridate)
library(shinyalert)
library(plotly)
library(readr)
library(dplyr)
library(ggmap)
library(rsconnect)

# Read the city data ####
it_city <- read_csv(file = "data/it.csv")

# Define val_sup function to provide initial choices for the Company dropdown
val_sup_init <- function() {
  data.frame(
    Company = character(0) # Empty character vector
  )
}

# Define UI for the app ####
ui <-fluidPage( 

  #Add a custom CSS
  tags$head(
    tags$style(HTML("
      .scroll-container {
        overflow-y: auto; /* Enable vertical scrolling */
        max-height: 500px; /* Set a maximum height for the scroll area */
      }
      .modal-lg {
        width: 1200px;
      }
      .butt {
        background-color: #8fce00;
        color: #e6ebef;
        margin-right: 10px;
      }
      .butt2{
        background-color:#8EACCD;
        color: #e6ebef;
      }
      .butt3{
        background-color:#FF8A8A;
        color: #e6ebef;
      }
      .butt4{
        background-color:#B7B7B7;
        color: #e6ebef;
      }
    "))
  ),
  
  ## Shinyalert setup ####
  useShinyalert(force = TRUE),
  
  # The page_navbar component ####
  page_navbar(
  title = "Supplier Relationship Board",
  theme = bs_theme(brand = "_brand.yml"),
  
  # Tab 1: Data Input Page ####
  nav_panel(
    title = "DATA INPUT",
    fluidPage(
      h1("Supplier Data Management", class = "text-center my-4"),
      
      fileInput("upload_csv", "Upload CSV", accept = c(".csv")),
      
      # Main content
      uiOutput("MainBody_sup"),
      # Action and download buttons in a row
      fluidRow(
        column(
          12,
          helpText("Note: Remember to save any updates!"),
          br(),
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
            actionButton(inputId = "Updated_sup", label = "Save", class = "butt"),
            downloadButton("Supplier_csv", "Download in CSV", class = "butt")
          )
        )
      )
    )
  ),
  
  # Tab 2: Benchmark Report ####
  nav_panel(
    title = "ANALYSIS",
    page_sidebar(
      # Sidebar
      sidebar = sidebar(
        title = "Controls",
        # fileInput(
        #   inputId = "upload_csv_report",
        #   label = "Upload Supplier Data (CSV)",
        #   accept = c(".csv"),
        #   multiple = FALSE,
        #   width = "100%",
        #   buttonLabel = "Browse...",
        #   placeholder = "No file selected"
        # ),
        selectInput(
          inputId = "Company",
          label = "Select Supplier:",
          choices = val_sup_init()$Company
        ),
        downloadButton("export_pdf", "PDF", class = "btn-success w-100"),
        
        card(
          card_header("Weight Control"),
          sliderInput("cost_weight", "Cost Weight:",
                      min = 0, max = 1, value = 0.25, step = 0.05),
          
          sliderInput("quality_weight", "Quality Weight:",
                      min = 0, max = 1, value = 0.3, step = 0.05),
          
          sliderInput("delivery_weight", "Delivery Weight:",
                      min = 0, max = 1, value = 0.25, step = 0.05),
          
          sliderInput("service_weight", "Service Weight:",
                      min = 0, max = 1, value = 0.1, step = 0.05), 
          
          sliderInput("tech_weight", "Technology Weight:",
                      min = 0, max = 1, value = 0.1, step = 0.05)
        )
      ),
      
      ## Main Content ####
      div(
        class = "container-fluid",
        # Display the page title
        # h1("Suppliers' Benchmark Report", class = "text-center my-4"),
        
        # First Row: Value Boxes
        layout_columns(
          fill = FALSE,
          value_box(
            title = "Top Performer",
            value = textOutput("top_performer"),
            showcase = bsicons::bs_icon("trophy-fill", size = ".7em"),
            theme = "success",
            full_screen = FALSE,
            height = "120px",
            showcase_layout = "top right"
          ),
          value_box(
            title = "Total Suppliers",
            value = textOutput("total_suppliers"),
            showcase = bsicons::bs_icon("people-fill", size = ".7em"),
            theme = "primary",
            full_screen = FALSE,
            height = "120px",
            showcase_layout = "top right"
          ),
          value_box(
            title = "Selected Company",
            value = textOutput("selected_company"),
            showcase = bsicons::bs_icon("person-vcard", size = ".7em"),
            theme = "secondary",
            full_screen = FALSE,
            height = "120px",
            showcase_layout = "top right"
          ),
          value_box(
            title = "Overall Score",
            value = textOutput("selected_score"),
            showcase = bsicons::bs_icon("speedometer2", size = ".7em"),
            theme = "secondary",
            full_screen = FALSE,
            height = "120px",
            showcase_layout = "top right"
          ),
          
          value_box(
            title = "Ranking",
            value = textOutput("supplier_rank"),
            showcase = bsicons::bs_icon("bar-chart-line-fill", size = ".7em"),
            theme = "secondary",
            full_screen = FALSE,
            height = "120px",
            showcase_layout = "top right"
          ),
          
          value_box(
            title = "Distance",
            value = textOutput("distance"),
            showcase = bsicons::bs_icon("sign-merge-right", size = ".7em"),
            theme = "secondary",
            full_screen = FALSE,
            height = "120px",
            showcase_layout = "top right"
          )
        ),
        
        ## Second row####
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Performance Analysis"),
            full_screen = TRUE,
            div(
              style = "height: 300px; width: 100%; display: flex; justify-content: center; align-items: center;",
              plotlyOutput("radarChart", height = "100%", width = "100%")
            ),
            card_body(min_height = 50)
          ),
          
          card(
            card_header("Geographical Distribution"),
            full_screen = TRUE,
            div(
              style = "height: 100%; margin: 0; padding: 0;",
              plotlyOutput("scattermap", height = "100%", width = "100%")
            ),
            card_body(
              style = "margin: 0; padding: 0;",
              min_height = 50
            )
          )
        ),
        
        ## Third Row: Data Table####
        card(
          card_header("Supplier Database"),
          full_screen = TRUE,
          dataTableOutput("Report_table_sup")
        )
      )
    )
  )
)
)

server <- function(input, output, session) {
  # Reactive values to store data
  val_sup <- reactiveValues()
  
  # Initialize val_sup$Data from the RDS file if it exists####
  observe({
    if (is.null(val_sup$Data)) {
      if (file.exists("note.rds")) {
        val_sup$Data <- readRDS("note.rds")
      } else {
        val_sup$Data <- data.frame(
          Date = character(),
          Company = character(),
          Cost = numeric(),
          Quality = numeric(),
          Delivery = numeric(),
          Service = numeric(),
          Technology = numeric(),
          City = character(),
          lon = numeric(),
          lat = numeric(),
          stringsAsFactors = FALSE
        )
      }
    }
  })
  
  # Reactive value for report data####
  report_data <- reactiveVal(data.frame(
    Date = character(),
    Company = character(),
    Cost = numeric(),
    Quality = numeric(),
    Delivery = numeric(),
    Service = numeric(),
    Technology = numeric(),
    City = character(),
    lon = numeric(),
    lat = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Update report_data when val_sup$Data changes####
  observe({
    if (!is.null(val_sup$Data) && nrow(val_sup$Data) > 0) {
      report_data(val_sup$Data)
      
      # Update Company dropdown dynamically
      updateSelectInput(
        session,
        "Company",
        choices = unique(val_sup$Data$Company),
        selected = unique(val_sup$Data$Company)[1]
      )
    }
  })
  
  #==========================================
  # Data Input Tab Functions
  #==========================================
  
  # CSV Upload Handling for Data Input tab####
  observeEvent(input$upload_csv, {
    req(input$upload_csv)
    tryCatch(
      {
        uploaded_data <- read_csv(input$upload_csv$datapath)
        required_cols <- c(
          "Date", "Company", "Cost", "Quality", "Delivery",
          "Service", "Technology", "City", "lon", "lat"
        )
        
        if (all(required_cols %in% names(uploaded_data))) {
          val_sup$Data <- uploaded_data
          shinyalert("Success!", "Data uploaded successfully", type = "success")
        } else {
          shinyalert("Error!", "CSV file must contain all required columns", type = "error")
        }
      },
      error = function(e) {
        shinyalert("Error!", "Failed to upload file", type = "error")
      }
    )
  })
  
  # MainBody_sup UI
  output$MainBody_sup <- renderUI({
    fluidPage(
      hr(),
      column(6,
             offset = 0,
             HTML('<div class="sbtn-group" role="group" aria-label="Basic example" style = "padding:10px">'),
             div(style = "display:inline-block;width:30%;text-align: center;", actionButton(inputId = "Add_row_head", label = "Add", class = "butt2")),
             div(style = "display:inline-block;width:30%;text-align: center;", actionButton(inputId = "mod_row_head", label = "Edit", class = "butt4")),
             div(style = "display:inline-block;width:30%;text-align: center;", actionButton(inputId = "Del_row_head", label = "Delete", class = "butt3")),
             HTML("</div>")
      ),
      column(12, dataTableOutput("Main_table_sup")),
      tags$script("$(document).on('click', '#Main_table_sup button', function () {
                   Shiny.onInputChange('lastClickId', this.id);
                   Shiny.onInputChange('lastClick', Math.random()) });")
    )
  })
  
  # Render DataTable for data input
  output$Main_table_sup <- renderDataTable({
    DT <- val_sup$Data
    datatable(DT,
              selection = "single", escape = F,
              options = list(
                scrollX = TRUE,
                scrollCollapse = TRUE,
                pageLength = 10,
                autoWidth = TRUE
              ),
              style = "bootstrap",
              class = "table-responsive"
    )
  })
  
  # Add Row Modal with Sliders
  observeEvent(input$Add_row_head, {
    showModal(modalDialog(
      title = "Add a new row",
      dateInput(paste0("Date_add", input$Add_row_head), "Date:", value = Sys.Date()),
      textInput(paste0("Company_add", input$Add_row_head), "Company"),
      sliderInput(paste0("Cost_add", input$Add_row_head), "Cost", min = 1, max = 10, value = 5),
      sliderInput(paste0("Quality_add", input$Add_row_head), "Quality", min = 1, max = 10, value = 5),
      sliderInput(paste0("Delivery_add", input$Add_row_head), "Delivery", min = 1, max = 10, value = 5),
      sliderInput(paste0("Service_add", input$Add_row_head), "Service", min = 1, max = 10, value = 5),
      sliderInput(paste0("Technology_add", input$Add_row_head), "Technology", min = 1, max = 10, value = 5),
      selectInput(paste0("City_add", input$Add_row_head), "City", choices = it_city$city),
      actionButton("go", "Add item"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Adding a new row to Data
  observeEvent(input$go, {
    city_details <- it_city %>% filter(city == input[[paste0("City_add", input$Add_row_head)]])
    new_row <- data.frame(
      Date = as.character(input[[paste0("Date_add", input$Add_row_head)]]),
      Company = input[[paste0("Company_add", input$Add_row_head)]],
      Cost = input[[paste0("Cost_add", input$Add_row_head)]],
      Quality = input[[paste0("Quality_add", input$Add_row_head)]],
      Delivery = input[[paste0("Delivery_add", input$Add_row_head)]],
      Service = input[[paste0("Service_add", input$Add_row_head)]],
      Technology = input[[paste0("Technology_add", input$Add_row_head)]],
      City = input[[paste0("City_add", input$Add_row_head)]],
      lon = city_details$lng,
      lat = city_details$lat,
      stringsAsFactors = FALSE
    )
    val_sup$Data <- rbind(val_sup$Data, new_row)
    
    # Update Company dropdown in the Benchmark tab
    if (length(unique(val_sup$Data$Company)) > 0) {
      updateSelectInput(
        session,
        "Company",
        choices = unique(val_sup$Data$Company),
        selected = input[[paste0("Company_add", input$Add_row_head)]]
      )
    }
    
    removeModal()
  })
  
  # Save to RDS 
  observeEvent(input$Updated_sup, {
    saveRDS(val_sup$Data, "note.rds")
    report_data(val_sup$Data)  # Update report data
    shinyalert(title = "Saved!", type = "success")
  })
  
  # Delete selected rows
  observeEvent(input$Del_row_head, {
    showModal(
      if (length(input$Main_table_sup_rows_selected) >= 1) {
        modalDialog(
          title = "Warning",
          paste("Are you sure delete", length(input$Main_table_sup_rows_selected), "rows?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "Yes")
          ), easyClose = TRUE
        )
      } else {
        modalDialog(
          title = "Warning",
          paste("Please select row(s) that you want to delete!"), easyClose = TRUE
        )
      }
    )
  })
  
  # If user says OK, then delete the selected rows
  observeEvent(input$ok, {
    val_sup$Data <- val_sup$Data[-input$Main_table_sup_rows_selected, ]
    removeModal()
  })
  
  # Edit button
  observeEvent(input$mod_row_head, {
    showModal(
      if (length(input$Main_table_sup_rows_selected) >= 1) {
        modalDialog(
          fluidPage(
            h3(strong("Modification"), align = "center"),
            hr(),
            dataTableOutput("row_modif"),
            actionButton("save_changes", "Save changes"),
            tags$script(HTML("$(document).on('click', '#save_changes', function () {
                             var list_value = [];
                             for (i = 0; i < $( '.new_input' ).length; i++) {
                               list_value.push($( '.new_input' )[i].value);
                             }
                             Shiny.onInputChange('newValue', list_value);
                             });"))
          ),
          size = "l"
        )
      } else {
        modalDialog(
          title = "Warning",
          paste("Please select the row that you want to edit!"), easyClose = TRUE
        )
      }
    )
  })
  
  # Modify part
  output$row_modif <- renderDataTable(
    {
      selected_row <- input$Main_table_sup_rows_selected
      old_row <- val_sup$Data[selected_row, ]
      row_change <- list()
      for (i in colnames(old_row)) {
        if (is.numeric(val_sup$Data[[i]])) {
          row_change[[i]] <- paste0('<input class="new_input" value="', old_row[[i]], '" type="number" id=new_', i, " ><br>")
        } else if (is.Date(val_sup$Data[[i]])) {
          row_change[[i]] <- paste0('<input class="new_input" value="', old_row[[i]], '" type="date" id=new_', i, "  ><br>")
        } else {
          row_change[[i]] <- paste0('<input class="new_input" value="', old_row[[i]], '" type="textarea" id=new_', i, "><br>")
        }
      }
      row_change <- as.data.table(row_change)
      setnames(row_change, colnames(old_row))
      DT <- row_change
      DT
    },
    escape = F,
    options = list(dom = "t", ordering = F, scrollX = TRUE),
    selection = "none"
  )
  
  # Replace the modified row to existing row
  observeEvent(input$newValue, {
    newValue <- lapply(input$newValue, function(col) {
      if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
        as.numeric(as.character(col))
      } else {
        col
      }
    })
    DF <- data.frame(lapply(newValue, function(x) t(data.frame(x))))
    colnames(DF) <- colnames(val_sup$Data)
    val_sup$Data[input$Main_table_sup_rows_selected, ] <- DF
    # Close the modal after saving changes
    removeModal()
  })
  
  # Download function
  output$Supplier_csv <- downloadHandler(
    filename = function() {
      paste("Supplier data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data.frame(val_sup$Data), file, row.names = F)
    }
  )
  
  #==========================================
  # Benchmark Report Tab Functions
  #==========================================
  
  # CSV Upload for Report tab
  observeEvent(input$upload_csv_report, {
    req(input$upload_csv_report)
    tryCatch(
      {
        uploaded_data <- read_csv(input$upload_csv_report$datapath)
        required_cols <- c(
          "Date", "Company", "Cost", "Quality", "Delivery",
          "Service", "Technology", "City", "lon", "lat"
        )
        
        if (all(required_cols %in% names(uploaded_data))) {
          report_data(uploaded_data)
          val_sup$Data <- uploaded_data  # Update the main data too
          shinyalert("Success!", "Data uploaded successfully", type = "success")
          
          # Update Company dropdown dynamically
          updateSelectInput(
            session,
            "Company",
            choices = unique(uploaded_data$Company),
            selected = unique(uploaded_data$Company)[1]
          )
          
        } else {
          shinyalert("Error!", "CSV file must contain all required columns", type = "error")
        }
      },
      error = function(e) {
        shinyalert("Error!", "Failed to upload file", type = "error")
      }
    )
  })
  
  # Validate that weights sum to 1
  observe({
    total_weight <- input$quality_weight + input$delivery_weight +
      input$cost_weight + input$service_weight + input$tech_weight
    
    if (abs(total_weight - 1) > 0.01) {
      showNotification(
        "Warning: Weights should sum to 1",
        type = "warning"
      )
    }
  })
  
  # Calculate total scores
  composite_scores <- reactive({
    req(report_data())
    if (nrow(report_data()) == 0) return(NULL)
    
    report_data() %>%
      mutate(composite_score = 
               input$cost_weight * Cost + 
               input$quality_weight * Quality + 
               input$delivery_weight * Delivery + 
               input$service_weight * Service + 
               input$tech_weight * Technology,
             # Categorize Technology into 3 size levels
             Technology_Level = cut(
               Technology, 
               breaks = c(0, 5, 8, 10),
               labels = c("Low", "Medium", "High"),
               include.lowest = TRUE
             ),
             
             # Categorization logic
             supplier_group = case_when(
               # Strategic Partners: High scores across most dimensions
               Quality >= 8 & Technology >= 8 & Service >= 8 & composite_score >= 7.5 
               ~ "Strategic",
               
               # High-Risk Suppliers: Low overall performance
               composite_score < 5 
               ~ "High-Risk",
               
               # Cost-Effective Performers: Good cost with decent other metrics
               Cost >= 5 & Quality >= 7 & Delivery >= 7 
               ~ "Cost-Effective",
               
               # Improvement Candidates: Moderate performance, room to grow
               TRUE 
               ~ "Improvement"
             )
      ) %>%
      arrange(desc(composite_score))
  })
  
  # Value box outputs
  
  # Total Numbers of Suppliers
  output$total_suppliers <- renderText({
    if (is.null(report_data()) || nrow(report_data()) == 0) return("N/A")
    nrow(report_data())
  })
  
  # Pull the name of selected company
  output$selected_company <- renderText({
    req(input$Company, composite_scores())
    if (nrow(report_data()) > 0 && input$Company %in% composite_scores()$Company) {
      input$Company
    } else {
      "N/A"
    }
  })
  
  # Pull the name of top performer
  output$top_performer <- renderText({
    if (is.null(composite_scores()) || nrow(composite_scores()) == 0) return("N/A")
    composite_scores() %>%
      pull(Company) %>%
      first()
  })
  
  # Selected supplier score
  output$selected_score <- renderText({
    req(input$Company, composite_scores())
    if (nrow(report_data()) > 0 && input$Company %in% composite_scores()$Company) {
      composite_scores() %>%
        filter(Company == input$Company) %>%
        pull(composite_score) %>%
        round(1)
    } else {
      "N/A"
    }
  })
  
  # Supplier rank
  output$supplier_rank <- renderText({
    req(input$Company, composite_scores())
    if (nrow(report_data()) > 0 && input$Company %in% composite_scores()$Company) {
      rank <- which(composite_scores()$Company == input$Company)
      paste(rank, "of", nrow(report_data()))
    } else {
      "N/A"
    }
  })
  
  # Radar Chart
  output$radarChart <- renderPlotly({
    req(input$Company, report_data())
    if (nrow(report_data()) > 0 && input$Company %in% report_data()$Company) {
      # Filter data for the selected company
      selected_data <- report_data() %>% filter(Company == input$Company)
      
      plot <- plot_ly(
        type = "scatterpolar",
        fill = "toself"
      ) %>%
        add_trace(
          r = c(
            selected_data$Cost, selected_data$Quality, selected_data$Delivery,
            selected_data$Service, selected_data$Technology, selected_data$Cost
          ),
          theta = c("Cost", "Quality", "Delivery", "Service", "Technology", "Cost"),
          name = selected_data$Company
        ) %>%
        layout(
          polar = list(
            radialaxis = list(visible = TRUE, range = c(0, 10))
          ),
          showlegend = FALSE,
          margin = list(l = 50, r = 50, t = 50, b = 50),
          autosize = TRUE
        )
      
      plot
    } else {
      plotly_empty()
    }
  })
  
  # Scatter Map
  output$scattermap <- renderPlotly({
    req(input$Company, report_data())
    if (nrow(report_data()) > 0 && input$Company %in% report_data()$Company) {
      # Filter data for the selected company
      selected_data <- report_data() %>% filter(Company == input$Company)
      
      plot <- plot_ly(
        data = selected_data,
        type = "scattermapbox",
        lon = ~lon,
        lat = ~lat,
        text = ~ paste("Company:", Company, "<br>City:", City),
        mode = "markers",
        marker = list(size = 10)
      ) %>%
        layout(
          mapbox = list(
            style = "open-street-map",
            zoom = 4.5,
            center = list(lon = mean(selected_data$lon), lat = mean(selected_data$lat))
          ),
          margin = list(l = 0, r = 0, t = 0, b = 0)
        )
      
      plot
    } else {
      plotly_empty()
    }
  })
  
  # Data table for report
  output$Report_table_sup <- renderDataTable({
    req(composite_scores())
    if (is.null(composite_scores()) || nrow(composite_scores()) == 0) return(NULL)
    
    table1 <- composite_scores() %>% select(-c(lon, lat))
    datatable(table1, options = list(pageLength = 10))
  })
  
  # Total suppliers
  total_sup <- reactive({
    if (is.null(report_data())) return(0)
    nrow(report_data())
  })
  
  # Supplier rank
  rank <- reactive({
    req(input$Company, composite_scores())
    if (is.null(composite_scores()) || !input$Company %in% composite_scores()$Company) return(NA)
    which(composite_scores()$Company == input$Company)
  })
  
  # Selected data with total score
  selected_data <- reactive({
    req(input$Company, composite_scores())
    if (is.null(composite_scores()) || !input$Company %in% composite_scores()$Company) return(NULL)
    composite_scores() %>% 
      filter(Company == input$Company)  
  })
  
  # Distance from Novara
  # Radius of Earth in km
  R <- 6371 
  # Coordinates of Novara
  novara_lon <- 8.6200
  novara_lat <- 45.4500
  
  distance <- reactive({
    req(selected_data())
    if (is.null(selected_data())) return(NA)
    
    # Convert degrees to radians
    to_radians <- function(deg) deg * pi / 180
    
    lon1 <- to_radians(novara_lon)
    lat1 <- to_radians(novara_lat)
    lon2 <- to_radians(selected_data()$lon)
    lat2 <- to_radians(selected_data()$lat)
    
    # Haversine formula
    delta_lon <- lon2 - lon1
    delta_lat <- lat2 - lat1
    
    a <- sin(delta_lat / 2)^2 + cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    
    distance <- round(R * c, 1)
    return(distance)
  })
  
  output$distance <- renderText({
    if (is.na(distance())) return("N/A")
    paste(distance(), "km")
  })
  
  # Assign the file name
  filename <- reactive({
    paste("SRM-Report", "_", input$Company, "_", Sys.Date(), ".pdf", sep = "")
  })
  
  # Export PDF
  output$export_pdf <- downloadHandler(
    filename = function() filename(),
    content = function(file) {
      # Use withProgress to show a progress bar
      withProgress(message = "Creating Report: ", value = 0, {
        # Stage 1: Increment progress
        incProgress(0.3, detail = "Collecting inputs...")
        
        # Write the dataframe to a temporary file
        temp_file <- tempfile(fileext = ".rds")
        saveRDS(composite_scores(), temp_file)
        
        # Ensure a company is selected
        req(input$Company)
        # Prepare parameters
        params <- list(
          company = input$Company,
          data = selected_data(),
          distance = distance(),
          df = temp_file,
          total_sup = total_sup(),
          ranking = rank()
        )
        
        # Stage 2
        incProgress(0.3, detail = "Building...")
        
        # Create a temporary directory for rendering
        tempDir <- tempdir()
        tempReport <- file.path(tempDir, "report.qmd")
        
        # Copy the report template to the temp directory
        file.copy("report.qmd", tempReport, overwrite = TRUE)
        
        # Copy required assets to temp directory
        dir.create(file.path(tempDir, "asset"), showWarnings = FALSE)
        file.copy("asset/sample_logo.png", file.path(tempDir, "asset/sample_logo.png"), overwrite = TRUE)
        file.copy("typst-show.typ", file.path(tempDir, "typst-show.typ"), overwrite = TRUE)
        file.copy("typst-template.typ", file.path(tempDir, "typst-template.typ"), overwrite = TRUE)
        
       # Set working directory to temp directory
        withr::with_dir(tempDir, {
          # Render the report with parameters
          quarto::quarto_render(
            input = "report.qmd",
            output_format = "typst",
            execute_params = params
          )
          # Stage 3
          incProgress(1, detail = "Downloading report...")
          
          # Copy the generated PDF to the output file location
          generatedPDF <- sub("\\.qmd$", ".pdf", tempReport)
          file.copy(generatedPDF, file, overwrite = TRUE)
        })
      })
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)