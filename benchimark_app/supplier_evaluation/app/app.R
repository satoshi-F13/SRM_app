# Libraries ----
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

# Helper Functions ----
# Function to create value boxes
create_value_box <- function(title, value_id, icon, theme = "secondary", height = NULL) {
  value_box(
    title = title,
    value = textOutput(value_id, container = span),
    showcase = bsicons::bs_icon(icon),
    theme = theme,
    full_screen = FALSE,
    height = height,
    width = NULL,
    fill = TRUE,
    # margin and padding for spacing
    class = "mb-3",
    p_box = list(class = "p-2"),
    p_showcase = list(class = "text-end"),
    showcase_layout = "top right"
  )
}

# Function to calculate  distance
calculate_distance <- function(lon1, lat1, lon2, lat2) {
  # Radius of Earth in km
  R <- 6371
  
  # Convert degrees to radians
  to_radians <- function(deg) deg * pi / 180
  
  lon1 <- to_radians(lon1)
  lat1 <- to_radians(lat1)
  lon2 <- to_radians(lon2)
  lat2 <- to_radians(lat2)
  
  # Haversine formula
  delta_lon <- lon2 - lon1
  delta_lat <- lat2 - lat1
  
  a <- sin(delta_lat / 2)^2 + cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  distance <- round(R * c, 1)
  return(distance)
}

# UI Components ----
# Load branding
theme <- bs_theme(brand = "_brand.yml")

# Read the city data
it_city <- read_csv(file = "data/it.csv")

# Define UI ----
ui <- fluidPage( 
  theme = theme,
  
  # Minimal CSS
  tags$head(
    tags$style(HTML("
      .scroll-container { overflow-y: auto; max-height: 500px; }
      .modal-lg { width: 1200px; }
      
        /* Value box styling for better responsiveness */
    .value-box {
      min-height: 90px !important;
      overflow: visible !important;
    }
    
    .value-box .value {
      font-size: 1.5rem !important;
      white-space: nowrap !important;
      overflow: visible !important;
    }
    
    .value-box .showcase {
      font-size: 1.8rem !important;
    }
    
    /* Make responsive at different breakpoints */
    @media (max-width: 1200px) {
      .value-box .value {
        font-size: 1.2rem !important;
      }
      .value-box .showcase {
        font-size: 1.5rem !important;
      }
    }
    
    @media (max-width: 992px) {
      .value-box {
        min-height: 80px !important;
      }
    }

    "))
  ),
  
  # Shinyalert setup
  useShinyalert(force = TRUE),
  
  # The page_navbar component
  page_navbar(
    title = "Supplier Relationship Board",
    bg = "#61A60E",
    theme = theme,
    
    # Tab 1: Data Input Page
    nav_panel(
      title = "DATA INPUT",
      icon = bsicons::bs_icon("database"),
      fluidPage(
        # Upload file function
        fileInput("upload_csv", "Upload CSV", accept = c(".csv")),
        
        # Main content
        uiOutput("MainBody_sup"),
        
        # Action buttons
        fluidRow(
          column(
            12,
            helpText("Note: Remember to save any updates!"),
            br(),
            div(
              style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
              actionButton(inputId = "Updated_sup", label = "Save", icon = icon("save"), class = "btn-primary"),
              downloadButton("Supplier_csv", "Download in CSV", class = "btn-info")
            )
          )
        )
      )
    ),
    
    # Tab 2: Benchmark Report
    nav_panel(
      title = "ANALYSIS",
      icon = bsicons::bs_icon("graph-up"),
      page_sidebar(
        # Sidebar
        sidebar = sidebar(
          title = "Controls",
          bg = "#f8f9fa",
          
          selectInput(
            inputId = "Company",
            label = "Select Supplier:",
            choices = NULL
          ),
          
          downloadButton("export_pdf", "Export Report", icon = icon("file-pdf"), class = "btn-primary w-100 mb-3"),
          
          # Weight controls
          card(
            card_header("Weight Control"),
            lapply(
              list(
                list("cost_weight", "Cost Weight:", 0.25),
                list("quality_weight", "Quality Weight:", 0.3),
                list("delivery_weight", "Delivery Weight:", 0.25),
                list("service_weight", "Service Weight:", 0.1),
                list("tech_weight", "Technology Weight:", 0.1)
              ),
              function(item) {
                sliderInput(item[[1]], item[[2]], min = 0, max = 1, value = item[[3]], step = 0.05)
              }
            )
          )
        ),
        
        # Main Content
        div(
          class = "container-fluid",
          
          # Value Boxes
          layout_columns(
            fill = TRUE,
            height = NULL,
            class = "mb-4",
            col_widths = c(4,4,4,4,4,4), # set clumn widths
            # col width sizes: 12 = full width, 6=half width, 4 = third width, 3 = quarter width
            # Frist row
            create_value_box("Top Performer", "top_performer", "trophy-fill", "success"),
            create_value_box("Total Suppliers", "total_suppliers", "people-fill", "primary"),
            create_value_box("Company", "selected_company", "person-vcard"),
            # Second row
            create_value_box("Overall Score", "selected_score", "speedometer2"),
            create_value_box("Ranking", "supplier_rank", "bar-chart-line-fill"),
            create_value_box("Distance", "distance", "sign-merge-right")
          ),
          
          # Charts
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
          
          # Data Table
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

# SERVER ----
server <- function(input, output, session) {
  # REACTIVE VALUES ----
  val_sup <- reactiveValues()
  
  # Initialize data from RDS file
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
  
  # Reactive value for report data
  report_data <- reactiveVal(data.frame())
  
  # Update report_data when val_sup$Data changes
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
  
  # DATA INPUT TAB FUNCTIONS ----
  
  # CSV Upload Handling
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
      card(
        card_header("Supplier Data Management"),
        card_body(
          column(6,
                 offset = 0,
                 div(
                   class = "d-flex justify-content-between mb-3",
                   actionButton(inputId = "Add_row_head", label = "Add", icon = icon("plus"), class = "btn-primary"),
                   actionButton(inputId = "mod_row_head", label = "Edit", icon = icon("edit"), class = "btn-info"),
                   actionButton(inputId = "Del_row_head", label = "Delete", icon = icon("trash"), class = "btn-danger")
                 )
          ),
          column(12, dataTableOutput("Main_table_sup")),
          tags$script("$(document).on('click', '#Main_table_sup button', function () {
                     Shiny.onInputChange('lastClickId', this.id);
                     Shiny.onInputChange('lastClick', Math.random()) });")
        )
      )
    )
  })
  
  # Main DataTable
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
  
  # Add Row Modal
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
  
  # Add new row to Data
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
    
    # Update Company dropdown
    updateSelectInput(
      session,
      "Company",
      choices = unique(val_sup$Data$Company),
      selected = input[[paste0("Company_add", input$Add_row_head)]]
    )
    
    removeModal()
  })
  
  # Save to RDS 
  observeEvent(input$Updated_sup, {
    saveRDS(val_sup$Data, "note.rds")
    report_data(val_sup$Data)  # Update report data
    shinyalert(title = "Saved!", type = "success")
  })
  
  # Delete rows
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
  
  # Confirm deletion
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
  
  # Modify UI
  output$row_modif <- renderDataTable({
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
    
    datatable(
      row_change, 
      escape = F,
      options = list(dom = "t", ordering = F, scrollX = TRUE),
      selection = "none"
    )
  })
  
  # Update with modified values
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
    removeModal()
  })
  
  # Download data
  output$Supplier_csv <- downloadHandler(
    filename = function() {
      paste("Supplier data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data.frame(val_sup$Data), file, row.names = F)
    }
  )
  
  # ANALYSIS TAB FUNCTIONS ----
  
  # Calculate composite scores with category and technology level
  composite_scores <- reactive({
    req(report_data())
    if (nrow(report_data()) == 0) return(NULL)
    
    report_data() %>%
      mutate(
        # Calculate weighted score
        composite_score = 
          input$cost_weight * Cost + 
          input$quality_weight * Quality + 
          input$delivery_weight * Delivery + 
          input$service_weight * Service + 
          input$tech_weight * Technology,
        
        # Technology categorization
        Technology_Level = cut(
          Technology, 
          breaks = c(0, 5, 8, 10),
          labels = c("Low", "Medium", "High"),
          include.lowest = TRUE
        ),
        
        # Supplier categorization
        supplier_group = case_when(
          Quality >= 8 & Technology >= 8 & Service >= 8 & composite_score >= 7.5 ~ "Strategic",
          composite_score < 5 ~ "High-Risk",
          Cost >= 5 & Quality >= 7 & Delivery >= 7 ~ "Budget",
          TRUE ~ "Growth"
        )
      ) %>%
      arrange(desc(composite_score))
  })
  
  # Selected supplier data
  selected_data <- reactive({
    req(input$Company, composite_scores())
    if (is.null(composite_scores()) || !input$Company %in% composite_scores()$Company) return(NULL)
    
    composite_scores() %>% 
      filter(Company == input$Company)  
  })
  
  # VALUE BOX OUTPUTS ----
  
  # Total number of suppliers
  output$total_suppliers <- renderText({
    if (is.null(report_data()) || nrow(report_data()) == 0) return("N/A")
    nrow(report_data())
  })
  
  # Selected company
  output$selected_company <- renderText({
    req(input$Company, composite_scores())
    if (nrow(report_data()) > 0 && input$Company %in% composite_scores()$Company) {
      input$Company
    } else {
      "N/A"
    }
  })
  
  # Top performer
  output$top_performer <- renderText({
    if (is.null(composite_scores()) || nrow(composite_scores()) == 0) return("N/A")
    composite_scores() %>% pull(Company) %>% first()
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
  
  # Distance from Novara
  output$distance <- renderText({
    req(selected_data())
    if (is.null(selected_data())) return("N/A")
    
    # Coordinates of Novara
    novara_lon <- 8.6200
    novara_lat <- 45.4500
    
    distance <- calculate_distance(
      novara_lon, novara_lat,
      selected_data()$lon, selected_data()$lat
    )
    
    paste(distance, "km")
  })
  
  # CHARTS ----
  
  # Radar Chart
  output$radarChart <- renderPlotly({
    req(input$Company, report_data())
    if (nrow(report_data()) > 0 && input$Company %in% report_data()$Company) {
      # Filter data for the selected company
      selected_data <- report_data() %>% filter(Company == input$Company)
      
      plot_ly(
        type = "scatterpolar",
        fill = "toself"
      ) %>%
        add_trace(
          r = c(
            selected_data$Cost, selected_data$Quality, selected_data$Delivery,
            selected_data$Service, selected_data$Technology, selected_data$Cost
          ),
          theta = c("Cost", "Quality", "Delivery", "Service", "Technology", "Cost"),
          name = selected_data$Company,
          fillcolor = scales::alpha("#61A60E", 0.7),
          line = list(color = "#61A60E")
        ) %>%
        layout(
          polar = list(
            radialaxis = list(visible = TRUE, range = c(0, 10))
          ),
          showlegend = FALSE,
          margin = list(l = 50, r = 50, t = 50, b = 50),
          autosize = TRUE
        )
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
      
      plot_ly(
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
    } else {
      plotly_empty()
    }
  })
  
  # TABLES ----
  
  # Report table
  output$Report_table_sup <- renderDataTable({
    req(composite_scores())
    if (is.null(composite_scores()) || nrow(composite_scores()) == 0) return(NULL)
    
    table1 <- composite_scores() %>% select(-c(lon, lat))
    datatable(table1, options = list(pageLength = 10))
  })
  
  # EXPORT PDF ----
  
  # Prepare data for export
  export_data <- reactive({
    list(
      company = input$Company,
      distance = if (is.null(selected_data())) NA else {
        calculate_distance(
          8.6200, 45.4500,
          selected_data()$lon, selected_data()$lat
        )
      },
      total_sup = nrow(report_data()),
      ranking = if (is.null(selected_data())) "N/A" else {
        rank <- which(composite_scores()$Company == input$Company)
        paste(rank, "of", nrow(report_data()))
      },
      tech_level = if (is.null(selected_data())) "N/A" else {
        selected_data()$Technology_Level
      },
      score = if (is.null(selected_data())) "N/A" else {
        round(selected_data()$composite_score, 1)
      },
      category = if (is.null(selected_data())) "N/A" else {
        selected_data()$supplier_group
      }
    )
  })
  
  # Export PDF handler
  output$export_pdf <- downloadHandler(
    filename = function() {
      paste("SRM-Report", "_", input$Company, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Use withProgress to show a progress bar
      withProgress(message = "Creating Report: ", value = 0, {
        # Stage 1: Increment progress
        incProgress(0.3, detail = "Collecting inputs...")
        
        # Save data to temporary files
        temp_file <- tempfile(fileext = ".rds")
        temp_file2 <- tempfile(fileext = ".rds")
        saveRDS(composite_scores(), temp_file)
        saveRDS(selected_data(), temp_file2)
        
        # Prepare parameters
        params <- c(
          export_data(),
          list(
            df = temp_file,
            data = temp_file2
          )
        )
        
        # Stage 2
        incProgress(0.3, detail = "Building...")
        
        # Create a temporary directory for rendering
        tempDir <- tempdir()
        tempReport <- file.path(tempDir, "report.qmd")
        
        # Copy required files
        file.copy("report.qmd", tempReport, overwrite = TRUE)
        dir.create(file.path(tempDir, "asset"), showWarnings = FALSE)
        file.copy("asset/sample_logo.png", file.path(tempDir, "asset/sample_logo.png"), overwrite = TRUE)
        file.copy("typst-show.typ", file.path(tempDir, "typst-show.typ"), overwrite = TRUE)
        file.copy("typst-template.typ", file.path(tempDir, "typst-template.typ"), overwrite = TRUE)
        
        # Render report
        withr::with_dir(tempDir, {
          quarto::quarto_render(
            input = "report.qmd",
            output_format = "typst",
            execute_params = params
          )
          
          # Stage 3
          incProgress(1, detail = "Downloading report...")
          
          # Copy the generated PDF
          generatedPDF <- sub("\\.qmd$", ".pdf", tempReport)
          file.copy(generatedPDF, file, overwrite = TRUE)
        })
      })
    }
  )
  
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
}

# Run the application
shinyApp(ui = ui, server = server)