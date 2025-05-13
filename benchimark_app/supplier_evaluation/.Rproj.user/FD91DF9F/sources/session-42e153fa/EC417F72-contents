library(shiny)
library(shinyjs)
library(bslib)
library(DT)
library(data.table)
library(lubridate)
library(shinyalert)
library(plotly)
library(readr)
library(dplyr)

# Read the city data
it_city <- read_csv(file = "data/it.csv")

# Define UI for the app
ui <- fluidPage(
  titlePanel("Supplier Relationship Management (SRM) App"),
  # Custom CSS for modal and download button
  tags$head(
    tags$style(HTML("
      .modal-lg {
        width: 1200px;
      }
      .butt {
        background-color: #8fce00;
        color: #e6ebef;
        margin-right: 10px;
      }
    "))
  ),
  fileInput("upload_csv", "Upload CSV", accept = c(".csv")),
  # Shinyalert setup
  useShinyalert(force = TRUE),
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
  ),
  page_fillable(theme = bs_theme(version = 5, bootswatch = "minty"))
)


# Define server logic
server <- function(input, output, session) {
  val_sup <- reactiveValues()
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

  # CSV Upload Handling
  observeEvent(input$upload_csv, {
    req(input$upload_csv)
    uploaded_data <- read_csv(input$upload_csv$datapath)
    if (all(c("Date", "Company", "Cost", "Quality", "Delivery", "Service", "Technology", "City", "lon", "lat") %in% names(uploaded_data))) {
      val_sup$Data <- uploaded_data
      shinyalert("Data Uploaded Successfully!", type = "success")
    } else {
      shinyalert("Invalid file format. Please upload a valid CSV!", type = "error")
    }
  })

  #### MainBody_sup is the id of DT table
  output$MainBody_sup <- renderUI({
    fluidPage(
      hr(),
      column(6,
        offset = 0,
        HTML('<div class="sbtn-group" role="group" aria-label="Basic example" style = "padding:10px">'),
        ### tags$head() This is to change the color of "Add a new row" button
        tags$head(tags$style(".butt2{background-color:#8EACCD;} .butt2{color: #e6ebef;}")),
        div(style = "display:inline-block;width:30%;text-align: center;", actionButton(inputId = "Add_row_head", label = "Add", class = "butt2")),
        tags$head(tags$style(".butt4{background-color:#B7B7B7;} .butt4{color: #e6ebef;}")),
        div(style = "display:inline-block;width:30%;text-align: center;", actionButton(inputId = "mod_row_head", label = "Edit", class = "butt4")),
        tags$head(tags$style(".butt3{background-color:#FF8A8A;} .butt3{color: #e6ebef;}")),
        div(style = "display:inline-block;width:30%;text-align: center;", actionButton(inputId = "Del_row_head", label = "Delete", class = "butt3")),
        ### Optional: a html button
        HTML("</div>")
      ),
      column(12, dataTableOutput("Main_table_sup")),
      tags$script("$(document).on('click', '#Main_table_sup button', function () {
                   Shiny.onInputChange('lastClickId', this.id);
                   Shiny.onInputChange('lastClick', Math.random()) });")
    )
  })

  # Render DataTable
  output$Main_table_sup <- renderDataTable({
    DT <- val_sup$Data
    datatable(DT,
      selection = "single", escape = F,
      options = list(
        scrollX = TRUE, # Enable horizontal scrolling
        scrollCollapse = TRUE, # Allow table to collapse
        pageLength = 10, # Set default number of rows
        autoWidth = TRUE # Automatically adjust column widths
      ),
      style = "bootstrap",
      class = "table-responsive" # Bootstrap responsive class
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
    removeModal()
  })

  ### Save to RDS part
  observeEvent(input$Updated_sup, {
    saveRDS(val_sup$Data, "note.rds")
    shinyalert(title = "Saved!", type = "success") # pop-up message
  })

  ### Delete selected rows part
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

  ### If user says OK, then delete the selected rows
  observeEvent(input$ok, {
    val_sup$Data <- val_sup$Data[-input$Main_table_sup_rows_selected, ]
    removeModal()
  })

  ### Edit button
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

  ### Modify part
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

  ### This is to replace the modified row to existing row
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



  ### Add download function
  output$Supplier_csv <- downloadHandler(
    filename = function() {
      paste("Supplier data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data.frame(val_sup$Data), file, row.names = F)
    }
  )
}

# Add a factor level list for the new column
factor_levels <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# Run the application
shinyApp(ui = ui, server = server)
