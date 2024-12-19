
server <- function(input, output, session) {
  # Initialize reactive values for storing supplier data
  suppliers <- reactiveVal(data.frame(
    Date = character(),
    Company = character(),
    Cost = numeric(),
    Quality = numeric(),
    Delivery = numeric(),
    Service = numeric(),
    Technology = numeric(),
    City = character(),
    stringsAsFactors = FALSE
  ))
  
  # Add new supplier
  observeEvent(input$add, {
    new_supplier <- data.frame(
      Date = format(input$date, "%d/%m/%Y"),
      Company = input$company,
      Cost = input$cost,
      Quality = input$quality,
      Delivery = input$delivery,
      Service = input$service,
      Technology = input$technology,
      City = input$city
    )
    
    suppliers(rbind(suppliers(), new_supplier))
    
    showNotification(
      "Supplier added successfully!",
      type = "message",
      duration = 3
    )
  })
  
  # Handle file upload
  observeEvent(input$upload, {
    req(input$upload)
    
    tryCatch({
      uploaded_data <- read.csv(input$upload$datapath, stringsAsFactors = FALSE)
      
      required_cols <- c("Date", "Company", "Cost", "Quality", "Delivery", 
                         "Service", "Technology", "City")
      
      if (!all(required_cols %in% colnames(uploaded_data))) {
        showNotification(
          "Upload failed: CSV file must contain all required columns",
          type = "error",
          duration = 5
        )
        return()
      }
      
      suppliers(rbind(suppliers(), uploaded_data))
      
      showNotification(
        "File uploaded successfully!",
        type = "message",
        duration = 3
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error uploading file:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # Display editable supplier table with delete buttons
  output$supplier_table <- renderDT({
    df <- suppliers()
    if (nrow(df) > 0) {
      # Add delete button column
      delete_buttons <- paste0(
        '<button class="btn btn-danger btn-sm delete-btn" onclick="Shiny.setInputValue(\'delete_row\', this.getAttribute(\'data-row\'))">',
        '<i class="fa fa-trash"></i>',
        '</button>'
      )
      
      df$Actions <- sprintf(
        delete_buttons,
        seq_len(nrow(df))
      )
    } else {
      df$Actions <- character(0)
    }
    
    datatable(df,
              editable = TRUE,
              options = list(
                pageLength = 10,
                scrollX = TRUE
              ),
              rownames = FALSE,
              escape = FALSE) # Allow HTML in the table
  })
  
  # Handle delete button clicks
  observeEvent(input$delete_row, {
    row_to_delete <- as.numeric(input$delete_row)
    current_data <- suppliers()
    
    if (row_to_delete <= nrow(current_data)) {
      # Remove the row
      current_data <- current_data[-row_to_delete, ]
      suppliers(current_data)
      
      showNotification(
        "Row deleted successfully!",
        type = "message",
        duration = 3
      )
    }
  })
  
  # Handle table edits
  observeEvent(input$supplier_table_cell_edit, {
    info <- input$supplier_table_cell_edit
    
    # Get current data
    modified_data <- suppliers()
    
    # Update the edited cell
    modified_data[info$row, info$col] <- info$value
    
    # Update the reactive value
    suppliers(modified_data)
    
    showNotification(
      "Data updated successfully!",
      type = "message",
      duration = 3
    )
  })
  
  # Download handler
  output$download <- downloadHandler(
    filename = function() {
      paste("supplier_database_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Remove the Actions column before saving
      data_to_save <- suppliers()
      write.csv(data_to_save, file, row.names = FALSE)
    }
  )
}

