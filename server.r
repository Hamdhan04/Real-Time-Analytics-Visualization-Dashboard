# Load required libraries
  library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(readr)
library(readxl)
library(corrplot)
library(RColorBrewer)
library(httr)
library(jsonlite)

# Define UI (remains unchanged)
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Advanced Data Dashboard", titleWidth = 250),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Upload & Data", tabName = "upload", icon = icon("upload")),
      menuItem("Visualizations", tabName = "visualize", icon = icon("chart-line")),
      menuItem("Analytics", tabName = "analytics", icon = icon("calculator")),
      menuItem("Export", tabName = "export", icon = icon("download")),
      menuItem("Live Stocks", tabName = "live_stocks", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    includeCSS("styles.css"),
    tabItems(
      # Tabs defined here (upload, visualize, analytics, export, live_stocks)
      # ... [same as existing tabs in the UI you uploaded]
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  values <- reactiveValues(data = NULL, original_data = NULL)
  
  observeEvent(input$loadData, {
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    tryCatch({
      if(ext == "csv") {
        values$data <- read_csv(input$file$datapath, col_names = input$header, locale = locale(encoding = "UTF-8"))
      } else if(ext %in% c("xlsx", "xls")) {
        values$data <- read_excel(input$file$datapath, col_names = input$header)
      }
      values$original_data <- values$data
      numeric_vars <- names(select_if(values$data, is.numeric))
      all_vars <- names(values$data)
      updateSelectInput(session, "xvar", choices = all_vars)
      updateSelectInput(session, "yvar", choices = numeric_vars)
      updateSelectInput(session, "numvar", choices = numeric_vars)
      updateSelectInput(session, "colorvar", choices = c("None" = "none", all_vars))
      showNotification("Data loaded successfully!", type = "success")
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  output$dataPreview <- renderTable({
    if (is.null(values$data)) return(NULL)
    n_rows <- tryCatch({
      if (is.null(input$previewRows) || !is.numeric(input$previewRows) || is.na(input$previewRows)) 10 else max(1, min(100, round(input$previewRows)))
    }, error = function(e) 10)
    head(values$data, n_rows)
  })
  
  output$stockPlot <- renderPlotly({})
  
  observeEvent(input$fetchStock, {
    req(input$stockSymbols)
    invalidateLater(1000, session)  # Fetch every second
    stock_data_list <- lapply(input$stockSymbols, function(symbol) {
      get_stock_data(symbol, input$timeInterval)
    })
    combined_stock_data <- do.call(rbind, stock_data_list)
    output$stockPlot <- renderPlotly({
      plot_ly(data = combined_stock_data, x = ~datetime, y = ~close, type = 'scatter', mode = 'lines', color = ~symbol) %>%
        layout(title = "Stock Prices", xaxis = list(title = "Time"), yaxis = list(title = "Price"))
    })
  })
  
  get_stock_data <- function(symbol, interval) {
    api_key <- "6f2b40b20ce04e62bcdee9413efc6543"
    url <- paste0("https://api.twelvedata.com/time_series?symbol=", symbol, "&interval=", interval, "&apikey=", api_key)
    response <- GET(url)
    data <- fromJSON(content(response, "text"))
    if (!is.null(data$error)) stop(data$error)
    stock_data <- as.data.frame(data$values)
    stock_data$datetime <- as.POSIXct(stock_data$datetime, format="%Y-%m-%d %H:%M:%S")
    stock_data$symbol <- symbol
    return(stock_data)
  }
  
  
  # Run the application
  shinyApp(ui = ui, server = server)
  
  # File upload and data loading
  observeEvent(input$loadData, {
    req(input$file)
    
    ext <- tools::file_ext(input$file$datapath)
    
    tryCatch({
      if(ext == "csv") {
        values$data <- read_csv(input$file$datapath,
                                col_names = input$header,
                                locale = locale(encoding = "UTF-8"))
      } else if(ext %in% c("xlsx", "xls")) {
        values$data <- read_excel(input$file$datapath,
                                  col_names = input$header)
      }
      
      values$original_data <- values$data
      
      # Update variable choices
      numeric_vars <- names(select_if(values$data, is.numeric))
      all_vars <- names(values$data)
      
      updateSelectInput(session, "xvar", choices = all_vars)
      updateSelectInput(session, "yvar", choices = numeric_vars)
      updateSelectInput(session, "numvar", choices = numeric_vars)
      updateSelectInput(session, "colorvar", 
                        choices = c("None" = "none", all_vars))
      
      showNotification("Data loaded successfully!", type = "success")
      
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # Data preview outputs
  output$dataInfo <- renderText({
    if(is.null(values$data)) {
      return("No data loaded")
    }
    
    paste(
      "Rows:", nrow(values$data), "\n",
      "Columns:", ncol(values$data), "\n",
      "File size:", format(object.size(values$data), units = "MB")
    )
  })
  
  output$dataPreview <- renderTable({
    if (is.null(values$data)) return(NULL)
    
    # Safely get the number of rows to display
    n_rows <- tryCatch({
      # Default to 10 if input is missing/invalid
      if (is.null(input$previewRows) || 
          !is.numeric(input$previewRows) || 
          is.na(input$previewRows)) {
        10
      } else {
        # Ensure value is between 1 and 100
        max(1, min(100, round(input$previewRows)))
      }
    }, error = function(e) 10)  # Fallback to 10 if any error occurs
    
    head(values$data, n_rows)
  })
  
  output$dataTable <- DT::renderDataTable({
    if(is.null(values$data)) return(NULL)
    DT::datatable(values$data, 
                  options = list(scrollX = TRUE, pageLength = 15),
                  class = 'cell-border stripe')
  })
  
  # Main plot generation
  output$mainPlot <- renderPlotly({
    if(is.null(values$data)) {
      return(plot_ly() %>% 
               layout(title = "Please upload and load data first"))
    }
    
    input$generatePlot # Dependency on generate button
    
    isolate({
      data <- values$data
      
      # Create base plot based on type
      p <- switch(input$plotType,
                  "scatter" = create_scatter_plot(data, input),
                  "line" = create_line_plot(data, input),
                  "bar" = create_bar_plot(data, input),
                  "histogram" = create_histogram(data, input),
                  "boxplot" = create_box_plot(data, input),
                  "violin" = create_violin_plot(data, input),
                  "density" = create_density_plot(data, input),
                  "heatmap" = create_heatmap(data, input),
                  "pie" = create_pie_chart(data, input),
                  "area" = create_area_chart(data, input)
      )
      
      return(p)
    })
  })
  
  # Helper functions for different plot types
  create_scatter_plot <- function(data, input) {
    req(input$xvar, input$yvar)
    
    p <- plot_ly(data, x = ~get(input$xvar), y = ~get(input$yvar),
                 type = 'scatter', mode = 'markers',
                 marker = list(size = 8, opacity = 0.7))
    
    if(input$colorvar != "none") {
      p <- p %>% add_markers(color = ~get(input$colorvar),
                             colors = get_color_palette(input$colorScheme))
    } else {
      p <- p %>% add_markers(marker = list(color = get_single_color(input$colorScheme)))
    }
    
    p %>% layout(title = "Scatter Plot",
                 xaxis = list(title = input$xvar),
                 yaxis = list(title = input$yvar))
  }
  
  create_line_plot <- function(data, input) {
    req(input$xvar, input$yvar)
    
    p <- plot_ly(data, x = ~get(input$xvar), y = ~get(input$yvar),
                 type = 'scatter', mode = 'lines+markers',
                 line = list(width = 3))
    
    if(input$colorvar != "none") {
      p <- p %>% add_lines(color = ~get(input$colorvar),
                           colors = get_color_palette(input$colorScheme))
    } else {
      p <- p %>% add_lines(line = list(color = get_single_color(input$colorScheme)))
    }
    
    p %>% layout(title = "Line Chart",
                 xaxis = list(title = input$xvar),
                 yaxis = list(title = input$yvar))
  }
  
  create_bar_plot <- function(data, input) {
    req(input$xvar, input$yvar)
    
    # Aggregate data for bar plot
    agg_data <- data %>%
      group_by(!!sym(input$xvar)) %>%
      summarise(avg_val = mean(!!sym(input$yvar), na.rm = TRUE), .groups = 'drop')
    
    plot_ly(agg_data, x = ~get(input$xvar), y = ~avg_val,
            type = 'bar',
            marker = list(color = get_single_color(input$colorScheme))) %>%
      layout(title = "Bar Chart",
             xaxis = list(title = input$xvar),
             yaxis = list(title = paste("Average", input$yvar)))
  }
  
  create_histogram <- function(data, input) {
    req(input$numvar)
    
    plot_ly(data, x = ~get(input$numvar), type = "histogram",
            marker = list(color = get_single_color(input$colorScheme),
                          opacity = 0.7)) %>%
      layout(title = "Histogram",
             xaxis = list(title = input$numvar),
             yaxis = list(title = "Frequency"))
  }
  
  create_box_plot <- function(data, input) {
    req(input$xvar, input$yvar)
    
    plot_ly(data, x = ~get(input$xvar), y = ~get(input$yvar),
            type = "box",
            fillcolor = get_single_color(input$colorScheme)) %>%
      layout(title = "Box Plot",
             xaxis = list(title = input$xvar),
             yaxis = list(title = input$yvar))
  }
  
  create_violin_plot <- function(data, input) {
    req(input$xvar, input$yvar)
    
    plot_ly(data, x = ~get(input$xvar), y = ~get(input$yvar),
            type = "violin",
            fillcolor = get_single_color(input$colorScheme)) %>%
      layout(title = "Violin Plot",
             xaxis = list(title = input$xvar),
             yaxis = list(title = input$yvar))
  }
  
  create_density_plot <- function(data, input) {
    req(input$numvar)
    
    density_data <- density(data[[input$numvar]], na.rm = TRUE)
    
    plot_ly(x = density_data$x, y = density_data$y,
            type = "scatter", mode = "lines", fill = "tozeroy",
            fillcolor = get_single_color(input$colorScheme),
            line = list(color = get_single_color(input$colorScheme))) %>%
      layout(title = "Density Plot",
             xaxis = list(title = input$numvar),
             yaxis = list(title = "Density"))
  }
  
  create_heatmap <- function(data, input) {
    numeric_data <- select_if(data, is.numeric)
    
    if(ncol(numeric_data) < 2) {
      return(plot_ly() %>% layout(title = "Need at least 2 numeric variables for heatmap"))
    }
    
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    
    plot_ly(z = cor_matrix, type = "heatmap",
            colors = get_color_palette(input$colorScheme),
            x = colnames(cor_matrix),
            y = colnames(cor_matrix)) %>%
      layout(title = "Correlation Heatmap")
  }
  
  create_pie_chart <- function(data, input) {
    req(input$xvar)
    
    # Count frequencies
    pie_data <- data %>%
      count(!!sym(input$xvar)) %>%
      arrange(desc(n))
    
    plot_ly(pie_data, labels = ~get(input$xvar), values = ~n,
            type = "pie",
            marker = list(colors = get_color_palette(input$colorScheme))) %>%
      layout(title = "Pie Chart")
  }
  
  create_area_chart <- function(data, input) {
    req(input$xvar, input$yvar)
    
    plot_ly(data, x = ~get(input$xvar), y = ~get(input$yvar),
            type = "scatter", mode = "lines", fill = "tozeroy",
            fillcolor = get_single_color(input$colorScheme),
            line = list(color = get_single_color(input$colorScheme))) %>%
      layout(title = "Area Chart",
             xaxis = list(title = input$xvar),
             yaxis = list(title = input$yvar))
  }
  
  # Color palette functions
  get_color_palette <- function(scheme) {
    switch(scheme,
           "viridis" = "Viridis",
           "plasma" = "Plasma",
           "inferno" = "Inferno",
           "magma" = "Magma",
           "cividis" = "Cividis",
           "Blues" = "Blues",
           "Reds" = "Reds",
           "Greens" = "Greens",
           "Purples" = "Purples",
           "Oranges" = "Oranges"
    )
  }
  
  get_single_color <- function(scheme) {
    switch(scheme,
           "viridis" = "#440154",
           "plasma" = "#0d0887",
           "inferno" = "#000004",
           "magma" = "#000004",
           "cividis" = "#00204d",
           "Blues" = "#08519c",
           "Reds" = "#a50f15",
           "Greens" = "#006d2c",
           "Purples" = "#54278f",
           "Oranges" = "#d94801"
    )
  }
  
  # Analytics outputs
  output$summaryStats <- renderPrint({
    if(is.null(values$data)) return("No data loaded")
    
    numeric_data <- select_if(values$data, is.numeric)
    if(ncol(numeric_data) == 0) {
      return("No numeric variables found")
    }
    
    summary(numeric_data)
  })
  
  output$correlationPlot <- renderPlotly({
    if(is.null(values$data)) return(plot_ly())
    
    numeric_data <- select_if(values$data, is.numeric)
    if(ncol(numeric_data) < 2) {
      return(plot_ly() %>% layout(title = "Need at least 2 numeric variables"))
    }
    
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    
    plot_ly(z = cor_matrix, type = "heatmap",
            x = colnames(cor_matrix),
            y = colnames(cor_matrix),
            colors = "RdBu") %>%
      layout(title = "Correlation Matrix")
  })
  
  output$distributionPlot <- renderPlotly({
    if(is.null(values$data)) return(plot_ly())
    
    numeric_data <- select_if(values$data, is.numeric)
    if(ncol(numeric_data) == 0) {
      return(plot_ly() %>% layout(title = "No numeric variables found"))
    }
    
    # Create multiple histograms
    plots <- list()
    for(i in 1:min(4, ncol(numeric_data))) {
      var_name <- names(numeric_data)[i]
      plots[[i]] <- plot_ly(numeric_data, x = ~get(var_name),
                            type = "histogram", name = var_name)
    }
    
    subplot(plots, nrows = 2) %>%
      layout(title = "Variable Distributions")
  })
  
  # Download handlers
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("processed_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if(!is.null(values$data)) {
        write_csv(values$data, file)
      }
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot_", Sys.Date(), ".", input$plotFormat, sep = "")
    },
    content = function(file) {
      # This would require additional setup for plot export
      # For now, we'll create a simple message
      writeLines("Plot export functionality would be implemented here", file)
    }
  )
  
  # Live Stock Data Fetching
  observeEvent(input$fetchStock, {
    req(input$stockSymbols)
    
    # Start a reactive loop to fetch stock data every second
    invalidateLater(1000, session)  # Fetch every second
    
    stock_data_list <- lapply(input$stockSymbols, function(symbol) {
      get_stock_data(symbol, input$timeInterval)
    })
    
    # Combine all stock data into one data frame
    combined_stock_data <- do.call(rbind, stock_data_list)
    
    output$stockPlot <- renderPlotly({
      plot_ly(data = combined_stock_data, x = ~datetime, y = ~close, type = 'scatter', mode = 'lines', color = ~symbol) %>%
        layout(title = "Stock Prices",
               xaxis = list(title = "Time"),
               yaxis = list(title = "Price"))
    })
  })
  
  # Function to fetch stock data from Twelve Data API
  get_stock_data <- function(symbol, interval) {
    api_key <- "0XFXngJPFtekyn5yZy8DjV_pQhvEQcWI"  # Replace with your Twelve Data API key
    url <- paste0("https://api.polygon.io.com/time_series?symbol=", symbol, "&interval=", interval, "&apikey=", api_key)
    
    response <- GET(url)
    data <- fromJSON(content(response, "text"))
    
    # Check if the response contains data
    if (!is.null(data$error)) {
      stop(data$error)
    }
    
    # Extract the time series data
    stock_data <- data$values
    stock_data <- as.data.frame(stock_data)
    stock_data$datetime <- as.POSIXct(stock_data$datetime, format="%Y-%m-%d %H:%M:%S")
    stock_data$symbol <- symbol  # Add symbol for identification
    
    return(stock_data)
  }
}