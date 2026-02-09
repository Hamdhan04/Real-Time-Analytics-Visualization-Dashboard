# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)

# Define UI
ui <- dashboardPage(
  skin = "purple",
  
  # Dashboard Header
  dashboardHeader(
    title = "Advanced Data Dashboard",
    titleWidth = 250
  ),
  
  # Dashboard Sidebar
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
  
  # Dashboard Body
  dashboardBody(
    includeCSS("styles.css"),
    
    tabItems(
      # Upload Tab
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "File Upload", status = "primary", solidHeader = TRUE,
            width = 12, height = 700,
            fluidRow(
              column(6,
                     h4("Select File"),
                     fileInput("file", NULL,
                               accept = c(".csv", ".xlsx", ".xls"),
                               buttonLabel = "Browse...",
                               placeholder = "No file selected"),
                     h4("File Options"),
                     checkboxInput("header", "Header", TRUE),
                     radioButtons("sep", "Separator",
                                  choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                                  selected = ","),
                     radioButtons("quote", "Quote",
                                  choices = c(None = "","Single Quote" = "'", "Double Quote" = '"'),
                                  selected = '"'),
                     actionButton("loadData", "Load Data", 
                                  class = "btn-primary btn-lg",
                                  style = "width: 50%; margin-top: 1px;")
              ),
              column(6,       
                     h4("Data Preview Options"),
                     numericInput("previewRows", "Number of rows to display:",
                                  value = 10, min = 1, max = 100, step = 1),
                     tags$script(HTML("
                         $(document).ready(function() {
                         $('#previewRows').on('input', function() {
                           Shiny.setInputValue('previewRows', $(this).val());
                        });
                     }")),
                     h4("Data Preview"),
                     div(style = "height: 375px; overflow-y: auto;",
                         tableOutput("dataPreview"))
              )
            )
          )
        )
      ),
      
      # Visualizations Tab
      tabItem(
        tabName = "visualize",
        fluidRow(
          box(
            title = "Plot Controls", status = "warning", solidHeader = TRUE,
            width = 3,
            selectInput("plotType", "Select Plot Type:",
                        choices = c(
                          "Scatter Plot" = "scatter",
                          "Line Chart" = "line",
                          "Bar Chart" = "bar",
                          "Histogram" = "histogram",
                          "Box Plot" = "boxplot",
                          "Violin Plot" = "violin",
                          "Density Plot" = "density",
                          "Heatmap" = "heatmap",
                          "Pie Chart" = "pie",
                          "Area Chart" = "area"
                        )),
            conditionalPanel(
              condition = "input.plotType != 'histogram' && input.plotType != 'density'",
              selectInput("xvar", "X Variable:", choices = NULL)
            ),
            conditionalPanel(
              condition = "input.plotType == 'scatter' || input.plotType == 'line' || input.plotType == 'bar' || input.plotType == 'boxplot' || input.plotType == 'violin' || input.plotType == 'area'",
              selectInput("yvar", "Y Variable:", choices = NULL)
            ),
            conditionalPanel(
              condition = "input.plotType == 'histogram' || input.plotType == 'density'",
              selectInput("numvar", "Numeric Variable:", choices = NULL)
            ),
            conditionalPanel(
              condition = "input.plotType == 'scatter' || input.plotType == 'line' || input.plotType == 'bar'",
              selectInput("colorvar", "Color Variable (Optional):", 
                          choices = c("None" = "none"), selected = "none")
            ),
            selectInput("colorScheme", "Color Scheme:",
                        choices = c(
                          "Viridis" = "viridis",
                          "Plasma" = "plasma",
                          "Inferno" = "inferno",
                          "Magma" = "magma",
                          "Cividis" = "cividis",
                          "Blues" = "Blues",
                          "Reds" = "Reds",
                          "Greens" = "Greens",
                          "Purples" = "Purples",
                          "Oranges" = "Oranges"
                        )),
            actionButton("generatePlot", "Generate Plot", class = "btn-success"),
            br(), br(),
            downloadButton("downloadPlot", "Download Plot", class = "btn-info")
          ),
          box(
            title = "Visualization", status = "primary", solidHeader = TRUE,
            width = 9,
            plotlyOutput("mainPlot", height = "600px")
          )
        )
      ),
      
      # Analytics Tab
      tabItem(
        tabName = "analytics",
        fluidRow(
          box(
            title = "Summary Statistics", status = "info", solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("summaryStats")
          ),
          box(
            title = "Correlation Matrix", status = "warning", solidHeader = TRUE,
            width = 6,
            plotlyOutput("correlationPlot")
          )
        ),
        fluidRow(
          box(
            title = "Data Distribution", status = "success", solidHeader = TRUE,
            width = 12,
            plotlyOutput("distributionPlot")
          )
        )
      ),
      
      # Export Tab
      tabItem(
        tabName = "export",
        fluidRow(
          box(
            title = "Export Options", status = "primary", solidHeader = TRUE,
            width = 12,
            h4("Download Data"),
            downloadButton("downloadData", "Download CSV", class = "btn-primary"),
            br(), br(),
            h4("Export Settings"),
            numericInput("plotWidth", "Plot Width (px):", value = 800, min = 400, max = 2000),
            numericInput("plotHeight", "Plot Height (px):", value = 600, min = 300, max = 1500),
            selectInput("plotFormat", "Plot Format:",
                        choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"))
          )
        )
      ),
      
      # Live Stocks Tab
      tabItem(
        tabName = "live_stocks",
        fluidRow(
          box(
            title = "Live Stock Data", status = "primary", solidHeader = TRUE,
            width = 12,
            selectInput("stockSymbols", "Select Stock Symbols:", 
                        choices = c("AAPL", "GOOGL", "MSFT", "AMZN", "TSLA"), 
                        multiple = TRUE, selected = "AAPL"),
            selectInput("timeInterval", "Select Time Interval:", 
                        choices = c("1 Minute" = "1min", "5 Minutes" = "5min", "15 Minutes" = "15min", 
                                    "1 Hour" = "1h", "1 Day" = "1d"), 
                        selected = "1min"),
            actionButton("fetchStock", "Fetch Stock Data"),
            plotlyOutput("stockPlot")
          )
        )
      )
    )
  )
)