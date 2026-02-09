# app.R
library(shiny)
library(shinydashboard)
library(shinymanager)
library(DT)
library(plotly)

# Create credentials (in production, use a secure database)
credentials <- data.frame(
  user = c("admin", "user1"),
  password = c("adminpass", "user1pass"),  # In production, use hashed passwords
  # password = sapply(c("adminpass", "user1pass"), sodium::password_store),
  stringsAsFactors = FALSE
)

# Source your UI and server files
source("ui.R")
source("server.R")

# Wrap your UI with secure_app
ui <- secure_app(
  ui, 
  tags_top = tags$div(
    tags$h2("My Secure App", style = "text-align:center;"),
    # Removed the broken image link
    tags$h4("Welcome to the Secure Dashboard")
  ),
  enable_admin = TRUE
)

# Server function with auth
server <- function(input, output, session) {
  
  # Check credentials
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # Get auth info
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # Only run your main app server code if authenticated
  observe({
    if (!is.null(res_auth$user)) {
      # Call your existing server logic
      source("server.R", local = TRUE)
    }
  })
}

shinyApp(ui, server)