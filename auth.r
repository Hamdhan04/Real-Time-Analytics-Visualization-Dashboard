# auth.R
library(shiny)
library(shinymanager)

# Create credentials dataframe (in production, use a database)
credentials <- data.frame(
  user = c("admin", "user1"),
  password = c("adminpass", "user1pass"),
  stringsAsFactors = FALSE
)

# Initialize shinymanager
set_labels(
  language = "en",
  "Please authenticate" = "Dashboard Login",
  "Username:" = "Username",
  "Password:" = "Password"
)


