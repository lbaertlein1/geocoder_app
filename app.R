library(shiny)
library(shinyjs)
library(leaflet)
library(DT)
library(shinydashboard)
library(sf)
library(dplyr)
library(openxlsx)
library(leaflet.extras)
library(httr)
library(jsonlite)
library(tibble)
library(plotly)
library(shinycssloaders)
library(lubridate)
library(digest)


# bypass_login <- TRUE
bypass_login <- FALSE

# --- Create user accounts here ("username" = "password") ---
valid_users <- rio::import("data/account_info.xlsx", which="Users")

login_ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .login-container {
        max-width: 350px;
        margin: 60px auto 0 auto;
        padding: 20px;
        border: 1px solid #ccc;
        border-radius: 10px;
        box-shadow: 0 0 10px rgba(0,0,0,0.1);
        background-color: #f9f9f9;
      }
      .login-title {
        text-align: center;
        margin-top: 40px;
        font-size: 24px;
        font-weight: bold;
        color: #333;
      }
    "))
  ),
  div(class = "login-title", "El Salvador Wolbachia Geocoding Project"),
  div(class = "login-container",
      textInput("user", "Username"),
      passwordInput("password", "Password"),
      actionButton("login_btn", "Login", class = "btn btn-primary btn-block"),
      br(),
      verbatimTextOutput("login_message")
  )
)

source("code/ui.R", local = TRUE)
app_ui <- ui

source("code/server.R", local = TRUE)
app_server <- server

ui <- fluidPage(
  useShinyjs(),
  uiOutput("main_ui")
)

server <- function(input, output, session) {
  credentials <- reactiveValues(authenticated = FALSE, user = NULL, group = NULL)
  
  output$main_ui <- renderUI({
    if (bypass_login || credentials$authenticated) {
      app_ui
    } else {
      login_ui
    }
  })
  
  if (bypass_login) {
    credentials$authenticated <- TRUE
    credentials$user <- "BypassUser"
    credentials$group <- "admin"  # or some default group
    app_server(input, output, session,
               username = reactive({ credentials$user }),
               user_group = reactive({ credentials$group }))
  }
  
  observeEvent(input$login_btn, {
    req(input$user, input$password)
    
    matched_row <- valid_users %>%
      filter(username == input$user & password == input$password)
    
    if (nrow(matched_row) == 1) {
      credentials$authenticated <- TRUE
      credentials$user <- matched_row$username
      credentials$group <- matched_row$group
      app_server(input, output, session,
                 username = reactive({ credentials$user }),
                 user_group = reactive({ credentials$group }))
    } else {
      output$login_message <- renderText("Invalid username or password.")
    }
  })
}


shinyApp(ui, server)
