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
  credentials <- reactiveValues(authenticated = FALSE, user = NULL)
  
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
    app_server(input, output, session, username = reactive({ credentials$user }))
  }
  
  observeEvent(input$login_btn, {
    req(input$user, input$password)
    valid <- any(input$user == valid_users$username & input$password == valid_users$password)
    if (valid) {
      credentials$authenticated <- TRUE
      credentials$user <- input$user  # Save username
      app_server(input, output, session, username = reactive({ credentials$user }))
    } else {
      output$login_message <- renderText("Invalid username or password.")
    }
  })
}

shinyApp(ui, server)
