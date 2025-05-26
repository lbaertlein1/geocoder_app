library(shiny)
library(leaflet)
library(DT)
library(sf)
library(dplyr)
library(openxlsx)
library(leaflet.extras)
library(httr)
library(jsonlite)
library(shinyjs)

sf_use_s2(FALSE)

# --- Login credentials ---
valid_users <- data.frame(
  username = c("Carol", "Demo"),
  password = c("qt314", "Password"),
  stringsAsFactors = FALSE
)

# --- Load data ---
intervention_areas <- readRDS("data/Shapefiles/intervention_areas.Rds")
addresses <- readRDS("data/clean/addresses_list.Rds")
geocode_pts <- readRDS("data/clean/addresses_geocode_pts.Rds")

country_outline <- st_read("data/raw/El Salvador_Municipios/WGS84_Municipios.shp") %>%
  st_union() %>% st_make_valid()

intervention_union <- st_union(intervention_areas) %>%
  st_make_valid() %>%
  st_transform(st_crs(country_outline))

intervention_mask <- st_difference(country_outline, intervention_union) %>%
  st_make_valid() %>% st_as_sf()

santa_ana_coords <- c(lng = -89.681, lat = 13.994)

# --- UI setup ---
login_ui <- fluidPage(
  useShinyjs(),
  titlePanel("Login"),
  wellPanel(
    textInput("user", "Username"),
    passwordInput("password", "Password"),
    actionButton("login_btn", "Login"),
    verbatimTextOutput("login_message")
  )
)

# Load app UI/server and assign to local variables
source("code/ui.R", local = TRUE)
app_ui <- ui  # rename to avoid overwrite

source("code/server.R", local = TRUE)
app_server <- server

ui <- fluidPage(
  useShinyjs(),
  uiOutput("main_ui")
)

server <- function(input, output, session) {
  credentials <- reactiveValues(authenticated = FALSE)
  
  output$main_ui <- renderUI({
    if (!credentials$authenticated) {
      login_ui
    } else {
      app_ui
    }
  })
  
  observeEvent(input$login_btn, {
    req(input$user, input$password)
    valid <- any(input$user == valid_users$username & input$password == valid_users$password)
    if (valid) {
      credentials$authenticated <- TRUE
      app_server(input, output, session)  # bind main app logic
    } else {
      output$login_message <- renderText("Invalid username or password.")
    }
  })
}

shinyApp(ui, server)
