library(shiny)
library(leaflet)
library(DT)
library(shinydashboard)
library(shinyjs)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML("
      html, body {
        height: 100%;
        margin: 0;
        padding: 0;
        overflow: hidden;
      }

      .full-height {
        height: calc(100vh - 20px);
      }

      .half-height {
        height: calc(50vh - 20px);
        overflow-y: auto;
      }

      .leaflet-container {
        height: 100% !important;
      }

      .box-no-margin {
        margin-bottom: 10px;
      }
    ")),
    tags$link(
      rel = "stylesheet",
      href = "https://use.fontawesome.com/releases/v5.15.4/css/all.css",
      integrity = "sha384-jLKHWMZPppYjH6v2KrWy2z6q9zjlKhOQZBxzSCTxnYJvoRxTSo2q9Fh41Zk5zw1T",
      crossorigin = "anonymous"
    )
  ),
  
  fluidRow(
    column(
      width = 8,
      box(
        title = "Map",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        class = "full-height box-no-margin",
        leafletOutput("map", height = "100%")
      )
    ),
    column(
      width = 4,
      div(
        class = "half-height box-no-margin",
        box(
          title = "Address List",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          DTOutput("address_table")
        )
      ),
      div(
        class = "half-height",
        box(
          title = "Selected Location:",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          fluidRow(
            column(width = 12, strong("Original Address:"), textOutput("selected_address"))
          ),
          fluidRow(
            column(width = 12, strong("Reverse-Geocoded Address:"), textOutput("reverse_geocode_address"))
          ),
          br(),
          fluidRow(
            column(width = 6, selectInput("location_type", "Location Type:",
                                          choices = c("", "Household", "Multi-House Complex", "Street", "Neighborhood", "City", "Other"))),
            column(width = 6, textInput("notes", "Notes:"))
          ),
          fluidRow(
            column(width = 6, strong("Correct Latitude:"), textOutput("correct_lat")),
            column(width = 6, strong("Correct Longitude:"), textOutput("correct_lng"))
          ),
          fluidRow(
            column(width = 6, actionButton("confirm_point", "Confirm Selected Location")),
            column(width = 6, downloadButton("download_data", "Download Completed Data"))
          )
        )
      )
    )
  )
)
