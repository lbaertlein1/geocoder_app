library(shiny)
library(leaflet)
library(DT)
library(shinydashboard)
library(shinyjs)

fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML(".map-box, .table-box { height: 600px; overflow-y: auto; }")),
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://use.fontawesome.com/releases/v5.15.4/css/all.css",
        integrity = "sha384-jLKHWMZPppYjH6v2KrWy2z6q9zjlKhOQZBxzSCTxnYJvoRxTSo2q9Fh41Zk5zw1T",
        crossorigin = "anonymous"
      )
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
        class = "map-box",
        leafletOutput("map", height = 580)
      )
    ),
    column(
      width = 4,
      box(
        title = "Address List",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        class = "table-box",
        DTOutput("address_table"),
        style = "height: 580px; overflow-y: scroll;"
      )
    )
  ),
  fluidRow(
    column(
      width = 8,
      box(
        title = "Selected Location:",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          column(width = 8, strong("Original Address:"), textOutput("selected_address"))
        ),
        fluidRow(
          column(width = 8, strong("Reverse-Geocoded Address:"), textOutput("reverse_geocode_address"))
        ),
        fluidRow(),
        fluidRow(
          column(width = 4, selectInput("location_type", "Location Type:",
                                        choices = c("", "Household", "Multi-House Complex", "Street", "Neighborhood", "City", "Other"))),
          column(width = 4, textInput("notes", "Notes:"))
        ),
        fluidRow(
          column(width = 4, strong("Correct Latitude:"), textOutput("correct_lat")),
          column(width = 4, strong("Correct Longitude:"), textOutput("correct_lng"))
        ),
        fluidRow(
          column(width = 4, actionButton("confirm_point", "Confirm Selected Location")),
          column(width = 4),
          column(width = 4, downloadButton("download_data", "Download Completed Data"))
        ),
        fluidRow(),
        fluidRow(),
        fluidRow(),
        fluidRow(),
        fluidRow(),
        fluidRow()
      )
    )
  )
)