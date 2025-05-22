library(shiny)
library(leaflet)
library(DT)
library(shinydashboard)

fluidPage(
  tags$head(
    tags$style(HTML(".map-box, .table-box { height: 600px; overflow-y: auto; }"))
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
        title = "Input Parameters",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          column(width = 8, verbatimTextOutput("selected_address"))
        ),
        fluidRow(
          column(width = 4, textInput("correct_lat", "Correct Latitude:")),
          column(width = 4, textInput("correct_lng", "Correct Longitude:"))
        ),
        fluidRow(
          column(width = 4, selectInput("location_type", "Location Type:",
                                        choices = c("", "Household", "Complex", "Neighborhood", "City"))),
          column(width = 4, textInput("notes", "Notes:"))
        ),
        fluidRow(
          column(width = 4, actionButton("confirm_point", "Confirm Selected Location")),
          column(width = 4),
          column(width = 4, downloadButton("download_data", "Download Completed Data"))
        )
      )
    )
  )
)