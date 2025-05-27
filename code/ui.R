

ui <- fluidPage(
  useShinyjs(),
    tags$head(
      tags$style(HTML("
    html, body {
      height: 100%;
      margin: 0;
      padding: 0;
      background-color: #f0f2f5;
      overflow-x: hidden;
    }

    .map-box {
      height: calc(98vh - 8px);
      background-color: white;
      border: 1px solid #ddd;
      border-radius: 6px;
      margin: 4px;
      padding: 0;
      box-shadow: 0 1px 3px rgba(0,0,0,0.1);
    }

    .twothirds-height {
      height: calc(54vh - 6px);
      overflow-y: auto;
      overflow-x: hidden;
      background-color: white;
      border: 1px solid #ddd;
      border-radius: 6px;
      margin: 4px;
      padding: 10px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.1);
    }
    
    .onethird-height {
        height: calc(44vh - 6px);
        overflow-y: auto;
        overflow-x: hidden;
        background-color: white;
        border: 1px solid #ddd;
        border-radius: 6px;
        margin: 4px;
        padding: 10px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        font-size: 10pt;
        position: relative;
      }
      
      .onethird-height .form-control,
      .onethird-height .shiny-text-output,
      .onethird-height label,
      .onethird-height .btn,
      .onethird-height .selectize-control,
      .onethird-height .selectize-input,
      .onethird-height .selectize-dropdown-content {
          font-size: 10pt !important;  /* Reduced from 10pt */
      }
      
      .onethird-height .btn {
          padding: 4px 8px;
          text-align: left !important;
      }
      
      .onethird-height strong {
        font-size: 10pt !important;
      }

    .leaflet-container {
      height: 100% !important;
      border-radius: 6px;
    }

    .row.no-gutters > [class^='col-'] {
      padding-left: 4px !important;
      padding-right: 4px !important;
    }

    .container-fluid {
      padding-left: 4px !important;
      padding-right: 4px !important;
    }
    
    .shiny-download-link {
        width: 100% !important;
        display: block;
    }
    
    table.dataTable thead th {
    font-size: 10px !important;
    }
    
    .btn-lightgreen {
    background-color: #d4edda !important;
    color: #155724 !important;
    border: 1px solid #c3e6cb;
     }
  ")),
    tags$link(
      rel = "stylesheet",
      href = "https://use.fontawesome.com/releases/v5.15.4/css/all.css",
      integrity = "sha384-jLKHWMZPppYjH6v2KrWy2z6q9zjlKhOQZBxzSCTxnYJvoRxTSo2q9Fh41Zk5zw1T",
      crossorigin = "anonymous"
    )
  ),
  
  fluidRow(class = "no-gutters",
           column(
             width = 8,
             div(
               class = "map-box",
               leafletOutput("map", height = "100%")
             )
           ),
           column(
             width = 4,
             div(
               class = "twothirds-height",
               DTOutput("address_table")
             ),
             div(
               class = "onethird-height",
               fluidRow(
                 column(width = 12, strong("Original Address:"), textOutput("selected_address"))
               ),
               fluidRow(
                 column(width = 12, strong("Reverse-Geocoded Address:"), textOutput("reverse_geocode_address"))
               ),
               fluidRow(
                 column(width = 6, uiOutput("correct_lat")),
                 column(width = 6, uiOutput("correct_lng"))
               ),
               div(style = "margin-top: 2px;"),
               fluidRow(
                 column(
                   width = 6,
                   selectizeInput(
                     "location_type",
                     label = NULL,
                     choices = c("", "Household", "Multi-House Complex", "Street", "Neighborhood", "City", "Other"),
                     options = list(
                       placeholder = "Location Type:",
                       dropdownParent = 'body'
                     )
                   )
                  ),
                 column(
                   width = 6,
                   selectizeInput(
                     "not_found", 
                     label = NULL,
                     choices = c(
                       "",
                       "In intervention area",
                       "Out of intervention area",
                       "Unknown"
                     ),
                     options = list(
                       placeholder = "Address Not Found?",
                       dropdownParent = 'body'
                     )
                   )
                 )
               ),
               fluidRow(
                 column(
                   width = 6,
                   textInput("notes", label = NULL, placeholder = "Notes:")
                 ),
                 column(width = 6, actionButton("confirm_point", "Confirm Selected Location", width="100%", class = "btn-lightgreen")),
                 
               ),
               div(style = "margin-top: 2px;"),
               fluidRow(
                 column(width = 6, downloadButton("download_data", "Completed Data", width="100%")),
                 column(width = 6, downloadButton("download_sop", "Geocoding SOP", width="100%"))
               )
           )
  )
  
))
