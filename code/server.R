sf_use_s2(FALSE)

# ---- Load data ----
intervention_areas <- st_as_sf(readRDS("data/Shapefiles/intervention_buffer.Rds"))
intervention_mask  <- st_as_sf(readRDS("data/Shapefiles/intervention_mask.Rds"))
addresses          <- readRDS("data/clean/addresses_list.Rds")
geocode_pts        <- readRDS("data/clean/addresses_geocode_pts.Rds")
santa_ana_coords   <- c(lng = -89.604, lat = 13.982)



# ---- Dropbox Sync Helpers ----
# Import and convert to named list
dropbox_keys <- rio::import("data/account_info.xlsx", which = "Dropbox") %>%
  select(name, value) %>%
  deframe()

# Access each variable
DROPBOX_DOWNLOAD_URL  <- dropbox_keys[["DROPBOX_DOWNLOAD_URL"]]
DROPBOX_UPLOAD_URL    <- dropbox_keys[["DROPBOX_UPLOAD_URL"]]
DROPBOX_UPLOAD_PATH   <- dropbox_keys[["DROPBOX_UPLOAD_PATH"]]
DROPBOX_REFRESH_TOKEN <- dropbox_keys[["DROPBOX_REFRESH_TOKEN"]]
DROPBOX_APP_KEY       <- dropbox_keys[["DROPBOX_APP_KEY"]]
DROPBOX_APP_SECRET    <- dropbox_keys[["DROPBOX_APP_SECRET"]]

# Load and merge confirmed data from Dropbox
sync_confirmed_data <- function() {
  temp_file <- tempfile(fileext = ".rds")
  tryCatch({
    httr::GET(DROPBOX_DOWNLOAD_URL, httr::write_disk(temp_file, overwrite = TRUE))
    raw_data <- readRDS(temp_file)
    
    # If it's already a data frame, return it
    if (is.data.frame(raw_data)) return(as_tibble(raw_data))
    
    # If it's a list of lists (named by SN), convert to tibble
    if (is.list(raw_data) && all(sapply(raw_data, is.list))) {
      df <- lapply(names(raw_data), function(sn) {
        vals <- raw_data[[sn]]
        tibble(
          sn = sn,
          latitude = vals$lat %||% NA_real_,
          longitude = vals$lng %||% NA_real_,
          reverse_geocoded_address = vals$reverse_address %||% NA_character_,
          location_type = vals$location_type %||% NA_character_,
          not_found = vals$not_found %||% NA_character_,
          notes = vals$notes %||% NA_character_,
          timestamp = vals$timestamp %||% NA,
          user = vals$user %||% NA,
          geometry = vals$geometry %||% NA
        )
      }) %>% bind_rows()
      return(df)
    }
    
    # Default fallback
    tibble()
    
  }, error = function(e) {
    message("Dropbox sync failed: ", e$message)
    tibble()
  })
}
# Upload updated confirmed data to Dropbox
upload_confirmed_data <- function(updated_df) {
  # Refresh access token
  token_res <- httr::POST(
    url = "https://api.dropboxapi.com/oauth2/token",
    body = list(
      grant_type = "refresh_token",
      refresh_token = DROPBOX_REFRESH_TOKEN,
      client_id = DROPBOX_APP_KEY,
      client_secret = DROPBOX_APP_SECRET
    ),
    encode = "form"
  )
  token <- httr::content(token_res)$access_token
  if (is.null(token)) {
    message("Dropbox token refresh failed.")
    return()
  }
  
  # Write to temp file and upload
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(updated_df, temp_file)
  
  tryCatch({
    httr::POST(
      DROPBOX_UPLOAD_URL,
      httr::add_headers(
        Authorization = paste("Bearer", token),
        `Content-Type` = "application/octet-stream",
        `Dropbox-API-Arg` = jsonlite::toJSON(
          list(
            path = DROPBOX_UPLOAD_PATH,
            mode = "overwrite",
            autorename = FALSE,
            mute = TRUE
          ), auto_unbox = TRUE
        )
      ),
      body = httr::upload_file(temp_file)
    )
  }, error = function(e) {
    message("Dropbox upload failed: ", e$message)
  })
}


# ---- Shared palette + buffer size ----
# Labels for legend and user display
accuracy_labels <- c(
  full_match = "Full Match",
  street     = "Street Level",
  place      = "Place or Landmark",
  city       = "City or Town",
  country    = "Country",
  unknown    = "Unknown"
)

# Match with existing levels
accuracy_levels <- c("full_match", "street", "place", "city", "country", "unknown")

# Colors for each accuracy level
accuracy_colors <- c(
  full_match = "#00cc00",  # bright green
  street     = "#33ccff",  # bright blue
  place      = "#ffff00",  # yellow
  city       = "#ff9933",  # orange
  country    = "#ff3300",  # red-orange
  unknown    = "#cc00ff"   # magenta
)

# Color factor for Leaflet
pal <- colorFactor(palette = accuracy_colors, levels = accuracy_levels)
buffer_lookup <- c(
  full_match = 0.001,
  street     = 0.005,
  place      = 0.01,
  city       = 0.02,
  country    = 0.05,
  unknown    = 0.02
)

# ---- Helper functions ----
reset_inputs <- function(session) {
  updateTextInput(session, "correct_lat", value = "")
  updateTextInput(session, "correct_lng", value = "")
  updateTextInput(session, "notes", value = "")
  updateSelectizeInput(session, "not_found", selected="")
  updateSelectizeInput(session, "location_type", selected = "")
}

add_corrected_marker <- function(proxy, data) {
  proxy %>%
    clearGroup("Corrected Point") %>%
    addAwesomeMarkers(
      data = data,
      icon = awesomeIcons(
        icon = 'star', iconColor = 'black', markerColor = 'yellow', library = 'fa'
      ),
      popup = "Corrected location",
      group = "Corrected Point"
    )
}

render_address_table <- function(addresses, completed, confirmed) {
  addresses %>%
    rowwise() %>%
    mutate(
      completed = case_when(
        sn %in% completed ~ {
          val <- confirmed[[sn]]
          if (!is.null(val$not_found) && val$not_found != "") "Not Found" else "Yes"
        },
        TRUE ~ "No"
      )
    ) %>%
    ungroup() %>%
    select(direccion_full, completed, mun, sn) %>%
    datatable(
      selection = "single",
      filter = "top",
      options = list(
        pageLength = 25,
        autoWidth = TRUE,
        columnDefs = list(
          list(targets = "_all", className = "dt-left wrap-text"),
          list(targets = 0, width = "50%"),
          list(targets = 1, width = "10%"),
          list(targets = 2, width = "30%"),
          list(targets = 3, width = "10%")
        ),
        dom = 'tip'
      ),
      escape = FALSE,
      rownames = FALSE,
      colnames = c("Address", "Done?", "Mun.", "ID")
    ) %>%
    formatStyle(
      columns = 1:4,
      fontSize = '10px',
      padding = '1px'
    )
}

# ---- Server ----
server <- function(input, output, session, username) {
  corrected_point <- reactiveVal(NULL)
  confirmed_data  <- reactiveValues(data = list())
  completed_rows  <- reactiveValues(done = character())
  reverse_address <- reactiveVal("")
  selected_lat    <- reactiveVal("")
  selected_lng    <- reactiveVal("")
  
  # ---- Load completed data from Dropbox on startup ----
  observe({
    initial_confirmed_df <- sync_confirmed_data()
    
    isolate({
      confirmed_data$data <- lapply(seq_len(nrow(initial_confirmed_df)), function(i) {
        row <- initial_confirmed_df[i, ]
        list(
          lat = row$latitude,
          lng = row$longitude,
          reverse_address = row$reverse_geocoded_address,
          location_type = row$location_type,
          notes = row$notes,
          not_found = row$not_found,
          timestamp = row$timestamp,
          user = row$user
        )
      })
      names(confirmed_data$data) <- initial_confirmed_df$sn
      completed_rows$done <- initial_confirmed_df$sn
    })
  })
  
  
  shinyjs::disable("confirm_point")
  
  selected_sn <- reactive({
    idx <- input$address_table_rows_selected
    if (length(idx) == 0) return(NULL)
    addresses[idx, "sn", drop = TRUE]
  })
  
  output$address_table <- renderDT({
    render_address_table(addresses, completed_rows$done, confirmed_data$data)
  })
  
  output$selected_address <- renderText({
    req(selected_sn())
    addresses %>% filter(sn == selected_sn()) %>% pull(direccion_full)
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2, maxZoom = 19)) %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      addProviderTiles("CartoDB.Voyager", group = "CARTO") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addResetMapButton() %>%
      addSearchOSM(options = searchOptions(collapsed = FALSE)) %>%
      addReverseSearchOSM(
        showSearchLocation = FALSE, showBounds = FALSE, showFeature = FALSE,
        displayText = FALSE, fitBounds = FALSE, group = "ReverseGeocode"
      ) %>%
      setView(lng = santa_ana_coords[["lng"]], lat = santa_ana_coords[["lat"]], zoom = 12) %>%
      addPolygons(data = intervention_mask, fillColor = "grey", fillOpacity = 0.2, stroke = FALSE, group = "Intervention Areas") %>%
      addPolygons(data = intervention_areas, color = "black", weight = 1, fill = FALSE, group = "Intervention Areas") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "CARTO", "Satellite"),
        overlayGroups = c("Intervention Areas"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observeEvent(input$address_table_rows_selected, {
    confirmed_df <- sync_confirmed_data()
    sn_val <- selected_sn()
    
    # Clear state
    corrected_point(NULL)
    reverse_address("")
    selected_lat("")
    selected_lng("")
    reset_inputs(session)
    
    # clear previous marker
    leafletProxy("map") %>%
      clearGroup("Corrected Point")
    
    sn_val <- selected_sn()
    reset_inputs(session)
    
    if (!is.null(sn_val) && sn_val %in% names(confirmed_data$data)) {
      vals <- confirmed_data$data[[sn_val]]
      reverse_address(vals$reverse_address %||% "")
      selected_lat(as.character(vals$lat))
      selected_lng(as.character(vals$lng))
      updateTextInput(session, "correct_lat", value = vals$lat)
      updateTextInput(session, "correct_lng", value = vals$lng)
      updateTextInput(session, "notes", value = vals$notes)
      updateSelectizeInput(session, "location_type", selected = vals$location_type)
      updateSelectizeInput(session, "not_found", selected = vals$not_found)

      corrected_point(NULL)
      if (!is.null(vals$lat) && !is.null(vals$lng) &&
          !is.na(vals$lat) && !is.na(vals$lng)) {
        
        point <- st_sf(geometry = st_sfc(st_point(c(vals$lng, vals$lat)), crs = 4326))
        corrected_point(point)
        add_corrected_marker(leafletProxy("map"), point)
      }
    }
  })
  
  observeEvent(selected_sn(), {
    req(selected_sn())
    pts <- geocode_pts %>% filter(sn == selected_sn())
    req(nrow(pts) > 0)
    
    pad <- max(buffer_lookup[pts$geocode_accuracy], na.rm = TRUE)
    coords <- st_coordinates(pts)
    bounds <- list(
      lng1 = min(coords[,1]) - pad,
      lng2 = max(coords[,1]) + pad,
      lat1 = min(coords[,2]) - pad,
      lat2 = max(coords[,2]) + pad
    )
    
    proxy <- leafletProxy("map") %>%
      clearSearchOSM() %>%
      clearGroup("Corrected Point") %>%
      clearGroup("Geocoded Points") %>%
      clearControls() %>%
      addCircleMarkers(
        data = pts, radius = 8, 
        color = "black",
        weight = 3,
        fillColor = ~pal(geocode_accuracy), fillOpacity = 0.8,
        stroke = TRUE, 
        group = "Geocoded Points",
        popup = ~paste("Geocoded Address:", geocode_match_address, "<br>Accuracy:", geocode_accuracy)
      ) %>%
      addLegend(
        "bottomright",
        colors = accuracy_colors,
        labels = accuracy_labels,
        title = "Geocode Accuracy:",
        opacity = 1
      ) %>%
      fitBounds(bounds$lng1, bounds$lat1, bounds$lng2, bounds$lat2)
    
    if (!is.null(corrected_point())) {
      add_corrected_marker(proxy, corrected_point())
    }
  })
  
  observeEvent(input$map_click, {
    click <- input$map_click
    req(click)
    
    # Clear 'Address Not Found' dropdown when a point is selected
    updateSelectizeInput(session, "not_found", selected = "")
    
    # Set new corrected point
    corrected_point(
      st_sf(geometry = st_sfc(st_point(c(click$lng, click$lat)), crs = 4326))
    )
    
    selected_lat(as.character(click$lat))
    selected_lng(as.character(click$lng))
    
    # Reverse geocode
    url <- sprintf("https://nominatim.openstreetmap.org/reverse?format=json&lat=%s&lon=%s&zoom=18&addressdetails=1", click$lat, click$lng)
    res <- httr::GET(url, user_agent("R Shiny App"))
    rev_data <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), flatten = TRUE)
    reverse_address(rev_data$display_name %||% "Address not found")
    
    updateTextInput(session, "correct_lat", value = as.character(click$lat))
    updateTextInput(session, "correct_lng", value = as.character(click$lng))
    
    add_corrected_marker(leafletProxy("map"), corrected_point())
  })
  
  output$reverse_geocode_address <- renderText({ reverse_address() })
  
  output$correct_lat <- renderUI({
    lat <- as.numeric(selected_lat())
    if (is.na(lat)) return(HTML("<strong>Correct Lat:</strong>"))
    HTML(paste0("<strong>Correct Lat:</strong> ", round(lat, 3)))
  })
  
  output$correct_lng <- renderUI({
    lng <- as.numeric(selected_lng())
    if (is.na(lng)) return(HTML("<strong>Correct Lon:</strong>"))
    HTML(paste0("<strong>Correct Lon:</strong> ", round(lng, 3)))
  })
  
  observeEvent(input$confirm_point, {
    sn_val <- selected_sn()
    req(sn_val)
    
    lat_val <- if (!is.null(selected_lat()) && !is.na(selected_lat())) as.numeric(selected_lat()) else NA_real_
    lng_val <- if (!is.null(selected_lng()) && !is.na(selected_lng())) as.numeric(selected_lng()) else NA_real_
    
    confirmed_data$data[[sn_val]] <- list(
      lat = lat_val,
      lng = lng_val,
      reverse_address = reverse_address(),
      location_type = input$location_type,
      notes = input$notes,
      not_found = input$not_found,
      timestamp = Sys.time(),
      user = username()
    )
    
    completed_rows$done <- unique(c(completed_rows$done, sn_val))
    
    output$address_table <- renderDT({
      render_address_table(addresses, completed_rows$done, confirmed_data$data)
    })
    
    df <- lapply(names(confirmed_data$data), function(sn) {
      vals <- confirmed_data$data[[sn]]
      tibble(
        sn = sn,
        latitude = vals$lat %||% NA_real_,
        longitude = vals$lng %||% NA_real_,
        reverse_geocoded_address = vals$reverse_address %||% NA_character_,
        location_type = vals$location_type %||% NA_character_,
        not_found = vals$not_found %||% NA_character_,
        notes = vals$notes %||% NA_character_,
        timestamp = vals$timestamp %||% NA,
        user = vals$user %||% NA,
      )
    }) %>% bind_rows()
    
    upload_confirmed_data(df)
  })
  
  output$download_data <- downloadHandler(
    filename = function() paste0("completed_geocoding_", Sys.Date(), ".xlsx"),
    content = function(file) {
      df <- lapply(names(confirmed_data$data), function(sn) {
        vals <- confirmed_data$data[[sn]]
        tibble(
          sn = sn,
          latitude = vals$lat %||% NA_real_,
          longitude = vals$lng %||% NA_real_,
          reverse_geocoded_address = vals$reverse_address %||% NA_character_,
          location_type = vals$location_type %||% NA_character_,
          not_found = vals$not_found %||% NA_character_,
          notes = vals$notes %||% NA_character_,
          timestamp = vals$timestamp %||% NA,
          user = vals$user %||% NA
        )
      }) %>% bind_rows()
      
      if(nrow(df) == 0){
        df <- addresses
      } else{
        df <- full_join(df, addresses, by = "sn")
      }
      writexl::write_xlsx(df, file)
    }
  )
  
  output$download_sop <- downloadHandler(
    filename = function() "wolbachia_project_geocoding_sop.docx",
    content = function(file) file.copy("www/wolbachia_project_geocoding_sop.docx", file),
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
  
  observe({
    if (length(confirmed_data$data) > 0) {
      shinyjs::enable("download_data")
    } else {
      shinyjs::disable("download_data")
    }
  })
  
  observe({
    has_point <- !is.null(corrected_point())
    has_flag  <- !is.null(input$not_found) && input$not_found != ""
    
    if (has_point || has_flag) {
      shinyjs::enable("confirm_point")
    } else {
      shinyjs::disable("confirm_point")
    }
  })
  
  observeEvent(input$not_found, {
    if (!is.null(input$not_found) && input$not_found != "") {
      # Clear corrected point
      corrected_point(NULL)
      reverse_address("")
      selected_lat("")
      selected_lng("")
      
      # Clear text inputs and map marker
      updateTextInput(session, "correct_lat", value = "")
      updateTextInput(session, "correct_lng", value = "")
      
      leafletProxy("map") %>%
        clearGroup("Corrected Point")
    }
  })
}