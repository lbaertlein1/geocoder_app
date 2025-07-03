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

#FOR TESTING
# DROPBOX_UPLOAD_PATH <- "/Apps/apmis_dashboard_log_app/confirmed_data_testing.Rds"
# DROPBOX_DOWNLOAD_URL <- "https://www.dropbox.com/scl/fi/r9bpb5gdxr5k2bjrp1ajs/confirmed_data_testing.Rds?rlkey=7g07zvs3ir783u2migzmlqsz9&st=u5ckcd8o&dl=1"

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
          precision_meters = vals$precision_meters %||% NA_character_,
          not_found = vals$not_found %||% NA_character_,
          notes = vals$notes %||% NA_character_,
          start_time = vals$start_time %||% NA,
          timestamp = vals$timestamp %||% NA,
          user = vals$user %||% NA,
          geometry = vals$geometry %||% NA,
          
          validation_latitude        = vals$validation_latitude %||% NA_real_,
          validation_longitude       = vals$validation_longitude %||% NA_real_,
          validation_timestamp       = vals$validation_timestamp %||% NA,
          validation_user            = vals$validation_user %||% NA_character_,
          validation_location_type   = vals$validation_location_type %||% NA_character_,
          validation_precision_meters       = vals$precision_meters %||% NA_character_,
          validation_not_found       = vals$validation_not_found %||% NA_character_,
          validation_notes           = vals$validation_notes %||% NA_character_,
          validation_start_time      = vals$validation_start_time %||% NA
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

upload_confirmed_data <- function(updated_df) {
  # Step 1: Refresh access token
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
  
  # Step 2: Read existing data from Dropbox
  temp_dl <- tempfile(fileext = ".rds")
  existing_df <- tryCatch({
    httr::GET(DROPBOX_DOWNLOAD_URL, httr::write_disk(temp_dl, overwrite = TRUE))
    readRDS(temp_dl)
  }, error = function(e) {
    tibble()
  })
  
  existing_hash <- digest::digest(existing_df)
  updated_hash  <- digest::digest(updated_df)
  
  if (identical(existing_hash, updated_hash)) {
    message("no upload needed")
    return()
  }
  
  # Step 3: Harmonize columns between existing and updated data
  all_cols <- union(names(existing_df), names(updated_df))
  
  # Add missing columns
  missing_in_existing <- setdiff(all_cols, names(existing_df))
  if (length(missing_in_existing) > 0) {
    existing_df <- dplyr::mutate(existing_df, !!!setNames(rep(list(NA), length(missing_in_existing)), missing_in_existing))
  }
  
  missing_in_updated <- setdiff(all_cols, names(updated_df))
  if (length(missing_in_updated) > 0) {
    updated_df <- dplyr::mutate(updated_df, !!!setNames(rep(list(NA), length(missing_in_updated)), missing_in_updated))
  }
  
  # Reorder columns to match
  existing_df <- existing_df[, all_cols]
  updated_df <- updated_df[, all_cols]
  
  # Coerce types to match existing_df
  # Preserve specific types for known columns
  known_numeric_cols <- c("latitude", "longitude", "validation_latitude", "validation_longitude")
  known_datetime_cols <- c("timestamp", "start_time", "validation_timestamp", "validation_start_time")
  
  for (col in all_cols) {
    # If in known numeric columns
    if (col %in% known_numeric_cols) {
      existing_df[[col]] <- suppressWarnings(as.numeric(existing_df[[col]]))
      updated_df[[col]] <- suppressWarnings(as.numeric(updated_df[[col]]))
    } else if (col %in% known_datetime_cols) {
      existing_df[[col]] <- suppressWarnings(as.POSIXct(existing_df[[col]], origin = "1970-01-01", tz = "UTC"))
      updated_df[[col]] <- suppressWarnings(as.POSIXct(updated_df[[col]], origin = "1970-01-01", tz = "UTC"))
    } else {
      # For all others, coerce to character as fallback
      existing_df[[col]] <- as.character(existing_df[[col]])
      updated_df[[col]] <- as.character(updated_df[[col]])
    }
  }
  
  # Step 4: Merge using rows_upsert
  combined_df <- dplyr::rows_upsert(existing_df, updated_df, by = "sn")

  # Step 5: Upload merged data
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(combined_df, temp_file)
  
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
  unknown    = "Unknown",
  muni_center= "Mun center"
)

# Match with existing levels
accuracy_levels <- c("full_match", "street", "place", "city", "country", "unknown","muni_center")

# Colors for each accuracy level
accuracy_colors <- c(
  full_match = "#00cc00",  # bright green
  street     = "#33ccff",  # bright blue
  place      = "#ffff00",  # yellow
  city       = "#ff9933",  # orange
  country    = "#ff3300",  # red-orange
  unknown    = "#cc00ff",  # magenta
  muni_center= "#00008B"   #darkblue
)

# Color factor for Leaflet
pal <- colorFactor(palette = accuracy_colors, levels = accuracy_levels)
buffer_lookup <- c(
  full_match = 0.001,
  street     = 0.005,
  place      = 0.01,
  city       = 0.02,
  country    = 0.05,
  unknown    = 0.02,
  muni_center= 0.02
)

# ---- Helper functions ----
reset_inputs <- function(session) {
  updateTextInput(session, "correct_lat", value = "")
  updateTextInput(session, "correct_lng", value = "")
  updateTextInput(session, "notes", value = "")
  updateSelectizeInput(session, "not_found", selected="")
  updateSelectizeInput(session, "location_type", selected = "")
  updateSelectizeInput(session, "precision_meters", selected="")
}

clear_session_state <- function(session, corrected_point, reverse_address, selected_lat, selected_lng) {
  corrected_point(NULL)
  reverse_address("")
  selected_lat("")
  selected_lng("")
  reset_inputs(session)
  
  leafletProxy("map") %>%
    clearGroup("Corrected Point") %>%
    clearGroup("buffers") %>%
    clearControls()
  
  # Optional: clear selected row
  # DT::selectRows("address_table", NULL)
}


add_corrected_marker <- function(proxy, data) {
  
  # Define distances and labels
  distances <- c(100, 250, 500, 1000)
  labels <- paste0(distances, "m")
  
  # Project point for buffer creation
  pt_proj <- st_transform(data, 3857)
  
  # Create buffer polygons
  buffers <- lapply(distances, function(d) st_buffer(pt_proj, d))
  
  # Create label points due east
  label_points <- lapply(seq_along(distances), function(i) {
    coords <- st_coordinates(pt_proj)
    label_pt <- st_point(c(coords[1] + distances[i], coords[2]))  # due east
    st_sf(label = labels[i], geometry = st_sfc(label_pt, crs = 3857))
  })
  
  # Transform everything back to WGS84
  buffers_wgs84 <- lapply(buffers, \(b) st_transform(b, 4326))
  label_sf <- do.call(rbind, lapply(label_points, \(p) st_transform(p, 4326)))
  
  proxy %>%
    clearGroup("Corrected Point") %>%
    addAwesomeMarkers(
      data = data,
      icon = awesomeIcons(
        icon = 'star', iconColor = 'black', markerColor = 'yellow', library = 'fa'
      ),
      popup = "Corrected location",
      group = "Corrected Point"
    ) %>%
    addPolygons(data = buffers_wgs84[[1]], group = "buffers", fillOpacity = 0.2, weight = 2, color = "#a50f15") %>%
    addPolygons(data = buffers_wgs84[[2]], group = "buffers", fillOpacity = 0.2, weight = 2, color = "#de2d26") %>%
    addPolygons(data = buffers_wgs84[[3]], group = "buffers", fillOpacity = 0.2, weight = 2, color = "#fb6a4a") %>%
    addPolygons(data = buffers_wgs84[[4]], group = "buffers", fillOpacity = 0.2, weight = 2, color = "#fc9272") %>%
    addLabelOnlyMarkers(
      data = label_sf,
      label = ~label,
      labelOptions = labelOptions(noHide = TRUE, direction = "left", textOnly = TRUE,
                                  style = list("font-weight" = "bold")),
      group = "buffers"
    )
}

render_address_table <- function(addresses, completed, confirmed) {
  # Build a data frame of precision_meters from the confirmed list
  precision_df <- tibble(
    sn = names(confirmed),
    precision_meters = sapply(confirmed, function(x) {
      val <- x$precision_meters %||% NA_character_
      if (is.null(val) || is.na(val) || val == "") "Not Specified" else val
    })
  )
  
  
  temp <- addresses %>%
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
    ungroup()
  
  temp %>%
    left_join(precision_df,
              by=c("sn")) %>%
    select(direccion_full, completed, precision_meters, mun, sn) %>%
    datatable(
      selection = "single",
      filter = "top",
      options = list(
        paging = FALSE,
        scrollY = FALSE,           # Match your desired height
        scrollCollapse = TRUE,
        scroller = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(targets = "_all", className = "dt-left wrap-text"),
          list(targets = 0, width = "50%"),
          list(targets = 1, width = "10%"),
          list(targets = 2, width = "10%"),
          list(targets = 3, width = "10%"),
          list(targets = 4, width = "20%")
        ),
        dom = 'tip'  # include filtering and table only
      ),
      # options = list(
      #   pageLength = 25,
      #   autoWidth = TRUE,
      #   columnDefs = list(
      #     list(targets = "_all", className = "dt-left wrap-text"),
      #     list(targets = 0, width = "50%"),
      #     list(targets = 1, width = "10%"),
      #     list(targets = 2, width = "20%"),
      #     list(targets = 3, width = "20%")
      #   ),
      #   dom = 'tip'
      # ),
      escape = FALSE,
      rownames = FALSE,
      colnames = c("Address", "Done?", "Precision", "Mun.", "ID")
    ) %>%
    formatStyle(
      columns = 1:5,
      fontSize = '10px',
      padding = '1px'
    )
}

# ---- Server ----
server <- function(input, output, session, username, user_group) {
  corrected_point <- reactiveVal(NULL)
  confirmed_data  <- reactiveValues(data = list())
  completed_rows  <- reactiveValues(done = character())
  reverse_address <- reactiveVal("")
  selected_lat    <- reactiveVal("")
  selected_lng    <- reactiveVal("")
  start_timer <- reactiveVal(NULL)
  validate_mode <- reactiveVal(FALSE)
  validation_sn <- reactiveVal(NULL)
  suppress_row_observer <- reactiveVal(FALSE)
  full_data <- reactiveVal(tibble())
  selected_point <- reactiveVal(NULL)
  
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
          precision_meters = row$precision_meters,
          not_found = row$not_found,
          start_time = row$start_time,
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
    if (validate_mode()) {
      return(validation_sn())
    }
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
  output$selected_mun <- renderText({
    req(selected_sn())
    addresses %>% filter(sn == selected_sn()) %>% pull(mun)
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
    
    if (isTRUE(suppress_row_observer())) {
      suppress_row_observer(FALSE)
      return()
    }
    
    # Exit validation mode if user manually selects a row
    validate_mode(FALSE)
    validation_sn(NULL)
    
    req(input$address_table_rows_selected)
    sn_val <- selected_sn()
    if (is.null(sn_val)) return()
    
    #Step 1: Clear previous corrected point from map and inputs
    corrected_point(NULL)
    reverse_address("")
    selected_lat("")
    selected_lng("")
    reset_inputs(session)
    
    leafletProxy("map") %>%
      clearGroup("Corrected Point") %>%
      clearGroup("buffers") %>%
      clearControls()
    
    #Step 2: Sync confirmed data from Dropbox (local-only for current SN)
    latest_confirmed_df <- sync_confirmed_data()
    latest_confirmed_list <- lapply(seq_len(nrow(latest_confirmed_df)), function(i) {
      row <- latest_confirmed_df[i, ]
      list(
        lat = row$latitude,
        lng = row$longitude,
        reverse_address = row$reverse_geocoded_address,
        location_type = row$location_type,
        notes = row$notes,
        precision_meters = row$precision_meters,
        not_found = row$not_found,
        start_time = row$start_time,
        timestamp = row$timestamp,
        user = row$user
      )
    })
    names(latest_confirmed_list) <- latest_confirmed_df$sn
    
    vals <- latest_confirmed_list[[sn_val]]
    if (is.null(vals)) return()
    
    #Step 3: Refill inputs and map if values exist
    reverse_address(vals$reverse_address %||% "")
    selected_lat(as.character(vals$lat))
    selected_lng(as.character(vals$lng))
    updateTextInput(session, "correct_lat", value = vals$lat)
    updateTextInput(session, "correct_lng", value = vals$lng)
    updateTextInput(session, "notes", value = vals$notes)
    updateSelectizeInput(session, "location_type", selected = vals$location_type)
    updateSelectizeInput(session, "not_found", selected = vals$not_found)
    updateSelectizeInput(session, "precision_meters", selected = vals$precision_meters)
    
    if (!is.null(vals$lat) && !is.null(vals$lng) &&
        !is.na(vals$lat) && !is.na(vals$lng)) {
      
      point <- st_sf(geometry = st_sfc(st_point(c(vals$lng, vals$lat)), crs = 4326))
      corrected_point(point)
      add_corrected_marker(leafletProxy("map"), point)
    }
  })
  
  observeEvent(input$address_table_rows_selected, {
    # Set timer only if a row was selected manually
      start_timer(Sys.time())
  })
  
  observeEvent(selected_sn(), {
    req(selected_sn())
    req(!is.null(selected_sn()))
    
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
      clearGroup("buffers") %>%
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
      
      updateSelectizeInput(session, "not_found", selected = "")
      
      # Create corrected point
      point <- st_sf(geometry = st_sfc(st_point(c(click$lng, click$lat)), crs = 4326))
      corrected_point(point)
      
      selected_lat(as.character(click$lat))
      selected_lng(as.character(click$lng))
      
      # Reverse geocode
      url <- sprintf("https://nominatim.openstreetmap.org/reverse?format=json&lat=%s&lon=%s&zoom=18&addressdetails=1", click$lat, click$lng)
      res <- httr::GET(url, user_agent("R Shiny App"))
      rev_data <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), flatten = TRUE)
      reverse_address(rev_data$display_name %||% "Address not found")
      
      updateTextInput(session, "correct_lat", value = as.character(click$lat))
      updateTextInput(session, "correct_lng", value = as.character(click$lng))
      
      # Define distances and labels
      distances <- c(100, 250, 500, 1000)
      labels <- paste0(distances, "m")
      
      # Create projected point
      pt_proj <- st_transform(point, 3857)
      
      # Create buffer polygons
      buffers <- lapply(distances, function(d) st_buffer(pt_proj, d))
      
      # Create label points due east
      label_points <- lapply(seq_along(distances), function(i) {
        coords <- st_coordinates(pt_proj)
        label_pt <- st_point(c(coords[1] + distances[i], coords[2]))  # due east
        st_sf(label = labels[i], geometry = st_sfc(label_pt, crs = 3857))
      })
      
      # Transform to WGS84
      buffers_wgs84 <- lapply(buffers, \(b) st_transform(b, 4326))
      label_sf <- do.call(rbind, lapply(label_points, \(p) st_transform(p, 4326)))
      
      # Update map
      leafletProxy("map") %>%
        clearGroup("buffers") %>%
        addMarkers(data = point, group = "buffers") %>%
        addPolygons(data = buffers_wgs84[[1]], group = "buffers", fillOpacity = 0.2, weight = 2, color = "#a50f15") %>%
        addPolygons(data = buffers_wgs84[[2]], group = "buffers", fillOpacity = 0.2, weight = 2, color = "#de2d26") %>%
        addPolygons(data = buffers_wgs84[[3]], group = "buffers", fillOpacity = 0.2, weight = 2, color = "#fb6a4a") %>%
        addPolygons(data = buffers_wgs84[[4]], group = "buffers", fillOpacity = 0.2, weight = 2, color = "#fc9272") %>%
        addLabelOnlyMarkers(
          data = label_sf,
          label = ~label,
          labelOptions = labelOptions(noHide = TRUE, direction = "left", textOnly = TRUE,
                                      style=list("font-weight" = "bold")),
          group = "buffers"
        )
      
      add_corrected_marker(leafletProxy("map"), point)
    })
  
  output$reverse_geocode_address <- renderText({
    addr <- reverse_address()
    if (is.null(addr) || addr == "") return("")
    addr
  })
  
  output$correct_lat <- renderUI({
    lat <- as.numeric(selected_lat())
    if (is.na(lat) || lat == "") return(HTML("<strong>Correct Lat:</strong>"))
    HTML(paste0("<strong>Correct Lat:</strong> ", round(lat, 3)))
  })
  
  output$correct_lng <- renderUI({
    lng <- as.numeric(selected_lng())
    if (is.na(lng)  || lng == "") return(HTML("<strong>Correct Lon:</strong>"))
    HTML(paste0("<strong>Correct Lon:</strong> ", round(lng, 3)))
  })
  
  observeEvent(input$confirm_point, {
    
    if (!is.na(user_group()) && !is.null(user_group()) && user_group() == "view_only") {
      showNotification("User account is view-only. Data not submitted.", type = "error")
      return(NULL)
    }
    
    sn_val <- selected_sn()
    req(sn_val)
    
    lat_val <- if (!is.null(selected_lat()) && !is.na(selected_lat())) as.numeric(selected_lat()) else NA_real_
    lng_val <- if (!is.null(selected_lng()) && !is.na(selected_lng())) as.numeric(selected_lng()) else NA_real_
    
    # Always get latest data
    existing_df <- sync_confirmed_data()
    
    is_validation <- validate_mode()
    now <- Sys.time()
    
    if (is_validation) {
      # First ensure all validation_* columns exist
      required_validation_cols <- list(
        validation_latitude         = NA_real_,
        validation_longitude        = NA_real_,
        validation_timestamp        = as.POSIXct(NA),
        validation_user             = NA_character_,
        validation_location_type    = NA_character_,
        validation_precision_meters        = NA_character_,
        validation_not_found        = NA_character_,
        validation_notes            = NA_character_,
        validation_start_time       = as.POSIXct(NA)
      )
      
      # Identify which are missing from the existing data
      missing_cols <- setdiff(names(required_validation_cols), names(existing_df))
      
      # Add them safely with mutate
      if (length(missing_cols) > 0) {
        existing_df <- existing_df %>%
          mutate(!!!required_validation_cols[missing_cols])
      }
      
      # Validation mode: update matching row with new validation_* columns
      existing_df <- existing_df %>%
        mutate(
          validation_latitude        = if_else(sn == sn_val, lat_val, validation_latitude),
          validation_longitude       = if_else(sn == sn_val, lng_val, validation_longitude),
          validation_timestamp       = if_else(sn == sn_val, now, validation_timestamp),
          validation_user            = if_else(sn == sn_val, username(), validation_user),
          validation_location_type   = if_else(sn == sn_val, input$location_type, validation_location_type),
          validation_precision_meters       = if_else(sn == sn_val, input$precision_meters, precision_meters),
          validation_not_found       = if_else(sn == sn_val, input$not_found, validation_not_found),
          validation_notes           = if_else(sn == sn_val, input$notes, validation_notes),
          validation_start_time      = if_else(sn == sn_val, start_timer(), validation_start_time)
        )
      
      updated_row <- existing_df %>% filter(sn == sn_val)
    } else {
      # Normal mode: either replace or add new confirmed row
      existing_row <- existing_df %>% filter(sn == sn_val)
      if (nrow(existing_row) == 0) {
        existing_row <- tibble(
          validation_latitude = NA_real_,
          validation_longitude = NA_real_,
          validation_timestamp = as.POSIXct(NA),
          validation_user = NA_character_,
          validation_location_type = NA_character_,
          validation_precision_meters = NA_character_,
          validation_not_found = NA_character_,
          validation_notes = NA_character_,
          validation_start_time = as.POSIXct(NA)
        )
      }
      
      new_row <- tibble(
        sn = sn_val,
        latitude = lat_val,
        longitude = lng_val,
        reverse_geocoded_address = reverse_address(),
        location_type = input$location_type,
        precision_meters = input$precision_meters,
        not_found = input$not_found,
        notes = input$notes,
        start_time = start_timer(),
        timestamp = now,
        user = username(),
        
        # Preserve validation fields if already present
        validation_latitude = existing_row$validation_latitude %||% NA_real_,
        validation_longitude = existing_row$validation_longitude %||% NA_real_,
        validation_timestamp = existing_row$validation_timestamp %||% as.POSIXct(NA),
        validation_user = existing_row$validation_user %||% NA_character_,
        validation_location_type = existing_row$validation_location_type %||% NA_character_,
        validation_precision_meters = existing_row$validation_precision_meters %||% NA_character_,
        validation_not_found = existing_row$validation_not_found %||% NA_character_,
        validation_notes = existing_row$validation_notes %||% NA_character_,
        validation_start_time = existing_row$validation_start_time %||% as.POSIXct(NA)
      )
      
      updated_df <- bind_rows(existing_df, new_row) %>%
        arrange(desc(timestamp)) %>%
        distinct(sn, .keep_all = TRUE)
    }
    
    # Upload updated dataset
    if (is_validation) {
      updated_row <- existing_df %>% filter(sn == sn_val)
      upload_confirmed_data(updated_row)
    } else {
      updated_row <- updated_df %>% filter(sn == sn_val)
      upload_confirmed_data(updated_row)
    }
    
    # Refresh local copy
    latest_df <- sync_confirmed_data()
    confirmed_data$data <- lapply(seq_len(nrow(latest_df)), function(i) {
      row <- latest_df[i, ]
      as.list(row)
    })
    names(confirmed_data$data) <- latest_df$sn
    completed_rows$done <- latest_df$sn
    
    clear_session_state(
      session = session,
      corrected_point = corrected_point,
      reverse_address = reverse_address,
      selected_lat = selected_lat,
      selected_lng = selected_lng
    )
    
    # Clear DataTable selection
    DT::dataTableProxy("address_table") %>% DT::selectRows(NULL)
    
    # Clear all map layers
    leafletProxy("map") %>%
      clearGroup("Corrected Point") %>%
      clearGroup("buffers") %>%
      clearGroup("Geocoded Points") %>%
      clearControls()
    
    validate_mode(FALSE)
    validation_sn(NULL)
    shinyjs::enable("validate_pt")
    DT::dataTableProxy("address_table") %>% DT::selectRows(NULL)
    
    showNotification("Address location successfully saved!", type = "message")
  })
  
  output$download_data <- downloadHandler(
    filename = function() paste0("completed_geocoding_", Sys.Date(), ".xlsx"),
    content = function(file) {
      #Always get latest data from Dropbox (not from session)
      latest_df <- sync_confirmed_data()
      
      # If nothing was confirmed, fall back to raw address list
      if (nrow(latest_df) == 0) {
        df <- addresses
      } else {
        df <- full_join(latest_df, addresses, by = "sn")
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
        clearGroup("Corrected Point") %>%
        clearGroup("buffers")
    }
  })
  
  observeEvent(input$sync_data, {
    latest_df <- sync_confirmed_data()
    
    confirmed_data$data <- lapply(seq_len(nrow(latest_df)), function(i) {
      row <- latest_df[i, ]
      list(
        lat = row$latitude,
        lng = row$longitude,
        reverse_address = row$reverse_geocoded_address,
        location_type = row$location_type,
        notes = row$notes,
        precision_meters = row$precision_meters,
        not_found = row$not_found,
        start_time = row$start_time,
        timestamp = row$timestamp,
        user = row$user
      )
    })
    names(confirmed_data$data) <- latest_df$sn
    completed_rows$done <- latest_df$sn
    
    clear_session_state(
      session = session,
      corrected_point = corrected_point,
      reverse_address = reverse_address,
      selected_lat = selected_lat,
      selected_lng = selected_lng
    )
    
    
    showNotification("Sync with database successful!", type = "message")
  })
  
  observeEvent(input$validate_pt, {
    req(!validate_mode())
    df <- sync_confirmed_data()
    
    # Only pick confirmed points with valid coordinates, from other users
    valid_sn <- df %>%
      filter(user != username()) %>%
      filter(!is.na(timestamp)) %>% 
      filter(is.na(validation_timestamp)) %>%
      pull(sn)
    
    if (length(valid_sn) == 0) {
      showNotification("No confirmed addresses available for validation.", type = "warning")
      return()
    }
    
    # Pick a random SN
    selected <- sample(valid_sn, 1)
    validation_sn(selected)
    validate_mode(TRUE)
    
    # Clear previous values
    reverse_address("")
    selected_lat("")
    selected_lng("")
    corrected_point(NULL)
    
    updateTextInput(session, "correct_lat", value = "")
    updateTextInput(session, "correct_lng", value = "")
    updateTextInput(session, "notes", value = "")
    updateSelectizeInput(session, "not_found", selected = "")
    updateSelectizeInput(session, "location_type", selected = "")
    updateSelectizeInput(session, "precision_meters", selected = "")
    
    leafletProxy("map") %>%
      clearGroup("Corrected Point") %>%
      clearGroup("buffers") %>%
      clearGroup("Geocoded Points") %>%
      clearControls()
    
    suppress_row_observer(TRUE)
    
    # Simulate row selection in the table
    idx <- which(addresses$sn == selected)
    DT::dataTableProxy("address_table") %>% DT::selectRows(idx)
    
    showNotification("Validation address loaded. Please select a location carefully..", type = "message", duration = 2)
    
  })
  
  output$selected_address_label <- renderUI({
    if (validate_mode()) {
      tagList(
        span(
          HTML('<span style="color: #e69f00; font-weight: bold;">Validation:</span>'),
          strong(" Original Address:")
        ),
        textOutput("selected_mun"),
        textOutput("selected_address")
      )
    } else {
      tagList(
        strong("Original Address:"),
        textOutput("selected_address")
      )
    }
  })
  
  output$dashboard_button_ui <- renderUI({
    if (username() %in% c("BypassUser", "Clary", "Carol","vcu","epi") | user_group()=="view_only") {
      actionButton("dashboard", "Dashboard", icon = icon("tachometer-alt"), width = "100%")
    }
  })
  
  observeEvent(input$dashboard, {
    updateTabsetPanel(session, "main_tabs", selected = "dashboard")
  })

  observeEvent(input$return_to_geocoding, {
    updateTabsetPanel(session, "main_tabs", selected = "geocoding")
  })
  
  observeEvent(input$main_tabs, {
    if (input$main_tabs == "dashboard") {
      df <- sync_confirmed_data() %>%
        filter(!(user %in% c("BypassUser", "Carol"))) %>%
        full_join(addresses, by = "sn")
      
      if (!is.data.frame(df) || nrow(df) == 0) {
        full_data(tibble())
        return()
      }
      
      valid_rows <- df %>%
        filter(
          !is.na(latitude), !is.na(longitude),
          !is.na(validation_latitude), !is.na(validation_longitude)
        )
      
      if (nrow(valid_rows) == 0) {
        df$within_1km <- NA
        df$in_agreement <- NA
        df$valid <- NA
        full_data(df)
        return()
      }
      
      original_pts <- st_as_sf(valid_rows, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
      validated_pts <- st_as_sf(valid_rows, coords = c("validation_longitude", "validation_latitude"), crs = 4326, remove=FALSE)
      
      planar_crs <- 32616  # Replace with appropriate UTM zone for your data
      
      original_pts_planar <- st_transform(original_pts, crs = planar_crs)
      validated_pts_planar <- st_transform(validated_pts, crs = planar_crs)
      intervention_areas_planar <- st_transform(intervention_areas, crs = planar_crs)
      
      original_in <- lengths(st_intersects(original_pts_planar, intervention_areas_planar)) > 0
      validated_in <- lengths(st_intersects(validated_pts_planar, intervention_areas_planar)) > 0
      
      dist_m <- geosphere::distHaversine(
        cbind(valid_rows$longitude, valid_rows$latitude),
        cbind(valid_rows$validation_longitude, valid_rows$validation_latitude)
      )
      
      within_1km <- dist_m <= 1000
      in_agreement <- original_in == validated_in
      
      df$within_1km <- NA
      df$in_agreement <- NA
      df$within_1km[match(valid_rows$sn, df$sn)] <- within_1km
      df$in_agreement[match(valid_rows$sn, df$sn)] <- in_agreement
      
      df <- df %>%
        mutate(valid = case_when(
          within_1km & in_agreement ~ 1,
          !within_1km | !in_agreement ~ 0,
          TRUE ~ NA_real_
        ))
      
      full_data(df)
    }
  })
  
  total_addresses <- reactive({
    nrow(addresses)
  })
  
  total_completed <- reactive({
    full_data() %>% filter(!is.na(timestamp)) %>% nrow()
  })
  
  total_validated <- reactive({
    full_data() %>% filter(!is.na(valid)) %>% nrow()
  })
  
  total_valid <- reactive({
    full_data() %>% filter(valid == 1) %>% nrow()
  })
  
  output$card_completed <- renderUI({
    pct <- round(total_completed() / total_addresses() * 100, 1)
    div(
      class = "well",
      h4("Completed Addresses"),
      span(
        strong(paste0(pct, "%")),
        " — ",
        total_completed(),
        "/",
        total_addresses()
      )
    )
  })
  
  output$card_validated <- renderUI({
    completed <- total_completed()
    validated <- total_validated()
    pct <- if (completed > 0)
      round(validated / completed * 100, 1)
    else
      0
    div(
      class = "well",
      h4("Reviewed Addresses (of completed)"),
      span(strong(paste0(pct, "%")), " — ", validated, "/", completed)
    )
  })
  
  output$card_valid <- renderUI({
    validated <- total_validated()
    valid <- total_valid()
    pct <- if (validated > 0)
      round(valid / validated * 100, 1)
    else
      0
    div(
      class = "well",
      h4("Concurrent Addresses (of Reviewed)"),
      span(strong(paste0(pct, "%")), " — ", valid, "/", validated)
    )
  })
  
  output$card_median_distance <- renderUI({
    df <- full_data() %>%
      filter(
        !is.na(longitude), !is.na(latitude),
        !is.na(validation_longitude), !is.na(validation_latitude)
      )
    
    if (nrow(df) == 0) return(NULL)
    
    distances <- geosphere::distHaversine(
      matrix(c(df$longitude, df$latitude), ncol = 2),
      matrix(c(df$validation_longitude, df$validation_latitude), ncol = 2)
    )
    
    median_dist <- median(distances, na.rm = TRUE)
    
    div(
      class = "well",
      style = "height: 45px; padding: 4px 8px; font-size: 12px; line-height: 1.2;",
      div("Median Distance from Original to Review Pt", style = "font-weight: bold;"),
      div(paste0(round(median_dist/1000, 1), " km"))
    )
  })
  
  
  output$card_median_time <- renderUI({
    df <- full_data() %>%
      filter(!is.na(start_time), !is.na(timestamp))
    
    if (nrow(df) == 0) return(NULL)
    
    median_secs <- median(as.numeric(difftime(df$timestamp, df$start_time, units = "secs")), na.rm = TRUE)
    
    div(
      class = "well",
      style = "height: 45px; padding: 4px 8px; font-size: 12px; line-height: 1.2;",
      div("Median Time Spent per Address", style = "font-weight: bold;"),
      div(paste0(round(median_secs), " seconds"))
    )
  })
  
  
  
  output$map_completed <- renderLeaflet({
    req(input$main_tabs == "dashboard")
    
    df <- full_data() %>%
      filter(!is.na(longitude), !is.na(latitude)) %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
    
    req(nrow(df) > 0)
    
    # Define readable status labels
    df$status <- case_when(
      df$valid == 1 ~ "Concurrent",
      df$valid == 0 & df$within_1km == TRUE ~ "Inconcurrent - Int. Area Diff.",
      df$valid == 0 & df$in_agreement == TRUE ~ "Inconcurrent - >1km Diff",
      df$valid == 0 ~ "Inconcurrent - Both Conditions",
      is.na(df$valid) ~ "Not Reviewed"
    )
    
    # Define ordered factor and named color vector
    status_levels <- c(
      "Concurrent",
      "Inconcurrent - Int. Area Diff.",
      "Inconcurrent - >1km Diff",
      "Inconcurrent - Both Conditions",
      "Not Reviewed"
    )
    status_colors <- c(
      "Concurrent"                     = "#377eb8",
      "Inconcurrent - Int. Area Diff." = "#984ea3",
      "Inconcurrent - >1km Diff"       = "#ff7f00",
      "Inconcurrent - Both Conditions" = "#e41a1c",
      "Not Reviewed"                   = "#636363"
    )
    
    df$status <- factor(df$status, levels = status_levels)
    
    pal <- colorFactor(palette = status_colors, domain = names(status_colors))
    
    base <- leaflet(options = leafletOptions(minZoom = 2, maxZoom = 19)) %>%
      addProviderTiles("OpenStreetMap") %>%
      setView(lng = santa_ana_coords[["lng"]],
              lat = santa_ana_coords[["lat"]],
              zoom = 12) %>%
      addPolygons(
        data = intervention_mask,
        fillColor = "grey",
        fillOpacity = 0.2,
        stroke = FALSE,
        group = "Intervention Areas"
      ) %>%
      addPolygons(
        data = intervention_areas,
        color = "black",
        weight = 1,
        fill = FALSE,
        group = "Intervention Areas"
      )
    
    for (status in status_levels) {
      subdf <- df %>%
        filter(status == !!status) %>%
        sf::st_drop_geometry()
      
      if (nrow(subdf) > 0) {
        base <- base %>%
          addCircleMarkers(
            data = subdf,
            lng = ~longitude,
            lat = ~latitude,
            fillColor = status_colors[[status]],
            color = "black",          # Outline color
            weight = 0.8,             # Thin outline
            stroke = TRUE,            # Enable outline
            radius = 5,
            fillOpacity = 0.8,
            label = ~paste0(sn, ": ", direccion_full),  
            group = status
          )
      }
    }
    
    # Add connecting line and validated point if selected
    pt <- selected_point()
    if (!is.null(pt)) {
      base <- base %>%
        addCircleMarkers(
          lng = pt$validation_lng,
          lat = pt$validation_lat,
          color = "black",
          fillColor = "white",
          fillOpacity = 1,
          radius = 4,
          stroke = TRUE,
          weight = 1.5,
          label = "Validated Location",
          group = "Validated Point"
        ) %>%
        addPolylines(
          lng = c(pt$original_lng, pt$validation_lng),
          lat = c(pt$original_lat, pt$validation_lat),
          color = "black",
          weight = 1,
          group = "Validated Line"
        )
    }
    
    base <- base %>%
      addLegend(
        "bottomright",
        colors = unname(status_colors),
        labels = names(status_colors),
        title = NULL
      ) %>%
      addLayersControl(
        overlayGroups = status_levels,
        options = layersControlOptions(collapsed = FALSE)
      )
    
    base
  })
  
  observeEvent(input$map_completed_marker_click, {
    click <- input$map_completed_marker_click
    if (is.null(click)) return()
    
    df <- full_data()
    
    matched <- df %>%
      filter(abs(longitude - click$lng) < 1e-6,
             abs(latitude - click$lat) < 1e-6) %>%
      slice(1)
    
    leafletProxy("map_completed") %>%
      clearGroup("validation_overlay")  # always clear existing
    
    if (nrow(matched) == 1 &&
        !is.na(matched$validation_latitude) &&
        !is.na(matched$validation_longitude)) {
      
      leafletProxy("map_completed") %>%
        addCircleMarkers(
          lng = matched$validation_longitude,
          lat = matched$validation_latitude,
          color = "black",
          fillColor = "red",
          fillOpacity = 1,
          radius = 6,
          stroke = TRUE,
          weight = 2,
          label = "Validated Location",
          group = "validation_overlay"
        ) %>%
        addPolylines(
          lng = c(matched$longitude, matched$validation_longitude),
          lat = c(matched$latitude, matched$validation_latitude),
          color = "black",
          weight = 3,
          group = "validation_overlay"
        )
    }
  })
  
  output$bar_completed_by_status <- renderPlotly({
    df <- full_data() %>%
      filter(!is.na(longitude), !is.na(latitude)) %>%
      mutate(
        status = case_when(
          valid == 1 ~ "Concurrent",
          valid == 0 & within_1km == TRUE ~ "Int. Area Diff.",
          valid == 0 & in_agreement == TRUE ~ ">1km Diff",
          valid == 0 ~ "Both Conditions",
          is.na(valid) ~ "Not Reviewed"
        ),
        status = factor(status, levels = c(
          "Concurrent",
          "Int. Area Diff.",
          ">1km Diff",
          "Both Conditions",
          "Not Reviewed"
        ))
      )
    
    reviewed_df <- df %>%
      filter(status != "Not Reviewed") %>%
      count(status, name = "n")
    
    total_reviewed <- sum(reviewed_df$n)
    reviewed_df <- reviewed_df %>%
      mutate(
        percent = round(n / total_reviewed * 100, 0),
        label = paste0(percent, "%\n(", n, "/\n", total_reviewed, ")"),
        color = c(
          "Concurrent"       = "#377eb8",
          "Int. Area Diff."  = "#984ea3",
          ">1km Diff"        = "#ff7f00",
          "Both Conditions"  = "#e41a1c"
        )[status]
      )
    
    plot_ly(
      data = reviewed_df,
      x = ~status,
      y = ~percent,
      type = "bar",
      text = ~label,
      textposition = "outside",
      textfont = list(size = 8),
      hoverinfo = "text",
      marker = list(
        color = ~color,
        line = list(color = "black", width = 1)
      )
    ) %>%
      layout(
        title = list(text = "Concurrence of Reviewed:", font = list(size = 10)),
        yaxis = list(
          title = list(text = "% of Reviewed", font = list(size = 8)),
          tickfont = list(size = 8),
          range = c(0, 110)
        ),
        xaxis = list(
          title = "",
          tickfont = list(size = 8)
        ),
        uniformtext = list(minsize = 8, mode = "show")
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtons = list(list("toImage"))
      )
  })
  
  output$bar_completed_by_level <- renderPlotly({
    df <- full_data() %>%
      filter(!is.na(timestamp)) %>%
      mutate(location_type = case_when(
        !is.na(location_type) & location_type != "" ~ location_type,
        !is.na(not_found) ~ "Not Found",
        TRUE ~ "Not Specified"
      )) %>%
      count(location_type, name = "n")
    
    total <- sum(df$n)
    
    # Define color map
    color_map <- c(
      "Household" = "#253494",
      "Multi-House Complex" = "#2c7fb8",
      "Street" = "#41b6c4",
      "Neighborhood" = "#7fcdbb",
      "City" = "#c7e9b4",
      "Other" = "#ffffcc",
      "Not Found" = "#de2d26",
      "Not Specified" = "#bdbdbd"
    )
    
    df <- df %>%
      mutate(
        percent = round(n / total * 100, 0),
        label = paste0(percent, "%\n(", n, "/\n", total, ")"),
        color = color_map[location_type],
        location_type = factor(location_type, levels = names(color_map))
      )
    
    plot_ly(
      data = df,
      x = ~location_type,
      y = ~percent,
      type = "bar",
      text = ~label,
      textposition = "outside",
      textfont = list(size = 8),
      hoverinfo = "text",
      marker = list(
        color = ~color,
        line = list(color = "black", width = 1)
      )
    ) %>%
      layout(
        title = list(text = "Location Type:", font = list(size = 10)),
        yaxis = list(
          title = list(text = "% of Addresses", font = list(size = 8)),
          tickfont = list(size = 8),
          range = c(0, 100)
        ),
        xaxis = list(
          title = "",
          tickfont = list(size = 8)
        ),
        uniformtext = list(minsize = 8, mode = "show")
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtons = list(list("toImage"))
      )
  })
  
  
  output$bar_completed_by_precision <- renderPlotly({
    df <- full_data() %>%
      filter(!is.na(timestamp)) %>%
      mutate(
        precision_meters = case_when(
          !is.na(precision_meters) & precision_meters != "" ~ precision_meters,
          !is.na(not_found) & not_found != "" ~ "Not Found",
          TRUE ~ "Not Specified"
        )
      ) %>%
      count(precision_meters, name = "n")
    
    total <- sum(df$n)
    
    # Define color map
    color_map <- c(
      "<100m" = "#253494",
      "<250m" = "#2c7fb8",
      "<500m" = "#41b6c4",
      "<1000m" = "#7fcdbb",
      ">1000m" = "#c7e9b4",
      "Not Specified" = "#de2d26",
      "Not Found" = "grey"
    )
    
    df <- df %>%
      mutate(
        percent = round(n / total * 100, 0),
        label = paste0(percent, "%\n(", n, "/\n", total, ")"),
        color = color_map[precision_meters],
        precision_meters = factor(precision_meters, levels = names(color_map))
      )
    
    plot_ly(
      data = df,
      x = ~precision_meters,
      y = ~percent,
      type = "bar",
      text = ~label,
      textfont = list(size = 8),
      textposition = "outside",
      hoverinfo = "text",
      marker = list(
        color = ~color,
        line = list(color = "black", width = 1)
      )
    ) %>%
      layout(
        title = list(text = "Precision:", font = list(size = 10)),
        yaxis = list(
          title = list(text = "% of Addresses", font = list(size = 8)),
          tickfont = list(size = 8),
          range = c(0, 100)
        ),
        xaxis = list(
          title = "",
          tickfont = list(size = 8)
        ),
        uniformtext = list(minsize = 8, mode = "show")
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtons = list(list("toImage"))
      )
  })
  
  
  output$boxplot_time_by_user <- renderPlotly({
    df <- full_data() %>%
      filter(!is.na(timestamp), !is.na(start_time)) %>%
      mutate(time_secs = as.numeric(difftime(timestamp, start_time, units = "secs"))) %>%
      group_by(user) %>%
      summarise(median_time = median(time_secs, na.rm = TRUE)) %>%
      ungroup()
    
    ymax <- max(df$median_time, na.rm = TRUE) * 1.1  # Add 10% buffer
    
    plot_ly(
      data = df,
      x = ~user,
      y = ~median_time,
      type = "bar",
      text = ~round(median_time),
      textposition = "outside",
      textfont = list(size = 8),
      marker = list(
        line = list(color = "black", width = 1)
      )
    ) %>%
      layout(
        title = list(text = "Median Time per Address", font = list(size = 10)),
        yaxis = list(
          title = list(text = "Seconds", font = list(size = 8)),
          tickfont = list(size = 8),
          range = c(0, ymax)
        ),
        xaxis = list(
          title = "",
          tickfont = list(size = 8)
        ),
        uniformtext = list(minsize = 8, mode = "show")
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtons = list(list("toImage"))
      )
  })
  
  
  output$bar_completed_by_date <- renderPlotly({
    df <- full_data() %>%
      filter(!is.na(timestamp)) %>%
      mutate(
        timestamp = timestamp - hours(6),
        date = as.Date(timestamp)
      ) %>%
      count(date, name = "completed")
    
    ymax <- max(df$completed, na.rm = TRUE) * 1.1  # Add 10% buffer for label space
    
    plot_ly(
      data = df,
      x = ~date,
      y = ~completed,
      type = "bar",
      text = ~completed,
      textposition = "outside",
      textfont = list(size = 8),
      marker = list(
        line = list(color = "black", width = 1)
      )
    ) %>%
      layout(
        title = list(text = "Completed Records by Date", font = list(size = 10)),
        yaxis = list(
          title = list(text = "Total Completed", font = list(size = 8)),
          tickfont = list(size = 8),
          range = c(0, ymax)
        ),
        xaxis = list(
          title = "",
          tickfont = list(size = 8)
        ),
        uniformtext = list(minsize = 8, mode = "show")
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtons = list(list("toImage"))
      )
  })
  
  observeEvent(input$return_to_geocoding, {
    updateTabsetPanel(session, inputId = "main_tabs", selected = "geocoding")
  })
}