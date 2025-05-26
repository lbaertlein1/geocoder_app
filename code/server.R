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

# Load data
intervention_areas <- readRDS("data/Shapefiles/intervention_areas.Rds")
addresses <- readRDS("data/clean/addresses_list.Rds")
geocode_pts <- readRDS("data/clean/addresses_geocode_pts.Rds")

# Create inverse mask
country_outline <- st_read("data/raw/El Salvador_Municipios/WGS84_Municipios.shp") %>%
  st_union() %>%
  st_make_valid()

intervention_union <- st_union(intervention_areas) %>%
  st_make_valid() %>%
  st_transform(st_crs(country_outline))

intervention_mask <- st_difference(country_outline, intervention_union) %>%
  st_make_valid() %>%
  st_as_sf()

santa_ana_coords <- c(lng = -89.681, lat = 13.994)

server <- function(input, output, session) {
  
  corrected_point <- reactiveVal(NULL)
  confirmed_data <- reactiveValues(data = list())
  
  selected_sn <- reactive({
    idx <- input$address_table_rows_selected
    if (length(idx) == 0) return(NULL)
    addresses[idx, "sn", drop = TRUE]
  })
  
  observeEvent(selected_sn(), {
    corrected_point(NULL)  # Clear corrected point on row change
    updateTextInput(session, "correct_lat", value = "")
    updateTextInput(session, "correct_lng", value = "")
    updateTextInput(session, "notes", value = "")
    updateSelectInput(session, "location_type", selected = "")
  })
  
  # Track completed rows (sn values marked as complete)
  completed_rows <- reactiveValues(done = character())
  
  output$address_table <- renderDT({
    datatable(
      addresses %>%
        mutate(
          `Completed?` = ifelse(
            sn %in% completed_rows$done,
            "<i class='fas fa-check-circle' style='font-size: 20px; color: green;'></i>",
            ""
          )
        ) %>%
        select(sn, mun, direccion_full, `Completed?`),
      selection = "single",
      filter = "top", 
      options = list(
        pageLength = 100,
        columnDefs = list(
          list(
            targets = "_all",
            className = "dt-left wrap-text"
          )
        )
      ),
      escape = FALSE,
      rownames = FALSE,
      colnames = c("ID", "Municipality", "Original Address", "Completed?")
    )
  })
  
  output$selected_address <- renderText({
    req(selected_sn())
    addresses %>% filter(sn == selected_sn()) %>% pull(direccion_full)
  })
  
  reverse_address <- reactiveVal("")
  selected_lat <- reactiveVal("")
  selected_lng <- reactiveVal("")
  
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2, maxZoom = 19)) %>%
      # Base layers
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      addProviderTiles("CartoDB.Voyager", group = "CARTO") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      
      addResetMapButton() %>%
      addSearchOSM(options = searchOptions(collapsed = FALSE)) %>%
      addReverseSearchOSM(
        showSearchLocation = FALSE,
        showBounds = FALSE,
        showFeature = FALSE,
        displayText = FALSE,
        fitBounds = FALSE,
        group = "ReverseGeocode"
      ) %>%
      
      setView(lng = santa_ana_coords["lng"], lat = santa_ana_coords["lat"], zoom = 12) %>%
      
      # Combine mask + intervention areas into one overlay group
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
      ) %>%
      
      # Layer controls
      addLayersControl(
        baseGroups = c("OpenStreetMap", "CARTO", "Satellite"),
        overlayGroups = c("Intervention Areas"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  observeEvent(selected_sn(), {
    req(selected_sn())
    
    pts <- geocode_pts %>% filter(sn == selected_sn())
    req(nrow(pts) > 0)
    
    accuracy_levels <- c("full_match", "street", "place", "city", "country", "unknown")
    accuracy_colors <- c(
      "full_match" = "#1a9641",
      "street"     = "#a6d96a",
      "place"      = "#ffffbf",
      "city"       = "#fdae61",
      "country"    = "#d7191c",
      "unknown"    = "#999999"
    )
    pal <- colorFactor(palette = accuracy_colors, levels = accuracy_levels)
    
    buffer_lookup <- c(
      full_match = 0.001,
      street     = 0.005,
      place      = 0.01,
      city       = 0.02,
      country    = 0.05,
      unknown    = 0.02
    )
    pad <- max(buffer_lookup[pts$geocode_accuracy], na.rm = TRUE)
    
    coords <- st_coordinates(pts)
    lng_range <- range(coords[, 1], na.rm = TRUE)
    lat_range <- range(coords[, 2], na.rm = TRUE)
    lng1 <- lng_range[1] - pad
    lng2 <- lng_range[2] + pad
    lat1 <- lat_range[1] - pad
    lat2 <- lat_range[2] + pad
    
    proxy <- leafletProxy("map")
    proxy %>%
      clearSearchOSM() %>%
      clearGroup("Corrected Point") %>%
      clearGroup("Geocoded Points") %>%
      clearControls() %>%
      addCircleMarkers(
        data = pts,
        radius = 7,
        color = "black",
        fillColor = ~pal(geocode_accuracy),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1,
        group = "Geocoded Points",
        popup = ~paste("Geocoded Address:", geocode_match_address, "<br>Accuracy:", geocode_accuracy)
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = pts$geocode_accuracy,
        title = "Geocode Accuracy",
        opacity = 1
      ) %>%
      fitBounds(lng1, lat1, lng2, lat2)
    
    if (!is.null(corrected_point())) {
      proxy %>%
        addAwesomeMarkers(
          data = corrected_point(),
          icon = awesomeIcons(
            icon = 'star',
            iconColor = 'black',
            markerColor = 'yellow',
            library = 'fa'
          ),
          popup = "Corrected location",
          group = "Corrected Point"
        )
    }
    
    # If previously completed, auto-fill form and drop marker
    sn_val <- selected_sn()
    if (!is.null(sn_val) && sn_val %in% names(confirmed_data$data)) {
      vals <- confirmed_data$data[[sn_val]]
      updateTextInput(session, "correct_lat", value = vals$lat)
      updateTextInput(session, "correct_lng", value = vals$lng)
      updateSelectInput(session, "location_type", selected = vals$location_type)
      updateTextInput(session, "notes", value = vals$notes)
      
      # Update map with stored geometry
      leafletProxy("map") %>%
        clearGroup("Corrected Point") %>%
        addAwesomeMarkers(
          data = vals$geometry,
          icon = awesomeIcons(
            icon = 'star',
            iconColor = 'black',
            markerColor = 'yellow',
            library = 'fa'
          ),
          popup = "Corrected location",
          group = "Corrected Point"
        )
    }
  })
  
  observeEvent(input$map_click, {
    click <- input$map_click
    req(click)
    
    corrected_point(
      st_sf(geometry = st_sfc(st_point(c(click$lng, click$lat)), crs = 4326))
    )
    
    selected_lat(as.character(click$lat))
    selected_lng(as.character(click$lng))
    
    # Query Nominatim reverse geocoding API
    url <- sprintf("https://nominatim.openstreetmap.org/reverse?format=json&lat=%s&lon=%s&zoom=18&addressdetails=1",
                   click$lat, click$lng)
    
    res <- httr::GET(url, user_agent("R Shiny App"))
    rev_data <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
    
    # Extract full address if available
    rev_address <- rev_data$display_name %||% "Address not found"
    reverse_address(rev_address)
    selected_lat(as.character(click$lat))
    selected_lng(as.character(click$lng))
    
    updateTextInput(session, "correct_lat", value = as.character(click$lat))
    updateTextInput(session, "correct_lng", value = as.character(click$lng))
    
    leafletProxy("map") %>%
      clearGroup("Corrected Point") %>%
      addAwesomeMarkers(
        data = corrected_point(),
        icon = awesomeIcons(
          icon = 'star',
          iconColor = 'black',
          markerColor = 'yellow',
          library = 'fa'
        ),
        popup = "Corrected location",
        group = "Corrected Point"
      )
    
    
  })
  
  output$reverse_geocode_address <- renderText({
    reverse_address()
  })
  output$correct_lat <- renderText({
    selected_lat()
  })
  
  output$correct_lng <- renderText({
    selected_lng()
  })
  
  observeEvent(input$confirm_point, {
    sn_val <- selected_sn()
    req(sn_val)
    
    # Save as completed
    completed_rows$done <- unique(c(completed_rows$done, sn_val))
    
    # Save form data and corrected geometry
    confirmed_data$data[[sn_val]] <- list(
      lat = as.numeric(selected_lat()),
      lng = as.numeric(selected_lng()),
      reverse_address = reverse_address(),
      location_type = input$location_type,
      notes = input$notes,
      geometry = corrected_point(),
      timestamp = Sys.time()
    )

    # Redraw table with check mark
    output$address_table <- renderDT({
      datatable(
        addresses %>%
          mutate(
            `Completed?` = ifelse(
              sn %in% completed_rows$done,
              "<i class='fas fa-check-circle' style='font-size: 20px; color: green;'></i>",
              ""
            )
          ) %>%
          select(sn, mun, direccion_full, `Completed?`),
        selection = "single",
        filter = "top",  # Enables filter input row
        options = list(
          pageLength = 100,
          columnDefs = list(
            list(
              targets = "_all",
              className = "dt-left wrap-text"
            )
          )
        ),
        escape = FALSE,
        rownames = FALSE,
        colnames = c("ID", "Municipality", "Original Address", "Completed?")
      )
    })
    
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("completed_geocoding_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(length(confirmed_data$data) > 0)
      
      df <- lapply(names(confirmed_data$data), function(sn) {
        vals <- confirmed_data$data[[sn]]
        tibble(
          sn = sn,
          latitude = if (is.null(vals$lat)) NA_real_ else vals$lat,
          longitude = if (is.null(vals$lng)) NA_real_ else vals$lng,
          reverse_geocoded_address = if (is.null(vals$reverse_address)) NA_character_ else vals$reverse_address,
          location_type = if (is.null(vals$location_type)) NA_character_ else vals$location_type,
          notes = if (is.null(vals$notes)) NA_character_ else vals$notes,
          timestamp = if (is.null(vals$timestamp)) NA else vals$timestamp
        )
      }) %>% bind_rows()
      
      if (nrow(df) == 0) return(NULL)
      
      df <- left_join(df, addresses, by = "sn")
      write.xlsx(df, file)
    }
  )
  
  # Add to confirm_point observer (append inside observer block):
  
  observeEvent(input$confirm_point, {
    sn_val <- selected_sn()
    req(sn_val)
    
    completed_rows$done <- unique(c(completed_rows$done, sn_val))
    
    confirmed_data$data[[sn_val]] <- list(
      lat = as.numeric(selected_lat()),
      lng = as.numeric(selected_lng()),
      reverse_address = reverse_address(),
      location_type = input$location_type,
      notes = input$notes,
      geometry = corrected_point(),
      timestamp = Sys.time()
    )
    
    output$address_table <- renderDT({
      datatable(
        addresses %>%
          mutate(
            `Completed?` = ifelse(
              sn %in% completed_rows$done,
              "<i class='fas fa-check-circle' style='font-size: 20px; color: green;'></i>",
              ""
            )
          ) %>%
          select(sn, mun, direccion_full, `Completed?`),
        selection = "single",
        filter = "top",
        options = list(
          pageLength = 100,
          columnDefs = list(
            list(
              targets = "_all",
              className = "dt-left wrap-text"
            )
          )
        ),
        escape = FALSE,
        rownames = FALSE,
        colnames = c("ID", "Municipality", "Original Address", "Completed?")
      )
    })
  })
  
  observe({
    if (is.null(selected_sn())) {
      corrected_point(NULL)
      reverse_address("")
      selected_lat("")
      selected_lng("")
      
      updateTextInput(session, "correct_lat", value = "")
      updateTextInput(session, "correct_lng", value = "")
      updateTextInput(session, "notes", value = "")
      updateSelectInput(session, "location_type", selected = "")
    }
  })
  observeEvent(input$address_table_rows_selected, {
    corrected_point(NULL)
    reverse_address("")
    selected_lat("")
    selected_lng("")
    
    updateTextInput(session, "correct_lat", value = "")
    updateTextInput(session, "correct_lng", value = "")
    updateTextInput(session, "notes", value = "")
    updateSelectInput(session, "location_type", selected = "")
    
    sn_val <- selected_sn()
    if (!is.null(sn_val) && sn_val %in% names(confirmed_data$data)) {
      vals <- confirmed_data$data[[sn_val]]
      
      reverse_address(vals$reverse_address %||% "")
      selected_lat(as.character(vals$lat))
      selected_lng(as.character(vals$lng))
      
      updateTextInput(session, "correct_lat", value = vals$lat)
      updateTextInput(session, "correct_lng", value = vals$lng)
      updateTextInput(session, "notes", value = vals$notes)
      updateSelectInput(session, "location_type", selected = vals$location_type)
      
      leafletProxy("map") %>%
        clearGroup("Corrected Point") %>%
        addAwesomeMarkers(
          data = vals$geometry,
          icon = awesomeIcons(
            icon = 'star',
            iconColor = 'black',
            markerColor = 'yellow',
            library = 'fa'
          ),
          popup = "Corrected location",
          group = "Corrected Point"
        )
    }
  })
  
  observe({
    if (length(confirmed_data$data) > 0) {
      shinyjs::enable("download_data")
    } else {
      shinyjs::disable("download_data")
    }
  })
}
