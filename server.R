library(shiny)
library(leaflet)
library(DT)
library(sf)
library(dplyr)
library(openxlsx)
library(leaflet.extras)

sf_use_s2(FALSE)

# Load data
intervention_areas <- readRDS("data/Shapefiles/intervention_areas.rds")
addresses <- readRDS("data/clean/addresses_list.rds")
geocode_pts <- readRDS("data/clean/addresses_geocode_pts.rds")

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

function(input, output, session) {
  
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
          `Completed?` = ifelse(sn %in% completed_rows$done, "\u2713", "")
        ) %>%
        select(sn, direccion_full, `Completed?`),
      selection = "single",
      options = list(pageLength = 10)
    )
  })
  
  output$selected_address <- renderPrint({
    req(selected_sn())
    addr_row <- addresses %>% filter(sn == selected_sn())
    addr_row$direccion_full 
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addSearchOSM(options = searchOptions(collapsed = FALSE)) %>%
      setView(lng = santa_ana_coords["lng"], lat = santa_ana_coords["lat"], zoom = 12) %>%
      addPolygons(
        data = intervention_mask,
        fillColor = "grey",
        fillOpacity = 0.2,
        stroke = FALSE,
        group = "Mask"
      ) %>%
      addPolygons(
        data = intervention_areas,
        color = "black",
        weight = 1,
        fill = FALSE,
        group = "Intervention Areas"
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
  
  observeEvent(input$confirm_point, {
    sn_val <- selected_sn()
    req(sn_val)
    
    # Save as completed
    completed_rows$done <- unique(c(completed_rows$done, sn_val))
    
    # Save form data and corrected geometry
    confirmed_data$data[[sn_val]] <- list(
      lat = input$correct_lat,
      lng = input$correct_lng,
      location_type = input$location_type,
      notes = input$notes,
      geometry = corrected_point()
    )
    
    # Redraw table with check mark
    output$address_table <- renderDT({
      datatable(
        addresses %>%
          mutate(
            `Completed?` = ifelse(sn %in% completed_rows$done, "\u2713", "")
          ) %>%
          select(sn, direccion_full, `Completed?`),
        selection = "single",
        options = list(pageLength = 10)
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
          latitude = vals$lat,
          longitude = vals$lng,
          location_type = vals$location_type,
          notes = vals$notes,
          timestamp = vals$timestamp
        )
      }) %>% bind_rows() %>%
        left_join(addresses, by=c("sn"))
      
      write.xlsx(df, file)
    }
  )
  
  # Add to confirm_point observer (append inside observer block):
  
  observeEvent(input$confirm_point, {
    sn_val <- selected_sn()
    req(sn_val)
    
    completed_rows$done <- unique(c(completed_rows$done, sn_val))
    
    confirmed_data$data[[sn_val]] <- list(
      lat = input$correct_lat,
      lng = input$correct_lng,
      location_type = input$location_type,
      notes = input$notes,
      geometry = corrected_point(),
      timestamp = Sys.time()
    )
    
    output$address_table <- renderDT({
      datatable(
        addresses %>%
          mutate(
            `Completed?` = ifelse(sn %in% completed_rows$done, "\u2713", "")
          ) %>%
          select(sn, direccion_full, `Completed?`),
        selection = "single",
        options = list(pageLength = 10)
      )
    })
  })
  
}