library(shiny)
library(shinyjs)
library(DT)
library(httr)
library(jsonlite)
# Clinical address resolver.
# Address and coordinate values exist only in the current Shiny session.

DEFAULT_CENTER <- c(lng = -91.86426, lat = 14.70413) # Coatepeque, Quetzaltenango
NOMINATIM_URL <- "https://nominatim.openstreetmap.org"
geocoder_user_agent <- "GuatemalaClinicalAddressResolver/0.1 (contact project administrator)"
SEARCH_ENVELOPE_KM <- 100
LOCALITY_PRIORITY_RADIUS_KM <- 15
SEARCH_LIMIT <- 10
MUNICIPALITY_WARNING_DISTANCE_KM <- 25
DEFAULT_LOCALITY <- "Coatepeque, Guatemala"
KNOWN_LOCALITY_HINTS <- c(
  "coatepeque", "flores costa cuca", "retalhuleu", "mazatenango",
  "quetzaltenango", "genova", "el quetzal", "la democracia", "malacatan",
  "ocós", "ocos", "san sebastian", "la blanca", "nuevo progreso",
  "la reforma", "pajapita", "tecun", "tecún umán", "colomba",
  "guatemala"
)

LOCATION_TYPE_LABELS <- c(
  "Primary sleeping place" = "Lugar donde durmió principalmente",
  "Work" = "Trabajo",
  "School" = "Escuela",
  "Market" = "Mercado",
  "Family/friend home" = "Casa familiar/amigo",
  "Church" = "Iglesia",
  "Recreation/play" = "Recreación/Juego",
  "Other" = "Otro"
)
LOCATION_TYPE_CHOICES_ES <- stats::setNames(names(LOCATION_TYPE_LABELS), unname(LOCATION_TYPE_LABELS))
LOCATION_TYPE_LABELS_EN <- c(
  "Primary sleeping place" = "Primary sleeping place",
  "Work" = "Work",
  "School" = "School",
  "Market" = "Market",
  "Family/friend home" = "Family/friend home",
  "Church" = "Church",
  "Recreation/play" = "Recreation/play",
  "Other" = "Other"
)
LOCATION_TYPE_CHOICES_EN <- stats::setNames(names(LOCATION_TYPE_LABELS_EN), unname(LOCATION_TYPE_LABELS_EN))

MUNICIPALITY_SEARCH_NAMES <- c(
  "Coatepeque" = "Coatepeque, Guatemala",
  "La Blanca" = "La Blanca, Guatemala",
  "Retalhuleu" = "Retalhuleu, Guatemala",
  "Nuevo Progreso" = "Nuevo Progreso, Guatemala",
  "La Reforma" = "La Reforma, Guatemala",
  "El Quetzal" = "El Quetzal, Guatemala",
  "Genova C.C." = "Génova Costa Cuca, Guatemala",
  "Flores C.C." = "Flores Costa Cuca, Guatemala",
  "Colomba C.C." = "Colomba Costa Cuca, Guatemala",
  "Pajapita" = "Pajapita, Guatemala",
  "Ocos" = "Ocós, Guatemala",
  "Tecun" = "Tecún Umán, Guatemala",
  "Other" = ""
)

# Used only as an offline fallback when Search is pressed with no address.
# These are municipality-seat reference points, not address matches.
MUNICIPALITY_FALLBACK_POINTS <- data.frame(
  municipality = c("Coatepeque", "La Blanca", "Retalhuleu"),
  lat = c(14.70413, 14.57917, 14.53611),
  lon = c(-91.86426, -92.14139, -91.67778),
  stringsAsFactors = FALSE
)

APP_ENV_FILE <- if (file.exists(file.path("geocoder_clinical", ".env"))) {
  file.path("geocoder_clinical", ".env")
} else {
  ".env"
}

load_dotenv_key <- function(name, path = APP_ENV_FILE) {
  existing <- Sys.getenv(name, unset = "")
  if (nzchar(existing) || !file.exists(path)) return(existing)
  lines <- readLines(path, warn = FALSE)
  match <- grep(paste0("^\\s*", name, "\\s*="), lines, value = TRUE)
  if (length(match) == 0) return("")
  value <- sub(paste0("^\\s*", name, "\\s*=\\s*"), "", match[[1]])
  sub("^['\"]|['\"]$", "", value)
}

GOOGLE_MAPS_API_KEY <- load_dotenv_key("GOOGLE_MAPS_API_KEY", APP_ENV_FILE)
GOOGLE_PLACES_API_KEY <- load_dotenv_key("GOOGLE_PLACES_API_KEY", APP_ENV_FILE)
if (!nzchar(GOOGLE_PLACES_API_KEY)) GOOGLE_PLACES_API_KEY <- GOOGLE_MAPS_API_KEY
google_places_switch <- if (nzchar(Sys.getenv("USE_GOOGLE_PLACES", unset = ""))) {
  Sys.getenv("USE_GOOGLE_PLACES")
} else {
  load_dotenv_key("USE_GOOGLE_PLACES", APP_ENV_FILE)
}
USE_GOOGLE_PLACES <- nzchar(GOOGLE_PLACES_API_KEY) &&
  !identical(google_places_switch, "0")

# Fill this block in before deployment. The bounding box is:
# west, south, east, north.
STUDY_AREA <- list(
  site_name = "Coatepeque, Guatemala",
  west = NA_real_,
  south = NA_real_,
  east = NA_real_,
  north = NA_real_,
  center_lng = DEFAULT_CENTER[["lng"]],
  center_lat = DEFAULT_CENTER[["lat"]],
  zoom = 14
)

# Hardcoded, non-patient geographic aliases for confirmed study-area landmarks.
# Add site-specific entries here as they are verified with local staff or a
# trusted map source. This is deliberately not a database or a patient-data
# store.
LOCAL_LANDMARKS <- list(
  list(
    aliases = c(
      "iglesia principe de paz galves flores costa cuca",
      "iglesia principe galves flores costa cuca"
    ),
    locality_aliases = c("galves", "flores costa cuca", "coatepeque"),
    display_name = "Iglesia Principe de Paz, Galves, Flores Costa Cuca, Guatemala",
    lat = 14.69959,
    lon = -91.8642,
    type = "church",
    class = "amenity",
    importance = 1.5
  )
)

google_maps_config <- jsonlite::toJSON(
  list(
    lat = STUDY_AREA$center_lat,
    lng = STUDY_AREA$center_lng,
    zoom = STUDY_AREA$zoom
  ),
  auto_unbox = TRUE
)

config_viewbox <- function(config) {
  values <- c(config$west, config$south, config$east, config$north)
  if (length(values) != 4 || any(!is.finite(as.numeric(values)))) return(NULL)
  as.numeric(values)
}

point_in_viewbox <- function(lat, lon, viewbox) {
  is.null(viewbox) ||
    (lon >= viewbox[[1]] && lon <= viewbox[[3]] &&
       lat >= viewbox[[2]] && lat <= viewbox[[4]])
}

safe_text <- function(x, fallback = "") {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) fallback else as.character(x[1])
}

valid_average_hours <- function(value) {
  hours <- suppressWarnings(as.numeric(safe_text(value)))
  is.finite(hours) && hours >= 1 && hours <= 24
}

resolve_municipality_name <- function(municipality) {
  key <- trimws(safe_text(municipality))
  if (!nzchar(key) || key %in% c("Municipality not specified", "Municipio no especificado")) {
    return("")
  }
  mapped <- unname(MUNICIPALITY_SEARCH_NAMES[key])
  if (!is.na(mapped) && nzchar(mapped)) mapped else paste(key, "Guatemala", sep = ", ")
}

municipality_fallback_result <- function(municipality) {
  key <- trimws(safe_text(municipality))
  point <- MUNICIPALITY_FALLBACK_POINTS[MUNICIPALITY_FALLBACK_POINTS$municipality == key, , drop = FALSE]
  if (nrow(point) == 0) return(data.frame())
  data.frame(
    display_name = paste0(
      "Approximate search area: ", resolve_municipality_name(key),
      " (municipality reference point; refine manually)"
    ),
    lat = point$lat[[1]],
    lon = point$lon[[1]],
    stringsAsFactors = FALSE
  )
}

normalize_address_query <- function(query) {
  query <- trimws(query)
  query_lines <- unlist(strsplit(query, "\\r?\\n"))
  if (length(query_lines) > 0 && grepl("^standardized address:", query_lines[[1]], ignore.case = TRUE)) {
    query <- sub("^standardized address:[ ]*", "", query_lines[[1]], ignore.case = TRUE)
  }
  query <- sub("[, ]+Guatemala[ ]*$", "", query, ignore.case = TRUE)
  query <- gsub("[[:space:]]+", " ", query)
  query <- gsub("[[:space:]]*,[[:space:]]*", ", ", query)
  query <- gsub(",[ ]*,", ",", query)
  query <- sub("[, ]+$", "", query)
  trimws(query)
}

has_locality_hint <- function(query) {
  normalized <- iconv(tolower(normalize_address_query(query)), to = "ASCII//TRANSLIT")
  any(vapply(
    iconv(tolower(KNOWN_LOCALITY_HINTS), to = "ASCII//TRANSLIT"),
    function(hint) grepl(hint, normalized, fixed = TRUE),
    logical(1)
  ))
}

qualify_search_query <- function(query, municipality = "") {
  normalized <- normalize_address_query(query)
  municipality_name <- resolve_municipality_name(municipality)
  municipality_is_present <- nzchar(municipality_name) &&
    grepl(
      iconv(tolower(sub(", Guatemala$", "", municipality_name)), to = "ASCII//TRANSLIT"),
      iconv(tolower(normalized), to = "ASCII//TRANSLIT"),
      fixed = TRUE
    )
  if (!nzchar(normalized)) {
    return(municipality_name)
  }
  if (municipality_is_present || has_locality_hint(normalized)) {
    return(normalized)
  }
  if (nzchar(municipality_name)) {
    return(paste(normalized, municipality_name, sep = ", "))
  }
  paste(normalized, DEFAULT_LOCALITY, sep = ", ")
}

fallback_search_viewbox <- function() {
  lat_delta <- SEARCH_ENVELOPE_KM / 111.32
  lon_delta <- SEARCH_ENVELOPE_KM / (111.32 * cos(STUDY_AREA$center_lat * pi / 180))
  c(
    STUDY_AREA$center_lng - lon_delta,
    STUDY_AREA$center_lat - lat_delta,
    STUDY_AREA$center_lng + lon_delta,
    STUDY_AREA$center_lat + lat_delta
  )
}

nominatim_viewbox <- function(viewbox) {
  if (is.null(viewbox)) return(NULL)
  c(viewbox[[1]], viewbox[[4]], viewbox[[3]], viewbox[[2]])
}

distance_km <- function(lat, lon, center_lat, center_lon) {
  earth_radius_km <- 6371
  lat1 <- lat * pi / 180
  lat2 <- center_lat * pi / 180
  dlat <- (lat - center_lat) * pi / 180
  dlon <- (lon - center_lon) * pi / 180
  2 * earth_radius_km * asin(sqrt(
    sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  ))
}

address_tokens <- function(x) {
  x <- iconv(tolower(normalize_address_query(x)), to = "ASCII//TRANSLIT")
  tokens <- unlist(strsplit(gsub("[^a-z0-9]+", " ", x), " +"))
  tokens[nchar(tokens) >= 2 & !tokens %in% c(
    "de", "del", "la", "el", "los", "las", "gt", "a", "al", "y",
    "cerca", "proximo", "frente", "junto", "lado", "par"
  )]
}

landmark_query_variants <- function(query) {
  base <- normalize_address_query(query)
  ascii_base <- iconv(tolower(base), to = "ASCII//TRANSLIT")
  proximity_pattern <- paste(
    "(^|[, ]+)(proximo[ ]*(a)?|pr[oó]ximo[ ]*(a)?|cerca[ ]+de[ ]*(la|el)?|",
    "frente[ ]+a|a[ ]+la[ ]+par[ ]+de|junto[ ]+a|al[ ]+lado[ ]+de|por[ ]+(la|el)?)[, ]*",
    sep = ""
  )
  match <- regexpr(proximity_pattern, ascii_base, ignore.case = TRUE, perl = TRUE)
  if (match[[1]] < 0) return(character())
  prefix <- trimws(substr(ascii_base, 1, match[[1]] - 1))
  suffix <- trimws(substr(ascii_base, match[[1]] + attr(match, "match.length"), nchar(ascii_base)))
  suffix <- sub("^(la|el|un|una)[ ]+", "", suffix)
  if (!nzchar(suffix)) return(character())
  if (!nzchar(prefix)) return(suffix)
  c(paste(suffix, prefix, sep = ", "), suffix)
}

query_variants <- function(query) {
  base <- normalize_address_query(query)
  variants <- c(landmark_query_variants(base), base)
  variants <- c(
    variants,
    gsub("\\bav\\.?([, ]|$)", "avenida\\1", base, ignore.case = TRUE),
    gsub("\\bave\\.?([, ]|$)", "avenida\\1", base, ignore.case = TRUE),
    gsub("\\bcal\\.?([, ]|$)", "calle\\1", base, ignore.case = TRUE),
    gsub("\\blot\\.?([, ]|$)", "lotificacion\\1", base, ignore.case = TRUE),
    gsub("\\bcol\\.?([, ]|$)", "colonia\\1", base, ignore.case = TRUE),
    gsub("\\bres\\.?([, ]|$)", "residencial\\1", base, ignore.case = TRUE),
    gsub("\\bcas\\.?([, ]|$)", "caserio\\1", base, ignore.case = TRUE),
    gsub("\\bmz\\.?([, ]|$)", "manzana\\1", base, ignore.case = TRUE),
    gsub("\\blt\\.?([, ]|$)", "lote\\1", base, ignore.case = TRUE),
    gsub("\\bsec\\.?([, ]|$)", "sector\\1", base, ignore.case = TRUE),
    gsub("\\bblvd\\.?([, ]|$)", "bulevar\\1", base, ignore.case = TRUE),
    gsub("\\bclzd\\.?([, ]|$)", "calzada\\1", base, ignore.case = TRUE),
    gsub("\\bctra\\.?([, ]|$)", "carretera\\1", base, ignore.case = TRUE),
    gsub("[, ]*[0-9]{5}[, ]*", ", ", base),
    gsub("\\bz\\.?[ ]*([0-9]+)\\b", "zona \\1", base, ignore.case = TRUE),
    gsub("\\b([0-9]+)(ra|da|ta|ma|va|a)\\.?\\b", "\\1", base, ignore.case = TRUE)
  )
  expanded <- base
  expanded <- gsub("\\bav\\.?([, ]|$)", "avenida\\1", expanded, ignore.case = TRUE)
  expanded <- gsub("\\bave\\.?([, ]|$)", "avenida\\1", expanded, ignore.case = TRUE)
  expanded <- gsub("\\bcal\\.?([, ]|$)", "calle\\1", expanded, ignore.case = TRUE)
  expanded <- gsub("\\blot\\.?([, ]|$)", "lotificacion\\1", expanded, ignore.case = TRUE)
  expanded <- gsub("\\bcol\\.?([, ]|$)", "colonia\\1", expanded, ignore.case = TRUE)
  expanded <- gsub("\\bres\\.?([, ]|$)", "residencial\\1", expanded, ignore.case = TRUE)
  expanded <- gsub("\\bcas\\.?([, ]|$)", "caserio\\1", expanded, ignore.case = TRUE)
  expanded <- gsub("\\bmz\\.?([, ]|$)", "manzana\\1", expanded, ignore.case = TRUE)
  expanded <- gsub("\\blt\\.?([, ]|$)", "lote\\1", expanded, ignore.case = TRUE)
  expanded <- gsub("\\bsec\\.?([, ]|$)", "sector\\1", expanded, ignore.case = TRUE)
  expanded <- gsub("\\bblvd\\.?([, ]|$)", "bulevar\\1", expanded, ignore.case = TRUE)
  expanded <- gsub("\\bclzd\\.?([, ]|$)", "calzada\\1", expanded, ignore.case = TRUE)
  expanded <- gsub("\\bctra\\.?([, ]|$)", "carretera\\1", expanded, ignore.case = TRUE)
  expanded <- gsub("\\bz\\.?[ ]*([0-9]+)\\b", "zona \\1", expanded, ignore.case = TRUE)
  expanded <- gsub("\\b([0-9]+)(ra|da|ta|ma|va|a)\\.?\\b", "\\1", expanded, ignore.case = TRUE)
  variants <- c(variants, expanded)
  variants <- variants[nzchar(trimws(variants))]
  unique(vapply(variants, normalize_address_query, character(1)))
}

rank_geocoder_results <- function(
  results,
  query,
  viewbox = NULL,
  max_distance_km = SEARCH_ENVELOPE_KM
) {
  if (length(results) == 0) return(data.frame())
  rows <- lapply(results, function(result) {
    address <- safe_text(result$display_name)
    lat <- suppressWarnings(as.numeric(safe_text(result$lat, NA)))
    lon <- suppressWarnings(as.numeric(safe_text(result$lon, NA)))
    data.frame(
      display_name = address,
      lat = lat,
      lon = lon,
      type = safe_text(result$type),
      class = safe_text(result$class),
      importance = suppressWarnings(as.numeric(safe_text(result$importance, 0))),
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  output <- output[is.finite(output$lat) & is.finite(output$lon), , drop = FALSE]
  if (nrow(output) == 0) return(data.frame())

  output$distance_km <- distance_km(
    output$lat, output$lon,
    STUDY_AREA$center_lat, STUDY_AREA$center_lng
  )
  if (!is.null(viewbox)) {
    output <- output[
      mapply(point_in_viewbox, output$lat, output$lon, MoreArgs = list(viewbox = viewbox)),
      , drop = FALSE
    ]
  } else if (is.finite(max_distance_km)) {
    output <- output[output$distance_km <= max_distance_km, , drop = FALSE]
  }
  if (nrow(output) == 0) return(data.frame())

  query_words <- unique(address_tokens(query))
  output_words <- lapply(output$display_name, address_tokens)
  token_hits <- vapply(output_words, function(words) sum(query_words %in% words), numeric(1))
  landmark_types <- c(
    "church", "place_of_worship", "school", "hospital", "clinic",
    "marketplace", "park", "fire_station", "community_centre", "library"
  )
  type_score <- ifelse(output$type %in% c("house", "building", "residential"), 30,
                       ifelse(output$type %in% landmark_types | output$class %in% c("amenity", "leisure", "tourism"), 35,
                              ifelse(output$type %in% c("road", "street", "path"), 20,
                                     ifelse(output$type %in% c("suburb", "neighbourhood", "village", "town"), 10, 0))))
  prioritize_coatepeque <- grepl(
    "coatepeque",
    iconv(tolower(query), to = "ASCII//TRANSLIT"),
    fixed = TRUE
  )
  coatepeque_priority <- ifelse(
    prioritize_coatepeque & output$distance_km <= LOCALITY_PRIORITY_RADIUS_KM,
    35,
    0
  )
  output$match_score <- token_hits * 100 + type_score + output$importance * 10 +
    coatepeque_priority - output$distance_km * 0.1
  output <- output[order(-output$match_score, output$distance_km), , drop = FALSE]
  output <- output[!duplicated(paste(round(output$lat, 5), round(output$lon, 5))), , drop = FALSE]
  output[seq_len(min(nrow(output), SEARCH_LIMIT)), c("display_name", "lat", "lon", "distance_km", "match_score"), drop = FALSE]
}

search_local_landmarks <- function(query, viewbox = NULL) {
  if (length(LOCAL_LANDMARKS) == 0) return(list())
  query_words <- unique(address_tokens(query))
  normalized_query <- iconv(tolower(normalize_address_query(query)), to = "ASCII//TRANSLIT")
  matches <- lapply(LOCAL_LANDMARKS, function(place) {
    alias_words <- unique(unlist(lapply(place$aliases, address_tokens)))
    hits <- sum(query_words %in% alias_words)
    if (hits < min(2, length(query_words))) return(NULL)
    if (has_locality_hint(query) && !any(vapply(
      iconv(tolower(place$locality_aliases), to = "ASCII//TRANSLIT"),
      function(locality) grepl(locality, normalized_query, fixed = TRUE),
      logical(1)
    ))) return(NULL)
    if (!point_in_viewbox(place$lat, place$lon, viewbox)) return(NULL)
    list(
      display_name = place$display_name,
      lat = place$lat,
      lon = place$lon,
      type = place$type,
      class = place$class,
      importance = place$importance + hits / 100
    )
  })
  Filter(Negate(is.null), matches)
}

google_places_bias <- function(viewbox = NULL) {
  box <- if (is.null(viewbox)) fallback_search_viewbox() else viewbox
  center_lat <- mean(c(box[[2]], box[[4]]))
  center_lng <- mean(c(box[[1]], box[[3]]))
  radius_km <- max(
    distance_km(box[[2]], box[[1]], center_lat, center_lng),
    distance_km(box[[4]], box[[3]], center_lat, center_lng)
  )
  list(circle = list(
    center = list(latitude = center_lat, longitude = center_lng),
    radius = min(radius_km * 1000, 50000)
  ))
}

search_google_places <- function(query, viewbox = NULL) {
  if (!USE_GOOGLE_PLACES || !nzchar(GOOGLE_PLACES_API_KEY)) return(list())
  google_queries <- unique(c(
    normalize_address_query(query),
    landmark_query_variants(query)
  ))
  google_queries <- google_queries[nzchar(google_queries)]
  all_places <- list()
  # Keep the normal interaction to one billable, fast provider request. The
  # local alias layer handles confirmed study landmarks; Google handles the
  # general place-name case.
  for (google_query in head(google_queries, 1)) {
    request_body <- list(
      textQuery = paste(google_query, "Guatemala"),
      languageCode = "es",
      regionCode = "GT",
      pageSize = SEARCH_LIMIT
    )
    if (!is.null(viewbox)) request_body$locationBias <- google_places_bias(viewbox)
    response <- httr::POST(
      "https://places.googleapis.com/v1/places:searchText",
      httr::add_headers(
        `X-Goog-Api-Key` = GOOGLE_PLACES_API_KEY,
        `X-Goog-FieldMask` = paste(
          "places.displayName",
          "places.formattedAddress",
          "places.location",
          "places.primaryType",
          "places.types",
          sep = ","
        )
      ),
      body = request_body,
      encode = "json",
      httr::timeout(3)
    )
    if (httr::status_code(response) >= 400) next
    payload <- jsonlite::fromJSON(
      httr::content(response, "text", encoding = "UTF-8"),
      simplifyDataFrame = FALSE
    )
    places <- payload$places
    if (is.null(places) || length(places) == 0) next
    all_places <- c(all_places, lapply(places, function(place) {
      place_name <- safe_text(place$displayName$text)
      formatted_address <- safe_text(place$formattedAddress)
      list(
        display_name = if (nzchar(formatted_address)) {
          paste(place_name, formatted_address, sep = ", ")
        } else {
          place_name
        },
        lat = safe_text(place$location$latitude, NA),
        lon = safe_text(place$location$longitude, NA),
        type = safe_text(place$primaryType),
        class = "google_place",
        importance = 0.9
      )
    }))
  }
  all_places
}

fallback_point_result <- function(query, viewbox = NULL) {
  box <- if (is.null(viewbox)) fallback_search_viewbox() else viewbox
  lat <- mean(c(box[[2]], box[[4]]))
  lon <- mean(c(box[[1]], box[[3]]))
  data.frame(
    display_name = paste0(
      "Approximate search area: ", STUDY_AREA$site_name,
      " (refine manually on map)"
    ),
    lat = lat,
    lon = lon,
    distance_km = distance_km(lat, lon, STUDY_AREA$center_lat, STUDY_AREA$center_lng),
    match_score = 0,
    stringsAsFactors = FALSE
  )
}

search_addresses <- function(query, viewbox = NULL, municipality = "") {
  municipality_name <- resolve_municipality_name(municipality)
  municipality_selected <- nzchar(municipality_name)
  explicit_locality <- has_locality_hint(query) || municipality_selected
  search_viewbox <- if (!explicit_locality && is.null(viewbox)) {
    fallback_search_viewbox()
  } else {
    viewbox
  }
  ranking_viewbox <- if (explicit_locality) NULL else viewbox
  max_distance_km <- if (explicit_locality) Inf else SEARCH_ENVELOPE_KM
  qualified_query <- qualify_search_query(query, municipality = municipality)
  variants <- query_variants(qualified_query)
  if (length(variants) == 0) variants <- normalize_address_query(query)
  ranking_query <- paste(variants, collapse = " ")

  # Fast primary path: confirmed local aliases plus Google Places. A transient
  # provider failure must not prevent the fallback point from being returned.
  ranked_local <- rank_geocoder_results(
    search_local_landmarks(query, viewbox = ranking_viewbox),
    ranking_query,
    viewbox = ranking_viewbox,
    max_distance_km = max_distance_km
  )
  if (nrow(ranked_local) > 0) return(ranked_local)

  primary_results <- tryCatch(
    search_google_places(qualified_query, viewbox = search_viewbox),
    error = function(e) list()
  )
  ranked_primary <- rank_geocoder_results(
    primary_results,
    ranking_query,
    viewbox = ranking_viewbox,
    max_distance_km = max_distance_km
  )
  if (nrow(ranked_primary) > 0) return(ranked_primary)

  # One bounded Nominatim request is retained as a low-cost secondary path.
  # Do not loop through every spelling variant here: the user must get a
  # usable point promptly, even when an upstream service is slow or empty.
  variant <- variants[[1]]
  ranked_nominatim <- tryCatch({
    response <- httr::GET(
      paste0(NOMINATIM_URL, "/search"),
      query = c(
        list(
          q = variant,
          format = "jsonv2",
          "accept-language" = "es",
          addressdetails = 1,
          namedetails = 1,
          dedupe = 1,
          limit = SEARCH_LIMIT,
          countrycodes = "gt",
          viewbox = if (is.null(search_viewbox)) {
            NULL
          } else {
            paste(nominatim_viewbox(search_viewbox), collapse = ",")
          }
        ),
        if (!is.null(search_viewbox) && !explicit_locality) list(bounded = 1) else list()
      ),
      httr::user_agent(geocoder_user_agent),
      httr::timeout(3)
    )
    httr::stop_for_status(response)
    result <- jsonlite::fromJSON(
      httr::content(response, "text", encoding = "UTF-8"),
      simplifyDataFrame = FALSE
    )
    rank_geocoder_results(
      result,
      ranking_query,
      viewbox = ranking_viewbox,
      max_distance_km = max_distance_km
    )
  }, error = function(e) data.frame())
  if (nrow(ranked_nominatim) > 0) return(ranked_nominatim)

  # Contract: every submitted search returns a point. This is explicitly
  # labeled approximate so the clinician can refine it manually.
  fallback_point_result(query, viewbox = viewbox)
}

reverse_address <- function(lat, lon) {
  response <- httr::GET(
    paste0(NOMINATIM_URL, "/reverse"),
    query = list(
      format = "jsonv2",
      lat = lat,
      lon = lon,
      zoom = 18,
      addressdetails = 1
    ),
    httr::user_agent(geocoder_user_agent),
    httr::timeout(8)
  )
  httr::stop_for_status(response)
  result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  safe_text(result$display_name, "No standardized address found")
}

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      html, body { height: 100%; overflow: hidden; }
      .page { height: 100vh; max-width: 1600px; margin: 0 auto; padding: 8px 12px;
              display: flex; flex-direction: column; gap: 8px; }
      .page h2 { margin: 0; font-size: 21px; line-height: 1.1; }
      .page > p { margin: 0; font-size: 13px; color: #53616d; }
      .search-strip { background: #fff; padding: 7px 10px 3px; border-radius: 7px;
                      box-shadow: 0 1px 5px rgba(0,0,0,.18); flex: 0 0 auto; }
      .search-strip .form-group { margin-bottom: 3px; }
      .search-strip label { font-size: 12px; margin-bottom: 2px; }
      .search-strip .form-control { height: 34px; }
      .search-strip .btn { min-height: 34px; padding: 5px 9px; }
      .workspace-row { flex: 1 1 auto; min-height: 0; display: flex; gap: 8px; }
      .map-panel { flex: 1 1 68%; min-width: 0; min-height: 0; }
      .google-map { height: 100%; min-height: 0; border-radius: 8px; }
      .right-panel { flex: 0 0 27%; min-width: 330px; min-height: 0; display: flex;
                     flex-direction: column; gap: 8px; overflow-y: scroll; overflow-x: hidden;
                     padding-right: 3px; scrollbar-gutter: stable; }
      .context-panel, .candidate-panel, .result-panel, .packet-panel { background: #f7f9fb; border: 1px solid #d9e1e8;
                                        border-radius: 8px; padding: 8px 10px; }
      .candidate-panel { flex: 0 0 auto; min-height: 72px; }
      .context-panel { flex: 0 0 auto; }
      .result-panel { flex: 0 0 auto; }
      .packet-panel { flex: 0 0 auto; min-height: 0; overflow: visible; }
      .context-panel h4, .candidate-panel h4, .result-panel h4, .packet-panel h4 { margin: 0 0 5px; font-size: 14px; font-weight: 600; }
      .candidate-panel h4 { font-size: 0; }
      .candidate-panel h4::after { content: 'Possible locations'; font-size: 15px; }
      .candidate-panel { display: none; }
      .result-value { font-size: 12px; word-break: break-word; }
      .context-panel .form-group, .result-panel .form-group { margin-bottom: 5px; }
      .context-panel label, .result-panel label, .packet-panel label { font-size: 12px; margin-bottom: 2px; }
      .context-panel .form-control, .result-panel .form-control { height: 29px; padding: 4px 7px; font-size: 12px; }
      .context-panel .btn, .result-panel .btn, .packet-panel .btn { min-height: 30px; padding: 4px 8px; font-size: 12px; }
      .status-line { font-size: 11px; color: #53616d; min-height: 15px; margin-top: 4px; }
      .validation-message { margin: 3px 0 5px; padding: 5px 7px; border: 1px solid #d9534f;
                            border-left: 4px solid #c9302c; border-radius: 4px; background: #fff1f0;
                            color: #8a1f17; font-size: 12px; line-height: 1.25; }
      .validation-message i { margin-right: 4px; }
      .packet-table { overflow-y: auto; max-height: 135px; font-size: 11px; }
      .packet-table table { margin-bottom: 5px; }
      .packet-table td, .packet-table th { padding: 3px 4px !important; }
      .packet-table .row-action { border: 0; background: transparent; padding: 0 3px; font-size: 11px; }
      .packet-table .row-edit { color: #1d6fa5; }
      .packet-table .row-delete { color: #b52b27; }
      .review-map { height: 65vh; min-height: 420px; width: 100%; border-radius: 7px; }
      .result-panel .form-group { margin-bottom: 0; }
      .result-panel .row { margin-bottom: 7px; }
      .result-panel .btn { min-height: 38px; }
      .utility-bar { height: 20px; position: relative; display: flex; align-items: center;
                     justify-content: space-between; text-align: left; }
      .info-button { padding: 0 4px; color: #1d6fa5; vertical-align: middle; }
      .language-switch { width: 95px; margin-left: auto; }
      .language-switch .form-group { margin: 0; }
      .language-switch .form-control { height: 24px; padding: 2px 5px; font-size: 11px; }
      .guidance-popover { display: none; position: absolute; z-index: 1000; max-width: 330px;
                          right: 0; top: 22px;
                          background: white; border: 1px solid #b9c8d5; border-radius: 6px;
                          padding: 9px 11px; box-shadow: 0 2px 8px rgba(0,0,0,.2);
                          font-size: 12px; font-weight: normal; }
      .precision-options .radio { display: block; margin: 0 0 4px 0; }
      .precision-options label { font-size: 12px; font-weight: normal; }
      @media (max-width: 800px) {
        html, body { overflow: auto; }
        .page { height: auto; min-height: 100vh; }
        .workspace-row { display: block; }
        .map-panel { height: 58vh; }
        .right-panel { min-width: 0; margin-top: 8px; }
       }
    ")),
    tags$script(HTML(paste0("
      document.addEventListener('keydown', function(event) {
        if (event.key === 'Enter' && event.target.id === 'address_query') {
          event.preventDefault();
          document.getElementById('search').click();
        }
      });
      function toggleGuidance() {
        var box = document.getElementById('guidance_popover');
        box.style.display = box.style.display === 'block' ? 'none' : 'block';
      }
      Shiny.addCustomMessageHandler('setLanguage', function(message) {
        var english = !!message.english;
        var text = function(selector, value) {
          var node = document.querySelector(selector);
          if (node) node.textContent = value;
        };
        text('.utility-bar > strong', english ? 'Location helper' : 'Asistente de ubicación');
        text('.context-panel > h4', english ? 'Location' : 'Ubicación');
        text('.result-panel > h4', english ? 'Selected location' : 'Ubicación seleccionada');
        text('.result-panel strong', english ? 'Standardized address' : 'Dirección estandarizada');
        text('.packet-panel > h4', english ? 'Results' : 'Resultados');
        text('#guidance_popover', english
          ? 'During the interview, search each place where the patient sleeps or spends time. Select the most likely point, adjust it if needed, and add it to Results. Copy the complete packet into the corresponding RedCap fields. This app does not save data.'
          : 'Durante la entrevista, busque cada lugar donde duerme o pasa tiempo el paciente. Seleccione el punto más probable, ajústelo si es necesario y agréguelo a Resultados. Copie el paquete completo en los campos correspondientes de RedCap. Esta aplicación no guarda datos.');
        var info = document.getElementById('info_button');
        if (info) info.title = english ? 'Workflow guidance' : 'Orientación';
      });
      Shiny.addCustomMessageHandler('copyText', function(message) {
        navigator.clipboard.writeText(message.text).then(function() {
          if (typeof Shiny.setInputValue === 'function') {
            Shiny.setInputValue('copy_success', Date.now(), {priority: 'event'});
          } else if (typeof Shiny.onInputChange === 'function') {
            Shiny.onInputChange('copy_success', Date.now());
          }
        });
      });
      window.googleMapState = {map: null, candidates: [], selected: null, reviewMarkers: [], hoverInfo: null};
      window.sendGoogleInput = function(name, value) {
        if (window.Shiny && typeof Shiny.setInputValue === 'function') {
          Shiny.setInputValue(name, value, {priority: 'event'});
        } else if (window.Shiny && typeof Shiny.onInputChange === 'function') {
          Shiny.onInputChange(name, value);
        } else {
          window.setTimeout(function() { window.sendGoogleInput(name, value); }, 100);
        }
      };
      window.initGoogleMap = function() {
        var cfg = ", google_maps_config, ";
        var mapNode = document.getElementById('google_map');
        if (!mapNode) return;
        googleMapState.map = new google.maps.Map(mapNode, {
          center: {lat: cfg.lat, lng: cfg.lng},
          zoom: cfg.zoom,
          mapTypeId: 'hybrid',
          mapTypeControl: true,
            mapTypeControlOptions: {
              mapTypeIds: ['roadmap', 'hybrid', 'satellite'],
              position: google.maps.ControlPosition.TOP_RIGHT
            },
          streetViewControl: false,
          fullscreenControl: true,
          clickableIcons: false
        });
        googleMapState.map.addListener('click', function(event) {
          sendGoogleInput('google_map_click', {
            lat: event.latLng.lat(),
            lng: event.latLng.lng(),
            nonce: Date.now()
          });
        });
        sendGoogleInput('google_map_ready', Date.now());
      };
      function compactCandidateLabel(name, index) {
        var firstPart = String(name || '').split(',')[0].trim();
        if (!firstPart) firstPart = 'Possible location';
        var label = String(index) + '. ' + firstPart;
        return label.length > 30 ? label.substring(0, 27) + '...' : label;
      }
      function escapeHtml(value) {
        return String(value || '').replace(/[&<>]/g, function(character) {
          return {'&': '&amp;', '<': '&lt;', '>': '&gt;'}[character];
        }).replace(new RegExp(String.fromCharCode(34), 'g'), '&quot;');
      }
      Shiny.addCustomMessageHandler('googleMapCandidates', function(message) {
        if (!googleMapState.map) return;
        if (!googleMapState.hoverInfo) googleMapState.hoverInfo = new google.maps.InfoWindow();
        googleMapState.candidates.forEach(function(marker) { marker.setMap(null); });
        googleMapState.candidates = [];
        var bounds = new google.maps.LatLngBounds();
        message.candidates.forEach(function(candidate) {
          var shortLabel = compactCandidateLabel(candidate.display_name, candidate.index);
          var labelWidth = Math.min(250, Math.max(90, shortLabel.length * 6 + 18));
          var labelSvg = `<svg xmlns='http://www.w3.org/2000/svg' width='${labelWidth}' height='42' viewBox='0 0 ${labelWidth} 42'><rect x='1' y='1' width='${labelWidth - 2}' height='23' rx='4' fill='white' fill-opacity='0.96' stroke='#8a9aaa' stroke-width='1'/><text x='${labelWidth / 2}' y='16' text-anchor='middle' font-family='Arial, sans-serif' font-size='11' font-weight='600' fill='#000000'>${escapeHtml(shortLabel)}</text><line x1='${labelWidth / 2}' y1='24' x2='${labelWidth / 2}' y2='33' stroke='#174a8b' stroke-width='2'/><circle cx='${labelWidth / 2}' cy='36' r='5' fill='#2d8cff' stroke='#174a8b' stroke-width='2'/></svg>`;
          var marker = new google.maps.Marker({
            map: googleMapState.map,
            position: {lat: candidate.lat, lng: candidate.lon},
            title: candidate.display_name,
            icon: {
              url: 'data:image/svg+xml;charset=UTF-8,' + encodeURIComponent(labelSvg),
              scaledSize: new google.maps.Size(labelWidth, 42),
              anchor: new google.maps.Point(labelWidth / 2, 42)
            }
          });
          marker.addListener('click', function() {
            sendGoogleInput('google_candidate_click', {index: candidate.index, nonce: Date.now()});
          });
          marker.addListener('mouseover', function() {
            googleMapState.hoverInfo.setContent(
              `<div style='max-width:320px;font-size:12px;line-height:1.35;'>${escapeHtml(candidate.display_name)}</div>`
            );
            googleMapState.hoverInfo.open({map: googleMapState.map, anchor: marker});
          });
          marker.addListener('mouseout', function() {
            googleMapState.hoverInfo.close();
          });
          googleMapState.candidates.push(marker);
          bounds.extend(marker.getPosition());
        });
        if (message.candidates.length > 0) {
          googleMapState.map.fitBounds(bounds, 60);
        }
      });
      Shiny.addCustomMessageHandler('googleMapSelectPoint', function(message) {
        if (!googleMapState.map) return;
        if (googleMapState.selected) googleMapState.selected.setMap(null);
        googleMapState.selected = new google.maps.Marker({
          map: googleMapState.map,
          position: {lat: message.lat, lng: message.lon},
          draggable: true,
          title: 'Selected location',
          icon: {path: google.maps.SymbolPath.CIRCLE, scale: 10,
                 fillColor: '#e53935', fillOpacity: 1,
                 strokeColor: '#8b0000', strokeWeight: 3}
        });
        googleMapState.selected.addListener('dragend', function(event) {
          sendGoogleInput('google_marker_dragend', {
            lat: event.latLng.lat(),
            lng: event.latLng.lng(),
            nonce: Date.now()
          });
        });
        googleMapState.map.panTo({lat: message.lat, lng: message.lon});
        if (message.zoom) googleMapState.map.setZoom(message.zoom);
      });
      window.googleReviewState = {map: null, markers: [], hoverInfo: null};
      function drawReviewMap(message) {
        var mapNode = document.getElementById('review_map');
        if (!mapNode) {
          window.setTimeout(function() { drawReviewMap(message); }, 100);
          return;
        }
        googleReviewState.map = new google.maps.Map(mapNode, {
          center: {lat: message.center.lat, lng: message.center.lng},
          zoom: message.center.zoom || 14,
          mapTypeId: googleMapState.map ? googleMapState.map.getMapTypeId() : 'hybrid',
          mapTypeControl: true,
          mapTypeControlOptions: {mapTypeIds: ['roadmap', 'hybrid', 'satellite']},
          streetViewControl: false,
          fullscreenControl: true,
          clickableIcons: false
        });
        googleReviewState.markers.forEach(function(marker) { marker.setMap(null); });
        googleReviewState.markers = [];
        googleReviewState.hoverInfo = new google.maps.InfoWindow();
        var bounds = new google.maps.LatLngBounds();
        (message.locations || []).forEach(function(location) {
          var shortLabel = String(location.typeLabel || 'Lugar');
          if (shortLabel.length > 24) shortLabel = shortLabel.substring(0, 21) + '...';
          var labelWidth = Math.min(210, Math.max(80, shortLabel.length * 6 + 18));
          var labelSvg = `<svg xmlns='http://www.w3.org/2000/svg' width='${labelWidth}' height='42' viewBox='0 0 ${labelWidth} 42'><rect x='1' y='1' width='${labelWidth - 2}' height='23' rx='4' fill='#fff7cc' fill-opacity='0.98' stroke='#8d6e00' stroke-width='1'/><text x='${labelWidth / 2}' y='16' text-anchor='middle' font-family='Arial, sans-serif' font-size='11' font-weight='600' fill='#000000'>${escapeHtml(shortLabel)}</text><line x1='${labelWidth / 2}' y1='24' x2='${labelWidth / 2}' y2='33' stroke='#8d6e00' stroke-width='2'/><circle cx='${labelWidth / 2}' cy='36' r='5' fill='#f2c94c' stroke='#8d6e00' stroke-width='2'/></svg>`;
          var marker = new google.maps.Marker({
            map: googleReviewState.map,
            position: {lat: Number(location.lat), lng: Number(location.lon)},
            title: location.typeLabel || 'Lugar',
            icon: {
              url: 'data:image/svg+xml;charset=UTF-8,' + encodeURIComponent(labelSvg),
              scaledSize: new google.maps.Size(labelWidth, 42),
              anchor: new google.maps.Point(labelWidth / 2, 42)
            }
          });
          marker.addListener('mouseover', function() {
            googleReviewState.hoverInfo.setContent(
              `<div style='max-width:380px;font-size:12px;line-height:1.4;'><strong>${escapeHtml(location.typeLabel || 'Lugar')}</strong><br>${escapeHtml(location.details || '')}</div>`
            );
            googleReviewState.hoverInfo.open({map: googleReviewState.map, anchor: marker});
          });
          marker.addListener('mouseout', function() { googleReviewState.hoverInfo.close(); });
          googleReviewState.markers.push(marker);
          bounds.extend(marker.getPosition());
        });
        if ((message.locations || []).length > 0) googleReviewState.map.fitBounds(bounds, 60);
      }
      Shiny.addCustomMessageHandler('googleReviewMap', function(message) {
        drawReviewMap(message);
      });
      Shiny.addCustomMessageHandler('googleMapClear', function(message) {
        if (googleMapState.selected) googleMapState.selected.setMap(null);
        googleMapState.selected = null;
        googleMapState.candidates.forEach(function(marker) { marker.setMap(null); });
        googleMapState.candidates = [];
        googleMapState.reviewMarkers.forEach(function(marker) { marker.setMap(null); });
        googleMapState.reviewMarkers = [];
      });
      Shiny.addCustomMessageHandler('googleMapReviewPacket', function(message) {
        if (!googleMapState.map) return;
        if (!googleMapState.hoverInfo) googleMapState.hoverInfo = new google.maps.InfoWindow();
        googleMapState.reviewMarkers.forEach(function(marker) { marker.setMap(null); });
        googleMapState.reviewMarkers = [];
        var bounds = new google.maps.LatLngBounds();
        (message.locations || []).forEach(function(location, index) {
          var shortLabel = String(location.typeLabel || 'Lugar');
          if (shortLabel.length > 24) shortLabel = shortLabel.substring(0, 21) + '...';
          var labelWidth = Math.min(210, Math.max(80, shortLabel.length * 6 + 18));
          var labelSvg = `<svg xmlns='http://www.w3.org/2000/svg' width='${labelWidth}' height='42' viewBox='0 0 ${labelWidth} 42'><rect x='1' y='1' width='${labelWidth - 2}' height='23' rx='4' fill='#fff7cc' fill-opacity='0.98' stroke='#8d6e00' stroke-width='1'/><text x='${labelWidth / 2}' y='16' text-anchor='middle' font-family='Arial, sans-serif' font-size='11' font-weight='600' fill='#000000'>${escapeHtml(shortLabel)}</text><line x1='${labelWidth / 2}' y1='24' x2='${labelWidth / 2}' y2='33' stroke='#8d6e00' stroke-width='2'/><circle cx='${labelWidth / 2}' cy='36' r='5' fill='#f2c94c' stroke='#8d6e00' stroke-width='2'/></svg>`;
          var marker = new google.maps.Marker({
            map: googleMapState.map,
            position: {lat: Number(location.lat), lng: Number(location.lon)},
            title: location.typeLabel || 'Lugar',
            icon: {
              url: 'data:image/svg+xml;charset=UTF-8,' + encodeURIComponent(labelSvg),
              scaledSize: new google.maps.Size(labelWidth, 42),
              anchor: new google.maps.Point(labelWidth / 2, 42)
            }
          });
          marker.addListener('mouseover', function() {
            googleMapState.hoverInfo.setContent(
              `<div style='max-width:360px;font-size:12px;line-height:1.4;'><strong>${escapeHtml(location.typeLabel || 'Lugar')}</strong><br>${escapeHtml(location.details || '')}</div>`
            );
            googleMapState.hoverInfo.open({map: googleMapState.map, anchor: marker});
          });
          marker.addListener('mouseout', function() { googleMapState.hoverInfo.close(); });
          googleMapState.reviewMarkers.push(marker);
          bounds.extend(marker.getPosition());
        });
        if ((message.locations || []).length > 0) googleMapState.map.fitBounds(bounds, 60);
      });
      Shiny.addCustomMessageHandler('googleMapSetView', function(message) {
        if (!googleMapState.map) return;
        googleMapState.map.setCenter({lat: message.lat, lng: message.lng});
        googleMapState.map.setZoom(message.zoom);
      });
      (function waitForGoogleMap() {
        if (window.google && window.google.maps && !googleMapState.map && typeof window.initGoogleMap === 'function') {
          window.initGoogleMap();
        } else if (!googleMapState.map) {
          window.setTimeout(waitForGoogleMap, 200);
        }
      })();
    "))),
    if (nzchar(GOOGLE_MAPS_API_KEY)) {
      tags$script(
        src = paste0(
          "https://maps.googleapis.com/maps/api/js?key=",
          utils::URLencode(GOOGLE_MAPS_API_KEY, reserved = TRUE),
          "&callback=initGoogleMap&v=weekly"
        ),
        async = NA,
        defer = NA
      )
    }
  ),
  div(class = "page",
      div(class = "workspace-row",
          div(class = "map-panel", div(id = "google_map", class = "google-map")),
          div(class = "right-panel",
              div(class = "utility-bar",
                  tags$strong("Asistente de ubicación"),
                  tags$button(
                    id = "info_button",
                    type = "button",
                    class = "btn btn-link info-button",
                    onclick = "toggleGuidance()",
                    title = paste0("Orientaci", intToUtf8(243), "n"),
                    icon("info-circle")
                  ),
                  div(class = "language-switch",
                      selectInput(
                        "language", NULL,
                        choices = stats::setNames(c("es", "en"), c(paste0("Espa", intToUtf8(241), "ol"), "English")),
                        selected = "es",
                        width = "95px"
                      )
                  ),
                  div(
                    id = "guidance_popover",
                    class = "guidance-popover",
                    paste0("Durante la entrevista, busque cada lugar donde duerme o pasa tiempo el paciente. Seleccione el punto m", intToUtf8(225), "s probable, aj", intToUtf8(250), "stelo si es necesario y agr", intToUtf8(233), "guelo a Resultados. Copie el paquete completo en los campos correspondientes de RedCap. Esta aplicaci", intToUtf8(243), "n no guarda datos.")
                  )
              ),
              div(class = "context-panel",
                  h4(paste0("Ubicaci", intToUtf8(243), "n")),
                  selectInput(
                    "location_type", "Lugar",
                    choices = LOCATION_TYPE_CHOICES_ES,
                    selected = "Primary sleeping place"
                  ),
                  conditionalPanel(
                    condition = "input.location_type == 'Other'",
                    textInput("other_place", "Especifique otro lugar", value = ""),
                    uiOutput("other_place_validation")
                  ),
                  selectInput(
                      "municipality", "Municipio",
                      choices = c(
                        "Municipio no especificado" = "",
                        "Coatepeque" = "Coatepeque",
                        "La Blanca" = "La Blanca",
                        "Retalhuleu" = "Retalhuleu",
                        "Nuevo Progreso" = "Nuevo Progreso",
                        "La Reforma" = "La Reforma",
                        "El Quetzal" = "El Quetzal",
                        "Genova C.C." = "Genova C.C.",
                        "Flores C.C." = "Flores C.C.",
                        "Colomba C.C." = "Colomba C.C.",
                        "Pajapita" = "Pajapita",
                        "Ocos" = "Ocos",
                        "Tecun" = "Tecun",
                        "Otro" = "Other"
                      )
                  ),
                  conditionalPanel(
                    condition = "input.municipality == 'Other'",
                    textInput("other_municipality", "Especifique otro municipio", value = ""),
                    uiOutput("other_municipality_validation")
                  ),
                  div(class = "field-label", uiOutput("address_field_label")),
                  textInput("address_query", NULL,
                            width = "100%", placeholder = ""),
                  fluidRow(
                    column(6, actionButton("search", "Buscar", class = "btn-primary", width = "100%")),
                    column(6, actionButton("reset", "Limpiar ubicación", class = "btn-secondary", width = "100%"))
                  ),
                  div(class = "status-line", textOutput("status")),
                  uiOutput("location_validation")
              ),
              div(class = "result-panel",
                  h4("Ubicación seleccionada"),
                  strong("Dirección estandarizada"),
                  textInput("standard_address_edit", NULL, value = "", width = "100%"),
                  uiOutput("address_validation"),
                  fluidRow(
                    column(6, textInput("latitude_edit", "Latitud", value = "")),
                    column(6, textInput("longitude_edit", "Longitud", value = ""))
                  ),
                  uiOutput("coordinates_validation"),
                  fluidRow(
                    column(6, div(class = "field-label", uiOutput("days_field_label")),
                           textInput("days_at_place", NULL, value = "", placeholder = "1–7")),
                    column(6, div(class = "field-label", uiOutput("hours_field_label")),
                           numericInput("hours_at_place", NULL, value = NULL, min = 1, max = 24, step = 0.1))
                  ),
                  uiOutput("days_validation"),
                  uiOutput("hours_validation"),
                  actionButton("add_location", "Agregar al paquete", class = "btn-success", width = "100%")
              ),
              div(class = "packet-panel",
                  h4("Resultados"),
                  div(class = "packet-table", DT::DTOutput("packet_table")),
                  fluidRow(
                    column(4, actionButton("copy_packet", "Copiar para RedCap", class = "btn-primary", width = "100%")),
                    column(3, actionButton("review_packet", "Revisar", class = "btn-info", width = "100%")),
                    column(5, actionButton("clear_packet", "Limpiar resultados", class = "btn-secondary", width = "100%"))
                  )
              )
          )
  )
)
)

server <- function(input, output, session) {
  current_point <- reactiveVal(NULL)
  current_address <- reactiveVal("")
  candidates <- reactiveVal(data.frame())
  status_message <- reactiveVal("Ready")
  pending_location <- reactiveVal(NULL)
  municipality_reference_cache <- reactiveVal(list())
  editing_packet_index <- reactiveVal(NULL)
  suppress_type_clear <- reactiveVal(FALSE)
  location_packet <- reactiveVal(data.frame(
    Type = character(), OtherPlace = character(), Municipality = character(), OtherMunicipality = character(),
    Address = character(), Latitude = character(), Longitude = character(),
    Days = character(), Hours = character(),
    stringsAsFactors = FALSE
  ))
  validation_state <- reactiveVal(list())
  validation_texts <- list(
    location = list(
      missing = c(es = "Seleccione o coloque una ubicación en el mapa.",
                  en = "Select or place a location on the map.")
    ),
    address = list(
      missing = c(es = "Ingrese o confirme una dirección estandarizada.",
                  en = "Enter or confirm a standardized address.")
    ),
    coordinates = list(
      invalid = c(es = "Ingrese una latitud y longitud válidas.",
                  en = "Enter valid latitude and longitude coordinates.")
    ),
    other_place = list(
      missing = c(es = "Especifique el otro lugar.",
                  en = "Specify the other place.")
    ),
    other_municipality = list(
      missing = c(es = "Especifique el otro municipio.",
                  en = "Specify the other municipality.")
    ),
    days = list(
      invalid = c(es = "Ingrese un número de días entre 1 y 7.",
                  en = "Enter a number of days from 1 to 7.")
    ),
    hours = list(
      missing = c(es = "Ingrese el promedio de horas por día. Este campo es obligatorio.",
                  en = "Enter the average hours per day. This field is required."),
      invalid = c(es = "Ingrese un número válido entre 1 y 24.",
                  en = "Enter a valid number from 1 to 24.")
    )
  )

  validation_message <- function(field) {
    messages <- validation_state()
    code <- messages[[field]]
    if (is.null(code) || !nzchar(code) || is.null(validation_texts[[field]][[code]])) return(NULL)
    language <- if (identical(safe_text(input$language, "es"), "en")) "en" else "es"
    message <- unname(validation_texts[[field]][[code]][[language]])
    div(
      class = "validation-message",
      role = "alert",
      icon("exclamation-triangle"),
      message
    )
  }

  output$location_validation <- renderUI(validation_message("location"))
  output$address_validation <- renderUI(validation_message("address"))
  output$coordinates_validation <- renderUI(validation_message("coordinates"))
  output$other_place_validation <- renderUI(validation_message("other_place"))
  output$other_municipality_validation <- renderUI(validation_message("other_municipality"))
  output$days_validation <- renderUI(validation_message("days"))
  output$hours_validation <- renderUI(validation_message("hours"))

  observeEvent(
    list(
      input$address_query, input$standard_address_edit, input$latitude_edit,
      input$longitude_edit, input$days_at_place, input$hours_at_place,
      input$other_place, input$other_municipality, input$location_type,
      input$municipality
    ),
    validation_state(list()),
    ignoreInit = TRUE
  )

  output$status <- renderText(status_message())

  output$address_field_label <- renderUI({
    labels_es <- c(
      "Primary sleeping place" = "&#191;Cu&#225;l es la direcci&#243;n del lugar donde durmi&#243; principalmente? Incluya el barrio, colonia, aldea o comunidad, si est&#225; disponible.",
      "Work" = "Ubicaci&#243;n del trabajo",
      "School" = "Ubicaci&#243;n de la escuela",
      "Market" = "Ubicaci&#243;n del mercado",
      "Family/friend home" = "Ubicaci&#243;n de la casa de familiar/amigo",
      "Church" = "Ubicaci&#243;n de la iglesia",
      "Recreation/play" = "Ubicaci&#243;n de la recreaci&#243;n",
      "Other" = "Ubicaci&#243;n de otro lugar"
    )
    labels_en <- c(
      "Primary sleeping place" = "What is the address where the patient mainly slept? Include the barrio, colonia, aldea, or community if available.",
      "Work" = "Work location",
      "School" = "School location",
      "Market" = "Market location",
      "Family/friend home" = "Family/friend home location",
      "Church" = "Church location",
      "Recreation/play" = "Recreation location",
      "Other" = "Other place location"
    )
    labels <- if (identical(safe_text(input$language, "es"), "en")) labels_en else labels_es
    key <- safe_text(input$location_type, "Primary sleeping place")
    if (!key %in% names(labels)) {
      return(HTML(unname(labels[["Primary sleeping place"]])))
    }
    HTML(unname(labels[[key]]))
  })

  output$days_field_label <- renderUI({
    labels_es <- c(
      "Work" = "D&#237;as que pas&#243; en el trabajo",
      "School" = "D&#237;as que pas&#243; en la escuela",
      "Market" = "D&#237;as que pas&#243; en el mercado",
      "Family/friend home" = "D&#237;as que pas&#243; en la casa",
      "Church" = "D&#237;as que pas&#243; en la iglesia",
      "Recreation/play" = "D&#237;as que pas&#243; en la recreaci&#243;n",
      "Other" = "D&#237;as que pas&#243; en otro lugar",
      "Primary sleeping place" = "D&#237;as durante el per&#237;odo de 7 d&#237;as"
    )
    labels_en <- c(
      "Work" = "Days at work", "School" = "Days at school", "Market" = "Days at the market",
      "Family/friend home" = "Days at the family/friend home", "Church" = "Days at church",
      "Recreation/play" = "Days at recreation", "Other" = "Days at other place",
      "Primary sleeping place" = "Days during the 7-day period"
    )
    labels <- if (identical(safe_text(input$language, "es"), "en")) labels_en else labels_es
    key <- safe_text(input$location_type)
    if (!key %in% names(labels)) return(NULL)
    HTML(unname(labels[[key]]))
  })

  output$hours_field_label <- renderUI({
    labels_es <- c(
      "Work" = "Promedio de horas por d&#237;a",
      "School" = "Promedio de horas por d&#237;a",
      "Market" = "Promedio de horas por d&#237;a",
      "Family/friend home" = "Promedio de horas por d&#237;a",
      "Church" = "Promedio de horas por d&#237;a",
      "Recreation/play" = "Promedio de horas por d&#237;a",
      "Other" = "Promedio de horas por d&#237;a",
      "Primary sleeping place" = "Promedio de horas por d&#237;a"
    )
    labels_en <- c(
      "Work" = "Average hours per day", "School" = "Average hours per day",
      "Market" = "Average hours per day", "Family/friend home" = "Average hours per day",
      "Church" = "Average hours per day", "Recreation/play" = "Average hours per day",
      "Other" = "Average hours per day", "Primary sleeping place" = "Average hours per day"
    )
    labels <- if (identical(safe_text(input$language, "es"), "en")) labels_en else labels_es
    key <- safe_text(input$location_type)
    if (!key %in% names(labels)) return(NULL)
    HTML(unname(labels[[key]]))
  })

  observeEvent(input$language, {
    english <- identical(safe_text(input$language, "es"), "en")
    choices <- if (english) LOCATION_TYPE_CHOICES_EN else LOCATION_TYPE_CHOICES_ES
    municipality_choices <- if (english) {
      stats::setNames(
        c("Municipality not specified", "Coatepeque", "La Blanca", "Retalhuleu", "Nuevo Progreso",
          "La Reforma", "El Quetzal", "Genova C.C.", "Flores C.C.", "Colomba C.C.",
          "Pajapita", "Ocos", "Tecun", "Other"),
        c("", "Coatepeque", "La Blanca", "Retalhuleu", "Nuevo Progreso", "La Reforma",
          "El Quetzal", "Genova C.C.", "Flores C.C.", "Colomba C.C.", "Pajapita", "Ocos", "Tecun", "Other")
      )
    } else {
      stats::setNames(
        c("Municipio no especificado", "Coatepeque", "La Blanca", "Retalhuleu", "Nuevo Progreso",
          "La Reforma", "El Quetzal", "Genova C.C.", "Flores C.C.", "Colomba C.C.",
          "Pajapita", "Ocos", "Tecun", "Otro"),
        c("", "Coatepeque", "La Blanca", "Retalhuleu", "Nuevo Progreso", "La Reforma",
          "El Quetzal", "Genova C.C.", "Flores C.C.", "Colomba C.C.", "Pajapita", "Ocos", "Tecun", "Other")
      )
    }
    updateSelectInput(session, "location_type", choices = choices,
                      selected = safe_text(input$location_type, "Primary sleeping place"),
                      label = if (english) "Place type" else "Lugar")
    updateSelectInput(session, "municipality", choices = municipality_choices,
                      selected = safe_text(input$municipality),
                      label = if (english) "Municipality" else "Municipio")
    updateTextInput(session, "other_place", label = if (english) "Specify another place" else "Especifique otro lugar")
    updateTextInput(session, "other_municipality", label = if (english) "Specify another municipality" else "Especifique otro municipio")
    updateTextInput(session, "latitude_edit", label = if (english) "Latitude" else "Latitud")
    updateTextInput(session, "longitude_edit", label = if (english) "Longitude" else "Longitud")
    updateActionButton(session, "search", label = if (english) "Search" else "Buscar")
    updateActionButton(session, "reset", label = if (english) "Clear location" else "Limpiar ubicación")
    updateActionButton(session, "add_location", label = if (english) "Add to results" else "Agregar al paquete")
    updateActionButton(session, "copy_packet", label = if (english) "Copy for RedCap" else "Copiar para RedCap")
    updateActionButton(session, "review_packet", label = if (english) "Review" else "Revisar")
    updateActionButton(session, "clear_packet", label = if (english) "Clear all results" else "Limpiar resultados")
    session$sendCustomMessage("setLanguage", list(english = english))
  }, ignoreInit = FALSE)

  observeEvent(input$location_type, {
    if (isTRUE(suppress_type_clear())) {
      suppress_type_clear(FALSE)
      return()
    }
    req(input$location_type)
    current_point(NULL)
    current_address("")
    candidates(data.frame())
    validation_state(list())
    updateTextInput(session, "address_query", value = "")
    updateTextInput(session, "standard_address_edit", value = "")
    updateTextInput(session, "latitude_edit", value = "")
    updateTextInput(session, "longitude_edit", value = "")
    updateTextInput(session, "days_at_place", value = "")
    updateNumericInput(session, "hours_at_place", value = NULL)
    session$sendCustomMessage("googleMapClear", list())
    status_message("Listo para buscar una nueva ubicación.")
  }, ignoreInit = TRUE)

  send_candidate_markers <- function() {
    results <- candidates()
    payload <- if (nrow(results) == 0) {
      list()
    } else {
      lapply(seq_len(nrow(results)), function(i) {
        list(
          index = i,
          lat = results$lat[[i]],
          lon = results$lon[[i]],
          display_name = results$display_name[[i]]
        )
      })
    }
    session$sendCustomMessage("googleMapCandidates", list(candidates = payload))
  }

  set_current_location <- function(point, address = safe_text(point$address)) {
    current_point(point)
    current_address(address)
    updateTextInput(session, "standard_address_edit", value = safe_text(address))
    updateTextInput(session, "latitude_edit", value = format(point$lat, digits = 8))
    updateTextInput(session, "longitude_edit", value = format(point$lon, digits = 8))
  }

  clear_selected_location_fields <- function() {
    current_point(NULL)
    current_address("")
    validation_state(list())
    updateTextInput(session, "standard_address_edit", value = "")
    updateTextInput(session, "latitude_edit", value = "")
    updateTextInput(session, "longitude_edit", value = "")
    updateTextInput(session, "days_at_place", value = "")
    updateNumericInput(session, "hours_at_place", value = NULL)
  }

  observeEvent(input$google_map_ready, {
    send_candidate_markers()
  }, ignoreInit = TRUE)

  output$candidate_table <- DT::renderDT({
    results <- candidates()
    if (nrow(results) == 0) {
      return(DT::datatable(
        data.frame(Message = "Search results will appear here"),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    DT::datatable(
      data.frame(
        Number = seq_len(nrow(results)),
        Address = results$display_name,
        stringsAsFactors = FALSE
      ),
      rownames = FALSE,
      selection = "single",
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        autoWidth = TRUE,
        columnDefs = list(list(className = "dt-left", targets = "_all"))
      ),
      escape = TRUE
    )
  })

  observeEvent(input$search, {
    query <- trimws(safe_text(input$address_query))
    blank_search <- !nzchar(query)
    municipality <- safe_text(input$municipality)
    if (identical(municipality, "Other")) {
      municipality <- trimws(safe_text(input$other_municipality))
    }
    if (blank_search) {
      query <- if (nzchar(municipality)) {
        resolve_municipality_name(municipality)
      } else {
        DEFAULT_LOCALITY
      }
    }
    status_message("Searching...")
    session$sendCustomMessage("googleMapClear", list())
    candidates(data.frame())
    existing_point <- current_point()
    existing_address <- current_address()
    clear_selected_location_fields()
    same_session_match <- !is.null(existing_point) &&
      nzchar(existing_address) &&
      identical(tolower(normalize_address_query(query)),
                tolower(normalize_address_query(existing_address)))

  tryCatch({
    results <- if (same_session_match) {
        data.frame(
          display_name = existing_address,
          lat = existing_point$lat,
          lon = existing_point$lon,
          stringsAsFactors = FALSE
        )
    } else {
      search_addresses(
        query,
        viewbox = config_viewbox(STUDY_AREA),
        municipality = municipality
      )
    }
    if (nrow(results) == 0) {
      results <- fallback_point_result(query, viewbox = config_viewbox(STUDY_AREA))
    }
    if (blank_search && any(grepl("Approximate search area", results$display_name, fixed = TRUE))) {
      municipality_fallback <- municipality_fallback_result(
        if (nzchar(municipality)) municipality else "Coatepeque"
      )
      if (nrow(municipality_fallback) > 0) results <- municipality_fallback
    }
    if (nrow(results) > 0) results$candidate_id <- paste0("candidate_", seq_len(nrow(results)))
    candidates(results)
    status_message(
      if (any(grepl("Approximate search area", results$display_name, fixed = TRUE))) {
        "Approximate point returned. Refine it on the map."
      } else if (nrow(results) > 0) {
        "Choose a result, or place the marker manually."
      } else {
        "No result found. Click the map to place the marker manually."
      }
    )
  }, error = function(e) {
    results <- fallback_point_result(query, viewbox = config_viewbox(STUDY_AREA))
    results$candidate_id <- "candidate_1"
    candidates(results)
    status_message("Search service unavailable. Approximate point returned; refine it on the map.")
  })

  })

  output$packet_table <- DT::renderDT({
    packet <- location_packet()
    english <- identical(safe_text(input$language, "es"), "en")
    if (nrow(packet) == 0) {
      return(DT::datatable(
        data.frame(Results = if (english) "No locations added yet" else "Aún no hay ubicaciones"),
        rownames = FALSE, options = list(dom = "t"),
        class = "compact stripe"
      ))
    }
    display_packet <- data.frame(
      Lugar = unname(LOCATION_TYPE_LABELS[packet$Type]),
      Ubicacion = packet$Address,
      Dias = packet$Days,
      Horas = packet$Hours,
      stringsAsFactors = FALSE
    )
    names(display_packet) <- if (english) {
      c("Place", "Location", "Days", "Hours")
    } else {
      c("Lugar", paste0("Ubicaci", intToUtf8(243), "n"), paste0("D", intToUtf8(237), "as"), "Horas")
    }
    display_packet[[1]][packet$Type == "Other" & nzchar(packet$OtherPlace)] <- paste0(
      display_packet[[1]][packet$Type == "Other" & nzchar(packet$OtherPlace)],
      ": ", packet$OtherPlace[packet$Type == "Other" & nzchar(packet$OtherPlace)]
    )
    edit_label <- if (english) "Edit" else "Editar"
    delete_label <- if (english) "Delete" else "Eliminar"
    display_packet$Actions <- vapply(seq_len(nrow(packet)), function(i) {
      paste0(
        "<button type='button' class='row-action row-edit' title='", edit_label, "' ",
        "onclick=\"Shiny.setInputValue('packet_edit_row', {index:", i,
        ", nonce:Date.now()}, {priority:'event'})\">", edit_label, "</button>",
        "<button type='button' class='row-action row-delete' title='", delete_label, "' ",
        "onclick=\"Shiny.setInputValue('packet_delete_row', {index:", i,
        ", nonce:Date.now()}, {priority:'event'})\">", delete_label, "</button>"
      )
    }, character(1))
    names(display_packet) <- if (english) {
      c("Place", "Location", "Days", "Hours", "Actions")
    } else {
      c("Lugar", paste0("Ubicaci", intToUtf8(243), "n"), paste0("D", intToUtf8(237), "as"), "Horas", "Acciones")
    }
    display_packet[1:4] <- lapply(display_packet[1:4], htmltools::htmlEscape)
    DT::datatable(
      display_packet,
      rownames = FALSE,
      selection = "none",
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        autoWidth = FALSE,
        columnDefs = list(
          list(width = "28%", targets = 0),
          list(width = "39%", targets = 1),
          list(width = "8%", targets = 2),
          list(width = "8%", targets = 3),
          list(width = "17%", targets = 4)
        )
      ),
      class = "compact stripe",
      escape = FALSE
    )
  })

  observeEvent(list(input$packet_table_rows_selected, input$packet_edit_row), {
    edit_event <- input$packet_edit_row
    idx <- if (!is.null(edit_event) && length(edit_event$index)) {
      as.integer(edit_event$index)
    } else {
      input$packet_table_rows_selected
    }
    packet <- location_packet()
    req(length(idx) == 1, nrow(packet) >= idx)
    row <- packet[idx, , drop = FALSE]
    editing_packet_index(as.integer(idx))
    updateActionButton(session, "add_location", label = "Actualizar ubicación")
    suppress_type_clear(TRUE)
    updateSelectInput(session, "location_type", selected = row$Type)
    updateTextInput(session, "other_place", value = row$OtherPlace)
    updateSelectInput(
      session,
      "municipality",
      selected = if (nzchar(row$OtherMunicipality)) "Other" else row$Municipality
    )
    updateTextInput(session, "other_municipality", value = row$OtherMunicipality)
    updateTextInput(session, "days_at_place", value = row$Days)
    updateNumericInput(session, "hours_at_place", value = as.numeric(row$Hours))
    set_current_location(
      list(lat = as.numeric(row$Latitude), lon = as.numeric(row$Longitude), address = row$Address),
      row$Address
    )
    session$sendCustomMessage(
      "googleMapSelectPoint",
      list(lat = as.numeric(row$Latitude), lon = as.numeric(row$Longitude), zoom = 18)
    )
    status_message("Ubicación cargada para editar.")
  })

  observeEvent(input$packet_delete_row, {
    idx <- as.integer(input$packet_delete_row$index)
    packet <- location_packet()
    req(length(idx) == 1, idx >= 1, idx <= nrow(packet))
    location_packet(packet[-idx, , drop = FALSE])
    editing_packet_index(NULL)
    updateActionButton(session, "add_location", label = "Agregar al paquete")
    status_message("Ubicación eliminada de los resultados.")
  })

  municipality_reference_point <- function(municipality) {
    municipality_name <- resolve_municipality_name(municipality)
    if (!nzchar(municipality_name)) return(NULL)
    cache <- municipality_reference_cache()
    key <- tolower(municipality_name)
    if (!is.null(cache[[key]])) return(cache[[key]])
    result <- tryCatch(
      search_addresses(municipality_name, viewbox = NULL),
      error = function(e) data.frame()
    )
    reference <- NULL
    if (nrow(result) > 0 &&
        !any(grepl("Approximate search area", result$display_name, fixed = TRUE))) {
      reference <- list(lat = result$lat[[1]], lon = result$lon[[1]])
    }
    cache[[key]] <- reference
    municipality_reference_cache(cache)
    reference
  }

  commit_location <- function(row) {
    packet <- location_packet()
    edit_index <- editing_packet_index()
    if (!is.null(edit_index) && length(edit_index) == 1 &&
        edit_index >= 1 && edit_index <= nrow(packet)) {
      packet[edit_index, ] <- row
      editing_packet_index(NULL)
      updateActionButton(session, "add_location", label = "Agregar al paquete")
    } else if (identical(row$Type, "Primary sleeping place")) {
      packet <- packet[packet$Type != row$Type, , drop = FALSE]
      packet <- rbind(packet, row)
    } else if (nrow(packet) >= 4) {
      status_message("El paquete ya tiene cuatro ubicaciones. Copie o limpie el paquete antes de agregar otra.")
      return(FALSE)
    } else {
      packet <- rbind(packet, row)
    }
    location_packet(packet)
    status_message(paste0(unname(LOCATION_TYPE_LABELS[row$Type]), " agregado al paquete."))
    TRUE
  }

  observeEvent(input$review_packet, {
    packet <- location_packet()
    req(nrow(packet) > 0)
    locations <- lapply(seq_len(nrow(packet)), function(i) {
      row <- packet[i, , drop = FALSE]
      type_label <- unname(LOCATION_TYPE_LABELS[row$Type])
      if (is.na(type_label) || !nzchar(type_label)) type_label <- row$Type
      details <- c(
        if (nzchar(row$OtherPlace)) paste0("Especifique otro lugar: ", row$OtherPlace),
        if (nzchar(row$Municipality) && !identical(row$Municipality, "Other")) paste0("Municipio: ", row$Municipality),
        if (nzchar(row$OtherMunicipality)) paste0("Especifique otro municipio: ", row$OtherMunicipality),
        paste0("Ubicación: ", row$Address),
        paste0("Latitud: ", row$Latitude),
        paste0("Longitud: ", row$Longitude),
        if (nzchar(row$Days)) paste0("Días: ", row$Days),
        if (nzchar(row$Hours)) paste0("Horas: ", row$Hours)
      )
      list(
        typeLabel = type_label,
        details = paste(details, collapse = " | "),
        lat = as.numeric(row$Latitude),
        lon = as.numeric(row$Longitude)
      )
    })
    showModal(modalDialog(
      title = "Revisar ubicaciones",
      div(id = "review_map", class = "review-map"),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Cerrar")
    ))
    session$sendCustomMessage("googleReviewMap", list(
      center = list(lat = STUDY_AREA$center_lat, lng = STUDY_AREA$center_lng, zoom = STUDY_AREA$zoom),
      locations = locations
    ))
  })

  observeEvent(input$candidate_table_rows_selected, {
    results <- candidates()
    idx <- input$candidate_table_rows_selected
    req(nrow(results) > 0, length(idx) == 1)
    result <- results[idx, , drop = FALSE]
    clear_selected_location_fields()
    set_current_location(list(lat = result$lat, lon = result$lon, address = result$display_name), result$display_name)
    session$sendCustomMessage(
      "googleMapSelectPoint",
      list(lat = result$lat, lon = result$lon, zoom = 18)
    )
    status_message("Possibility selected. Refine the location on the map.")
  })

  observeEvent(candidates(), {
    send_candidate_markers()
  }, ignoreInit = TRUE)

  observeEvent(input$google_candidate_click, {
    click <- input$google_candidate_click
    req(click, click$index)
    results <- candidates()
    idx <- as.integer(click$index)
    req(idx >= 1, idx <= nrow(results))
    DT::dataTableProxy("candidate_table") %>% DT::selectRows(idx)
    result <- results[idx, , drop = FALSE]
    clear_selected_location_fields()
    set_current_location(list(lat = result$lat, lon = result$lon, address = result$display_name), result$display_name)
    session$sendCustomMessage(
      "googleMapSelectPoint",
      list(lat = result$lat, lon = result$lon, zoom = 18)
    )
    status_message("Possibility selected. Click the map again to refine the location.")
  })

  observeEvent(input$google_map_click, {
    click <- input$google_map_click
    req(click)
    if (!point_in_viewbox(click$lat, click$lng, config_viewbox(STUDY_AREA))) {
      status_message("That point is outside the configured study area.")
      return()
    }
    status_message("Confirming the address for this point...")
    clear_selected_location_fields()
    point <- list(lat = click$lat, lon = click$lng, address = "")
    set_current_location(point, "")
    session$sendCustomMessage(
      "googleMapSelectPoint",
      list(lat = click$lat, lon = click$lng, zoom = 18)
    )

    tryCatch({
      address <- reverse_address(click$lat, click$lng)
      point$address <- address
      set_current_location(point, address)
      status_message("Location ready to copy.")
    }, error = function(e) {
      updateTextInput(session, "standard_address_edit", value = "No standardized address found")
      current_address("No standardized address found")
      status_message("The point was saved, but reverse geocoding was unavailable.")
    })
  })

  observeEvent(input$google_marker_dragend, {
    event <- input$google_marker_dragend
    req(event)
    if (!point_in_viewbox(event$lat, event$lng, config_viewbox(STUDY_AREA))) {
      status_message("That point is outside the configured study area.")
      return()
    }
    status_message("Confirming the address for this point...")
    clear_selected_location_fields()
    point <- list(lat = event$lat, lon = event$lng, address = "")
    set_current_location(point, "")
    tryCatch({
      address <- reverse_address(event$lat, event$lng)
      point$address <- address
      set_current_location(point, address)
      status_message("Location ready to copy.")
    }, error = function(e) {
      updateTextInput(session, "standard_address_edit", value = "No standardized address found")
      current_address("No standardized address found")
      status_message("The point was saved, but reverse geocoding was unavailable.")
    })
  })

  observeEvent(input$add_location, {
    validation_state(list())
    english <- identical(safe_text(input$language, "es"), "en")
    point <- current_point()
    if (is.null(point)) {
      validation_state(list(location = "missing"))
      status_message(if (english) "Select or place a location on the map before adding it to results." else "Seleccione o coloque una ubicación en el mapa antes de agregarla al paquete.")
      return()
    }
    address <- trimws(safe_text(input$standard_address_edit))
    if (!nzchar(address)) address <- trimws(current_address())
    if (!nzchar(address) || identical(address, "No standardized address found")) {
      validation_state(list(address = "missing"))
      status_message(if (english) "Enter or confirm a standardized address before adding it to results." else "Ingrese o confirme una dirección estandarizada antes de agregarla al paquete.")
      return()
    }
    latitude <- suppressWarnings(as.numeric(safe_text(input$latitude_edit)))
    longitude <- suppressWarnings(as.numeric(safe_text(input$longitude_edit)))
    if (!is.finite(latitude) || !is.finite(longitude)) {
      validation_state(list(coordinates = "invalid"))
      status_message(if (english) "Enter valid latitude and longitude coordinates." else "Ingrese coordenadas válidas de latitud y longitud.")
      return()
    }
    location_type <- safe_text(input$location_type, "Other")
    if (identical(location_type, "Other") &&
        !nzchar(trimws(safe_text(input$other_place)))) {
      validation_state(list(other_place = "missing"))
      status_message(if (english) "Specify another place before adding it to results." else "Especifique otro lugar antes de agregarlo al paquete.")
      return()
    }
    if (identical(safe_text(input$municipality), "Other") &&
        !nzchar(trimws(safe_text(input$other_municipality)))) {
      validation_state(list(other_municipality = "missing"))
      status_message(if (english) "Specify another municipality before adding it to results." else "Especifique otro municipio antes de agregarlo al paquete.")
      return()
    }
    days <- trimws(safe_text(input$days_at_place))
    hours_input <- trimws(safe_text(input$hours_at_place))
    hours <- suppressWarnings(as.numeric(hours_input))
    if (!identical(location_type, "Primary sleeping place") &&
        !grepl("^[1-7]$", days)) {
      validation_state(list(days = "invalid"))
      status_message(if (english) "Enter the number of days spent at the place (1–7)." else "Ingrese los días que pasó en el lugar (un número de 1 a 7).")
      return()
    }
    if (!valid_average_hours(hours_input)) {
      validation_state(list(hours = if (nzchar(hours_input)) "invalid" else "missing"))
      status_message(if (english) "Enter a valid number from 1 to 24." else "Ingrese un número válido entre 1 y 24.")
      return()
    }
    hours <- format(hours, trim = TRUE, scientific = FALSE)
    row <- data.frame(
      Type = location_type,
      OtherPlace = if (identical(location_type, "Other")) {
        trimws(safe_text(input$other_place))
      } else {
        ""
      },
      Municipality = safe_text(input$municipality),
      OtherMunicipality = if (identical(safe_text(input$municipality), "Other")) {
        trimws(safe_text(input$other_municipality))
      } else {
        ""
      },
      Address = address,
      Latitude = format(latitude, digits = 8),
      Longitude = format(longitude, digits = 8),
      Days = days,
      Hours = hours,
      stringsAsFactors = FALSE
    )
    municipality_for_check <- {
      if (identical(safe_text(input$municipality), "Other")) {
        safe_text(input$other_municipality)
      } else {
        safe_text(input$municipality)
      }
    }
    reference <- municipality_reference_point(municipality_for_check)
    if (!is.null(reference)) {
      distance <- distance_km(
        as.numeric(row$Latitude), as.numeric(row$Longitude),
        reference$lat, reference$lon
      )
      if (distance > MUNICIPALITY_WARNING_DISTANCE_KM) {
        english <- identical(safe_text(input$language, "es"), "en")
        pending_location(row)
        showModal(modalDialog(
          title = if (english) "Check location" else "Verifique la ubicación",
          paste0(
            if (english) "The selected point is approximately " else "El punto seleccionado está aproximadamente a ",
            format(round(distance, 1), nsmall = 1),
            if (english) {
              paste0(" km from the selected municipality (", municipality_for_check,
                     "). You can return to the map or add it anyway.")
            } else {
              paste0(" km del municipio seleccionado (", municipality_for_check,
                     "). Puede regresar al mapa o agregarlo de todos modos.")
            }
          ),
          footer = tagList(
            modalButton(if (english) "Return to map" else "Regresar al mapa"),
            actionButton("confirm_far_location", if (english) "Add anyway" else "Agregar de todos modos", class = "btn-warning")
          ),
          easyClose = TRUE
        ))
        return()
      }
    }
    committed <- commit_location(row)
    if (isTRUE(committed) && !nzchar(trimws(safe_text(input$municipality)))) {
      status_message("Ubicación agregada al paquete. Municipio no especificado; puede seleccionarlo para mejorar la búsqueda.")
    }
  })

  observeEvent(input$confirm_far_location, {
    row <- pending_location()
    req(!is.null(row))
    pending_location(NULL)
    removeModal()
    commit_location(row)
  })

  observeEvent(input$copy_packet, {
    packet <- location_packet()
    req(nrow(packet) > 0)
    lines <- unlist(lapply(seq_len(nrow(packet)), function(i) {
      row <- packet[i, , drop = FALSE]
      if (identical(row$Type, "Primary sleeping place")) {
        c(
          paste0("Lugar donde durmi", intToUtf8(243), " principalmente"),
          if (nzchar(row$Municipality) && !identical(row$Municipality, "Other")) paste0("Municipio: ", row$Municipality),
          if (nzchar(row$OtherMunicipality)) paste0("Especifique otro municipio: ", row$OtherMunicipality),
          paste0("Direcci", intToUtf8(243), "n del lugar donde durmi", intToUtf8(243), " principalmente: ", row$Address),
          paste0("Coordenadas - Latitud: ", row$Latitude),
          paste0("Coordenadas - Longitud: ", row$Longitude),
          ""
        )
      } else {
        address_labels <- c(
          "Work" = paste0("Ubicaci", intToUtf8(243), "n del trabajo"),
          "School" = paste0("Ubicaci", intToUtf8(243), "n de la escuela"),
          "Market" = paste0("Ubicaci", intToUtf8(243), "n del mercado"),
          "Family/friend home" = paste0("Ubicaci", intToUtf8(243), "n de la casa de familiar/amigo"),
          "Church" = paste0("Ubicaci", intToUtf8(243), "n de la iglesia"),
          "Recreation/play" = paste0("Ubicaci", intToUtf8(243), "n de la recreaci", intToUtf8(243), "n"),
          "Other" = paste0("Ubicaci", intToUtf8(243), "n de otro lugar")
        )
        place_labels <- c(
          "Work" = "trabajo", "School" = "escuela", "Market" = "mercado",
          "Family/friend home" = "la casa", "Church" = "la iglesia",
          "Recreation/play" = paste0("la recreaci", intToUtf8(243), "n"), "Other" = "otro lugar"
        )
        address_label <- unname(address_labels[row$Type])
        place <- unname(place_labels[row$Type])
        c(
          paste0(address_label, ": ", row$Address),
          if (nzchar(row$OtherPlace)) paste0("Especifique otro lugar: ", row$OtherPlace),
          paste0("Coordenadas - Latitud de ", place, ": ", row$Latitude),
          paste0("Coordenadas - Longitud de ", place, ": ", row$Longitude),
          paste0("D", intToUtf8(237), "as que pas", intToUtf8(243), " en ", place, ": ", row$Days),
          paste0("Horas que pas", intToUtf8(243), " en ", place, ": ", row$Hours),
          ""
        )
      }
    }))
    session$sendCustomMessage("copyText", list(text = paste(lines, collapse = "\n")))
    status_message("Full location packet copied to clipboard.")
  })

  observeEvent(input$clear_packet, {
    location_packet(location_packet()[0, , drop = FALSE])
    editing_packet_index(NULL)
    updateActionButton(session, "add_location", label = "Agregar al paquete")
    status_message("Packet cleared.")
  })

  observeEvent(input$reset, {
    current_point(NULL)
    current_address("")
    candidates(data.frame())
    validation_state(list())
    editing_packet_index(NULL)
    status_message("Ubicación actual limpiada.")
    updateTextInput(session, "address_query", value = "")
    updateTextInput(session, "other_place", value = "")
    updateTextInput(session, "standard_address_edit", value = "")
    updateTextInput(session, "latitude_edit", value = "")
    updateTextInput(session, "longitude_edit", value = "")
    updateActionButton(session, "add_location", label = "Agregar al paquete")
    updateTextInput(session, "days_at_place", value = "")
    updateNumericInput(session, "hours_at_place", value = NULL)
    session$sendCustomMessage("googleMapClear", list())
    session$sendCustomMessage(
      "googleMapSetView",
      list(lat = STUDY_AREA$center_lat, lng = STUDY_AREA$center_lng, zoom = STUDY_AREA$zoom)
    )
  })

  output$standard_address <- renderText(current_address())
  output$latitude <- renderText({
    point <- current_point()
    if (is.null(point)) "" else format(point$lat, digits = 8)
  })
  output$longitude <- renderText({
    point <- current_point()
    if (is.null(point)) "" else format(point$lon, digits = 8)
  })
}

shinyApp(ui, server)
