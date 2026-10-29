app_path <- file.path("geocoder_clinical", "app.R")
source(app_path, local = FALSE)

fixture <- read.csv(
  file.path("geocoder_clinical", "tests", "test_addresses.csv"),
  stringsAsFactors = FALSE
)

corpus <- read.csv(
  file.path("geocoder_clinical", "tests", "guatemala_address_corpus.csv"),
  na.strings = c("", "NA"),
  stringsAsFactors = FALSE
)

stopifnot(normalize_address_query("Magnolia, Coatepeque, Quetzaltenango, 09020, Guatemala") ==
            "Magnolia, Coatepeque, Quetzaltenango, 09020")
stopifnot(valid_average_hours("1"))
stopifnot(valid_average_hours("24"))
stopifnot(valid_average_hours("2.5"))
stopifnot(!valid_average_hours("0"))
stopifnot(!valid_average_hours("24.1"))
stopifnot(!valid_average_hours("2:30"))
stopifnot(normalize_address_query(paste(
  "Standardized address: Magnolia, Coatepeque, Quetzaltenango, 09020, Guatemala",
  "Latitude: 14.688786",
  "Longitude: -91.885564",
  sep = "\n"
)) == "Magnolia, Coatepeque, Quetzaltenango, 09020")
stopifnot(any(grepl("avenida", query_variants("5a Av., Coatepeque"), fixed = TRUE)))
stopifnot(any(grepl("calle", query_variants("3a Cal., Coatepeque"), fixed = TRUE)))
stopifnot(any(grepl("zona 1", query_variants("z. 1, Coatepeque"), fixed = TRUE)))
stopifnot(any(grepl("lotificacion", query_variants("Lot. Magnolia, Coatepeque"), fixed = TRUE)))
stopifnot(any(!grepl("09020", query_variants("Magnolia, Coatepeque, 09020"), fixed = TRUE)))
stopifnot(any(grepl("carretera al pacifico", tolower(query_variants("km 210 carretera al Pacifico Coatepeque")), fixed = TRUE)))
stopifnot(any(grepl("aldea el gariton", tolower(query_variants("Aldea El Gariton Guatemala")), fixed = TRUE)))
stopifnot(any(grepl("parcelamiento tierra linda", tolower(query_variants("Parcelamiento Tierra Linda Guatemala")), fixed = TRUE)))
stopifnot(any(grepl("colonia centro", tolower(query_variants("Col. Centro Coatepeque")), fixed = TRUE)))
stopifnot(qualify_search_query("iglesia del centro") ==
            "iglesia del centro, Coatepeque, Guatemala")
stopifnot(qualify_search_query("iglesia del centro, Flores Costa Cuca") ==
            "iglesia del centro, Flores Costa Cuca")
stopifnot(qualify_search_query("iglesia del centro", "Retalhuleu") ==
            "iglesia del centro, Retalhuleu, Guatemala")
stopifnot(qualify_search_query("iglesia del centro, Retalhuleu", "Retalhuleu") ==
            "iglesia del centro, Retalhuleu")
stopifnot(qualify_search_query("iglesia del centro", "Municipality not specified") ==
            "iglesia del centro, Coatepeque, Guatemala")
landmark_variants <- landmark_query_variants("Galves, Flores Costa Cuca, proximo iglesia principe")
stopifnot(any(grepl("iglesia principe, galves, flores costa cuca",
                     tolower(landmark_variants), fixed = TRUE)))
stopifnot(any(grepl("escuela galves flores costa cuca",
                     tolower(landmark_query_variants("frente a la escuela Galves Flores Costa Cuca")),
                     fixed = TRUE)))
stopifnot(any(grepl("iglesia del centro coatepeque",
                     tolower(landmark_query_variants("por la iglesia del centro Coatepeque")),
                     fixed = TRUE)))

landmark_result <- list(list(
  display_name = "Iglesia Principe de Paz, Galves, Flores Costa Cuca, Guatemala",
  lat = 14.69959,
  lon = -91.8642,
  type = "church",
  class = "amenity",
  importance = 0.4
))
landmark_ranked <- rank_geocoder_results(
  landmark_result,
  "Galves, Flores Costa Cuca, proximo iglesia principe"
)
stopifnot(nrow(landmark_ranked) == 1)
stopifnot(abs(landmark_ranked$lat[[1]] - 14.69959) < 0.01)
local_landmark <- search_local_landmarks(
  "galves, flores costa cuca, proximo iglesia principe"
)
stopifnot(length(local_landmark) == 1)
stopifnot(abs(as.numeric(local_landmark[[1]]$lat) - 14.69959) < 0.01)
stopifnot(length(search_local_landmarks(
  "iglesia principe de paz, Retalhuleu, Guatemala"
)) == 0)

fallback <- fallback_search_viewbox()
stopifnot(length(fallback) == 4)
stopifnot(point_in_viewbox(STUDY_AREA$center_lat, STUDY_AREA$center_lng, fallback))
stopifnot(distance_km(14.6349, -90.5069, STUDY_AREA$center_lat, STUDY_AREA$center_lng) > SEARCH_ENVELOPE_KM)
stopifnot(identical(nominatim_viewbox(c(-92, 14, -91, 15)), c(-92, 15, -91, 14)))
fallback_point <- fallback_point_result("unmatched informal description")
stopifnot(nrow(fallback_point) == 1)
stopifnot(point_in_viewbox(fallback_point$lat, fallback_point$lon, fallback))

mock_results <- lapply(seq_len(nrow(fixture[1:2, ])), function(i) {
  list(
    display_name = fixture$address[[i]],
    lat = fixture$latitude[[i]],
    lon = fixture$longitude[[i]],
    type = "town",
    class = "place",
    importance = 0.7
  )
})
ranked <- rank_geocoder_results(mock_results, fixture$address[[1]])
stopifnot(nrow(ranked) >= 1)
stopifnot(abs(ranked$lat[[1]] - fixture$latitude[[1]]) < 0.01)
stopifnot(abs(ranked$lon[[1]] - fixture$longitude[[1]]) < 0.01)

far_result <- list(list(
  display_name = "Guatemala City, Guatemala",
  lat = 14.6349,
  lon = -90.5069,
  type = "city",
  class = "place",
  importance = 0.8
))
stopifnot(nrow(rank_geocoder_results(far_result, "Guatemala City")) == 0)
explicit_distant <- rank_geocoder_results(
  far_result,
  "Guatemala City",
  max_distance_km = Inf
)
stopifnot(nrow(explicit_distant) == 1)

inside_result <- list(list(
  display_name = "Iglesia, Coatepeque, Guatemala",
  lat = 14.70413,
  lon = -91.86426,
  type = "church",
  class = "amenity",
  importance = 0.3
))
outside_result <- list(list(
  display_name = "Iglesia Principe, Retalhuleu, Guatemala",
  lat = 14.53611,
  lon = -91.67778,
  type = "church",
  class = "amenity",
  importance = 0.9
))
priority_ranked <- rank_geocoder_results(
  c(outside_result, inside_result),
  "Iglesia Principe Coatepeque"
)
stopifnot(nrow(priority_ranked) == 2)
stopifnot(abs(priority_ranked$lat[[1]] - 14.70413) < 0.01)
stopifnot(distance_km(14.53611, -91.67778, 14.70413, -91.86426) < SEARCH_ENVELOPE_KM)

# Every corpus entry must normalize and produce at least one usable search form.
# Rows with coordinates are recorded regression anchors; rows without them are
# pattern-coverage cases for live geocoder evaluation.
for (i in seq_len(nrow(corpus))) {
  normalized <- normalize_address_query(corpus$query[[i]])
  stopifnot(nzchar(normalized))
  variants <- query_variants(corpus$query[[i]])
  stopifnot(length(variants) >= 1)
  if (!is.na(corpus$expected_variant[[i]])) {
    stopifnot(any(grepl(
      tolower(corpus$expected_variant[[i]]),
      tolower(variants),
      fixed = TRUE
    )))
  }
}

if (identical(Sys.getenv("RUN_LIVE_GEOCODER_TESTS", unset = "0"), "1")) {
  for (i in seq_len(nrow(fixture))) {
    result <- search_addresses(fixture$address[[i]])
    stopifnot(nrow(result) > 0)
    nearest <- result[which.min(result$distance_km), , drop = FALSE]
    stopifnot(distance_km(
      nearest$lat, nearest$lon,
      fixture$latitude[[i]], fixture$longitude[[i]]
    ) <= fixture$tolerance_km[[i]])
    Sys.sleep(1.1)
  }
}

if (identical(Sys.getenv("RUN_LIVE_GOOGLE_PLACES_TESTS", unset = "0"), "1") &&
    USE_GOOGLE_PLACES && nzchar(GOOGLE_PLACES_API_KEY)) {
  result <- search_addresses(
    "galves, flores costa cuca, proximo iglesia principe"
  )
  stopifnot(nrow(result) > 0)
  nearest <- result[which.min(distance_km(
    result$lat, result$lon, 14.69959, -91.8642
  )), , drop = FALSE]
  stopifnot(distance_km(
    nearest$lat, nearest$lon, 14.69959, -91.8642
  ) <= 1)
}

cat("BACKEND_TESTS_OK\n")
