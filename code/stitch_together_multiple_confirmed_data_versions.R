# Install if needed: remotes::install_github("ropensci/rdrop2")
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# --- CONFIG ---
app_key <- "<your_app_key>"
app_secret <- "<your_app_secret>"
refresh_token <- "<your_app_refresh_token>"
dropbox_path <- "<your_dropbox_path>"

# Step 1: Refresh Dropbox access token
get_access_token <- function(refresh_token, app_key, app_secret) {
  resp <- httr::POST("https://api.dropboxapi.com/oauth2/token",
                     authenticate(app_key, app_secret),
                     body = list(
                       grant_type = "refresh_token",
                       refresh_token = refresh_token
                     ),
                     encode = "form")
  content(resp)$access_token
}

access_token <- get_access_token(refresh_token, app_key, app_secret)

# Step 2: List all versions
list_revisions <- function(path, access_token) {
  resp <- POST("https://api.dropboxapi.com/2/files/list_revisions",
               add_headers(Authorization = paste("Bearer", access_token)),
               body = list(path = path, limit = 100),
               encode = "json")
  fromJSON(content(resp, as = "text", encoding = "UTF-8"))$entries
}

revs <- list_revisions(dropbox_path, access_token)

# Step 3: Download each revision
download_revision <- function(rev_id, i, access_token) {
  temp <- tempfile(fileext = ".Rds")
  headers <- add_headers(
    Authorization = paste("Bearer", access_token),
    "Dropbox-API-Arg" = jsonlite::toJSON(list(path = dropbox_path, rev = rev_id), auto_unbox = TRUE)
  )
  resp <- httr::POST("https://content.dropboxapi.com/2/files/download", headers)
  writeBin(content(resp, "raw"), temp)
  tryCatch({
    df <- readRDS(temp)
    df$._version <- i
    df$._rev <- rev_id
    df
  }, error = function(e) NULL)
}

all_versions <- purrr::map2(revs$rev, seq_along(revs$rev),
                            ~download_revision(.x, .y, access_token))

# Add manually downloaded versions
manual_paths <- list.files("C:/Users/wxf7/Downloads", pattern = "confirmed_data \\(.*\\)\\.Rds", full.names = TRUE)

manual_versions <- purrr::imap(manual_paths, function(path, i) {
  tryCatch({
    df <- readRDS(path)
    
    # Safely parse timestamp field (if present)
    if ("timestamp" %in% names(df)) {
      ts <- suppressWarnings(as.POSIXct(as.character(df$timestamp), tz = "UTC"))
      df$timestamp <- ts  # ensure it's properly typed
      version_time <- max(ts, na.rm = TRUE)
    } else {
      version_time <- as.POSIXct(NA)
    }
    
    df$._version <- version_time
    df$._rev <- basename(path)
    df
  }, error = function(e) {
    message(sprintf("Failed to read %s: %s", path, e$message))
    NULL
  })
})

all_versions <- lapply(all_versions, function(df) {
  if ("timestamp" %in% names(df)) {
    ts <- suppressWarnings(as.POSIXct(as.character(df$timestamp), tz = "UTC"))
    df$timestamp <- ts
    version_time <- max(ts, na.rm = TRUE)
  } else {
    version_time <- as.POSIXct(NA)
  }
  df$._version <- version_time
  df
})

all_versions <- c(all_versions, manual_versions)

all_versions <- compact(all_versions)

# Define expected numeric columns
numeric_cols <- c("latitude", "longitude", 
                  "validation_latitude", "validation_longitude")

# Coerce them to numeric where they exist
all_versions <- lapply(all_versions, function(df) {
  for (col in numeric_cols) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }
  
  # Coerce timestamp fields
  if ("validation_timestamp" %in% names(df)) {
    # First convert to character to strip attributes, then to POSIXct
    df$validation_timestamp <- as.POSIXct(as.character(df$validation_timestamp), tz = "UTC")
  }
  if ("timestamp" %in% names(df)) {
    df$timestamp <- as.POSIXct(as.character(df$timestamp), tz = "UTC")
  }
  if ("validation_start_time" %in% names(df)) {
    df$validation_start_time <- as.POSIXct(as.character(df$validation_start_time), tz = "UTC")
  }
  if ("start_time" %in% names(df)) {
    df$start_time <- as.POSIXct(as.character(df$start_time), tz = "UTC")
  }
  
  df
})

# Step 4: Combine and deduplicate by most recent SN
combined_data <- bind_rows(all_versions)

# Step 1: Extract main (non-validation) fields
main_data <- combined_data %>%
  select(sn, ._version, ._rev, everything()) %>%
  select(-starts_with("validation_")) %>%
  mutate(
    has_main_coords = !is.na(latitude) & !is.na(longitude)
  ) %>%
  filter(!(user %in% c("BypassUser", "Carol"))) %>%
  group_by(sn) %>%
  arrange(desc(has_main_coords), desc(._version)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-has_main_coords, -._rev) %>%
  filter(!(as.Date(._version) == "2025-06-26"))

# Step 2: Extract validation fields
validation_data <- combined_data %>%
  select(sn, ._version, ._rev, starts_with("validation_")) %>%
  mutate(
    has_valid_coords = !is.na(validation_latitude) & !is.na(validation_longitude)
  ) %>%
  filter(!(validation_user %in% c("BypassUser", "Carol"))) %>%
  group_by(sn) %>%
  arrange(desc(has_valid_coords), desc(._version)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-has_valid_coords, -._rev) %>%
  filter(!(as.Date(._version) == "2025-06-26"))
  

# Step 3: Join by SN
latest <- left_join(main_data, validation_data, by = "sn")

final_data <- latest %>%
  select(colnames(confirmed_data))
