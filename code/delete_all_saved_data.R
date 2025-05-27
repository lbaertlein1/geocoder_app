library(httr)
library(jsonlite)

# ---- Load Dropbox credentials ----
dropbox_keys <- rio::import("data/account_info.xlsx", which = "Dropbox") %>%
  dplyr::select(name, value) %>%
  tibble::deframe()

DROPBOX_UPLOAD_URL    <- dropbox_keys[["DROPBOX_UPLOAD_URL"]]
DROPBOX_UPLOAD_PATH   <- dropbox_keys[["DROPBOX_UPLOAD_PATH"]]
DROPBOX_REFRESH_TOKEN <- dropbox_keys[["DROPBOX_REFRESH_TOKEN"]]
DROPBOX_APP_KEY       <- dropbox_keys[["DROPBOX_APP_KEY"]]
DROPBOX_APP_SECRET    <- dropbox_keys[["DROPBOX_APP_SECRET"]]

# ---- Refresh token ----
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
access_token <- content(token_res)$access_token

if (is.null(access_token)) {
  stop("Failed to refresh Dropbox token.")
}

# ---- Create and upload empty tibble ----
temp_file <- tempfile(fileext = ".rds")
empty_df <- tibble::tibble()  # or list() for your app if it expects a list format
saveRDS(empty_df, temp_file)

upload_res <- httr::POST(
  url = DROPBOX_UPLOAD_URL,
  httr::add_headers(
    Authorization = paste("Bearer", access_token),
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

if (upload_res$status_code == 200) {
  message("Successfully cleared Dropbox data.")
} else {
  stop("Dropbox upload failed: ", content(upload_res, "text"))
}
