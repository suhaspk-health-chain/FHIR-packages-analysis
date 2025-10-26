# ==============================================================
# global.R — shared objects for the FHIR Packages Dashboard
# ==============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(DT)
  library(stringr)
  library(tidyr)
  library(jsonlite)
  library(readr)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------- Load bundle (for spec diffs etc.) ----------
bundle_path <- "data/fhir_dashboard.rds"
store <- if (file.exists(bundle_path)) readRDS(bundle_path) else list(data = list(), meta = list())
D     <- store$data
meta  <- store$meta

# ---------- Prefer JSON for resources (robust + app-relative paths) ----------
json_candidates <- c(
  "data/xig_resources.json",        # when running from shiny_app/
  "../data/raw/xig_resources.json", # when present outside app
  "../xig_resources.json"           # project root fallback
)
existing  <- json_candidates[file.exists(json_candidates)]
json_path <- if (length(existing) > 0) existing[[1]] else NULL

if (!is.null(json_path) && nzchar(json_path)) {
  message("Loading resources from JSON: ", json_path)
  raw <- jsonlite::fromJSON(json_path, flatten = TRUE)
  resources_tbl <- tibble::as_tibble(raw)
} else {
  message("JSON not found; falling back to bundle data$data$resources")
  resources_tbl <- D$resources %||% tibble::tibble()
}

# ---------- Coerce to character & trim ----------
to_chr <- function(x) if (is.list(x)) as.character(x) else as.character(x)
resources_tbl <- resources_tbl |>
  mutate(across(everything(), to_chr)) |>
  mutate(across(everything(), ~ stringr::str_trim(.)))

# ---------- Column normalization WITHOUT using '.' inside mutate ----------
get_col <- function(df, want, fb = character()) {
  if (want %in% names(df)) return(df[[want]])
  alt <- intersect(fb, names(df))
  if (length(alt)) return(df[[alt[1]]])
  rep(NA_character_, nrow(df))
}
canon_realm <- function(x) {
  y <- tolower(trimws(x))
  y[y %in% c("", "na", "n/a", "null")] <- NA
  y[y %in% c("uv","global","intl","international","world")] <- "uv"
  y[y %in% c("us","usa","united states","united-states")]   <- "us"
  y[y %in% c("nz","new zealand","new-zealand")]             <- "nz"
  y[y %in% c("au","australia")]                              <- "au"
  y[y %in% c("ca","canada")]                                 <- "ca"
  y[y %in% c("uk","gb","united kingdom","great britain")]    <- "uk"
  toupper(y)
}

# Assign normalized columns directly
resources_tbl$package_text  <- get_col(resources_tbl, "package_text",  c("package","Package","pkg"))
resources_tbl$identity_text <- get_col(resources_tbl, "identity_text", c("identity","Identity"))
resources_tbl$version       <- get_col(resources_tbl, "version",       c("Version"))
resources_tbl$status        <- get_col(resources_tbl, "status",        c("Status"))
resources_tbl$auth          <- get_col(resources_tbl, "auth",          c("Author","author"))
resources_tbl$realm_raw     <- get_col(resources_tbl, "realm",         c("Realm"))
resources_tbl$realm         <- canon_realm(resources_tbl$realm_raw)

# ---------- Build choices from the live data ----------
with_missing <- function(x) {
  vals <- sort(unique(na.omit(x)))
  if (any(is.na(x))) c(vals, "(missing)") else vals
}
choices <- list(
  version = sort(unique(na.omit(resources_tbl$version))),
  status  = sort(unique(na.omit(resources_tbl$status))),
  author  = sort(unique(na.omit(resources_tbl$auth))),
  realm   = with_missing(resources_tbl$realm)
)
