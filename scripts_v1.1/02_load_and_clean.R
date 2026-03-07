source("D:/health-chain-repository/HC Data analysis/FHIR-packages-analysis/scripts/00_setup_packages.R"); 
source("D:/health-chain-repository/HC Data analysis/FHIR-packages-analysis/scripts/01_config.R")

# ==========================================================
# Load the main FHIR package data (JSON format)
# ==========================================================

library(jsonlite)
library(dplyr)

# Define paths
DATA_JSON <- "data/raw/xig_resources.json"
DATA_NDJSON <- "data/raw/xig_resources.ndjson"

# ---- 1️⃣ Preferred: load JSON array file ----
if (file.exists(DATA_JSON)) {
  message("✅ Loading data from JSON: ", DATA_JSON)
  df <- fromJSON(DATA_JSON, flatten = TRUE) %>%
    as_tibble()
  
  # ---- 2️⃣ Fallback: load NDJSON line-by-line ----
} else if (file.exists(DATA_NDJSON)) {
  message("⚠️ JSON not found. Loading from NDJSON: ", DATA_NDJSON)
  con <- file(DATA_NDJSON, "r", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  
  acc <- list(); i <- 0L
  repeat {
    ln <- readLines(con, n = 1L)
    if (!length(ln)) break
    i <- i + 1L
    acc[[i]] <- jsonlite::fromJSON(ln)
  }
  
  df <- bind_rows(acc) %>% as_tibble()
  
  # ---- 3️⃣ Fail safe ----
} else {
  stop("❌ No input JSON or NDJSON file found in data/raw/.")
}

# Quick validation summary
message("📦 Records loaded: ", nrow(df))
message("🧩 Columns available: ", paste(names(df), collapse = ", "))


# 2) Normalize blanks/whitespace -> NA across all columns
df <- df %>% mutate(across(everything(), ~ na_if(trimws(as.character(.)), "")))

# 3) Standardize key fields
df <- df %>%
  rename(
    package = package_text,
    package_url = package_href,
    identity   = identity_text,
    identity_url = identity_href,
    title = name_title
  ) %>%
  mutate(
    version = str_to_upper(version),                 # e.g., R4, R5
    date = suppressWarnings(lubridate::ymd(date)),   # parse YYYY-MM-DD/.. best-effort
    status = str_to_lower(status),
    wg     = str_to_lower(wg),
    realm  = str_to_lower(realm),
    auth   = str_to_lower(auth)
  )

# 4) Persist cleaned data
arrow::write_parquet(df, file.path(OUT_DIR, "interim/fhir_packages_clean.parquet"))
readr::write_csv(df, file.path(OUT_DIR, "interim/fhir_packages_clean.csv"))
