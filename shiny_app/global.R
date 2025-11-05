# ==============================================================
# global.R — FHIR Packages Dashboard Global Setup
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
  library(cowplot)
  library(magick)
  library(RColorBrewer)
  library(forcats)
})

# ---- Paths and Config --------------------------------------------------------
message("Loading configuration...")

# All data is inside shiny_app/data/
DATA_DIR <- "data"

# ---- Source theme (optional - if it exists outside shiny_app) ---------------
theme_file <- "../scripts/05_theme_healthchain.R"
if (file.exists(theme_file)) {
  source(theme_file)
  message("✅ Health Chain theme loaded successfully")
} else {
  message("⚠️  Theme file not found, using default theme")
  # Fallback theme
  theme_healthchain <- function(base_size = 12) {
    theme_minimal(base_size = base_size) +
      theme(
        plot.title = element_text(color = "#f26d21", face = "bold", size = base_size * 1.2),
        plot.subtitle = element_text(color = "#666666"),
        axis.title = element_text(face = "bold")
      )
  }
}

# ---- Logo path for plots -----------------------------------------------------
HC_LOGO_PATH <- "assets/health-chain-full-logo.png"
if (file.exists(HC_LOGO_PATH)) {
  message("✅ Logo found at: ", HC_LOGO_PATH)
} else {
  message("⚠️  Logo not found, will skip logo in exports")
  HC_LOGO_PATH <- ""
}

# ---- Load main resources data ------------------------------------------------
# Try xig_resources.json (JSON format) first
resources_file <- file.path(DATA_DIR, "xig_resources.json")

if (!file.exists(resources_file)) {
  # Try CSV format as fallback
  resources_file <- file.path(DATA_DIR, "xig_resources.csv")
  
  if (file.exists(resources_file)) {
    message("Loading resources from CSV: ", resources_file)
    resources_tbl <- read_csv(resources_file, col_types = cols(), show_col_types = FALSE)
  } else {
    stop("❌ Resources file not found!\n",
         "   Expected: data/xig_resources.json or data/xig_resources.csv")
  }
} else {
  message("Loading resources from JSON: ", resources_file)
  resources_tbl <- fromJSON(resources_file, flatten = TRUE)
}

message("✅ Resources loaded: ", nrow(resources_tbl), " rows")

# ---- Load evolution data (delta_counts) --------------------------------------
delta_file <- file.path(DATA_DIR, "resources_added_removed_by_transition.csv")

if (file.exists(delta_file)) {
  delta_counts <- read_csv(delta_file, col_types = cols(), show_col_types = FALSE)
  
  # Check column names and rename if needed
  if ("added" %in% names(delta_counts) && "removed" %in% names(delta_counts)) {
    delta_counts <- delta_counts %>%
      rename(
        added_resources = added,
        removed_resources = removed
      ) %>%
      mutate(
        # Calculate counts from comma-separated strings
        added = sapply(added_resources, function(x) {
          if (is.na(x) || x == "" || x == "NA") return(0)
          length(strsplit(as.character(x), ",\\s*")[[1]])
        }),
        removed = sapply(removed_resources, function(x) {
          if (is.na(x) || x == "" || x == "NA") return(0)
          length(strsplit(as.character(x), ",\\s*")[[1]])
        })
      )
  }
  
  message("✅ Evolution data loaded: ", nrow(delta_counts), " transitions")
} else {
  message("⚠️  Evolution CSV not found. Creating empty table.")
  delta_counts <- tibble(
    transition = character(),
    added_resources = character(),
    removed_resources = character(),
    added = numeric(),
    removed = numeric()
  )
}

# ---- Load presence matrix ----------------------------------------------------
matrix_file <- file.path(DATA_DIR, "resource_presence_matrix.csv")

if (file.exists(matrix_file)) {
  matrix_tbl <- read_csv(matrix_file, col_types = cols(), show_col_types = FALSE)
  message("✅ Presence matrix loaded: ", nrow(matrix_tbl), " resources")
} else {
  message("⚠️  Presence matrix not found")
  matrix_tbl <- tibble()
}

# ---- Load stable resources (≥4 versions) -------------------------------------
stable_file <- file.path(DATA_DIR, "stable_resources.csv")

if (file.exists(stable_file)) {
  stable_tbl <- read_csv(stable_file, col_types = cols(), show_col_types = FALSE)
  message("✅ Stable resources loaded: ", nrow(stable_tbl), " resources")
} else {
  # Compute from matrix if available
  if (nrow(matrix_tbl) > 0) {
    stable_tbl <- matrix_tbl %>%
      mutate(presence_count = rowSums(select(., -resource))) %>%
      filter(presence_count >= 4) %>%
      arrange(desc(presence_count), resource)
    message("✅ Stable resources computed: ", nrow(stable_tbl), " resources")
  } else {
    message("⚠️  Stable resources not available")
    stable_tbl <- tibble()
  }
}

# ---- Raw preview data --------------------------------------------------------
raw_preview <- resources_tbl %>% slice_head(n = 100)

# ---- KPI helper functions ----------------------------------------------------
kpi_rows <- function(df) nrow(df)
kpi_n_distinct <- function(vec) n_distinct(vec)
kpi_authors_nonempty <- function(df) {
  df %>% 
    filter(!is.na(auth), auth != "") %>% 
    pull(auth) %>% 
    n_distinct()
}

# ---- Meta information --------------------------------------------------------
meta <- list(
  built_at = Sys.time(),
  source_dirs = list(
    processed = normalizePath(DATA_DIR, mustWork = FALSE),
    raw = normalizePath(DATA_DIR, mustWork = FALSE)
  )
)

message("✅ Global.R loaded successfully")
message(paste(rep("=", 60), collapse = ""))
