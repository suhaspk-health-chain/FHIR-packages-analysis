# ==============================================================================
# global.R - COMPLETE FIXED VERSION
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(DT)
  library(scales)
  library(jsonlite)
  library(readr)
  library(bslib)
  library(cowplot)
  library(magick)
  library(arrow)  # For reading Parquet files
})

# Load utility functions early (needed by global.R itself)
source(file.path(getwd(), "R", "utils.R"), local = FALSE)

# Get the app directory
APP_DIR <- getwd()
message(paste("App directory:", APP_DIR))

# Data paths
DATA_DIR <- file.path(APP_DIR, "data")
PROC_DIR <- file.path(DATA_DIR, "processed")

message(paste("Data directory:", DATA_DIR))
message(paste("Processed directory:", PROC_DIR))

# Verify data directory exists
if (!dir.exists(PROC_DIR)) {
  stop("❌ Data directory not found at: ", PROC_DIR)
}

# Load theme (05_theme_healthchain.R lives at shiny_app root, not in a scripts subfolder)
theme_script <- file.path(APP_DIR, "05_theme_healthchain.R")

if (file.exists(theme_script)) {
  tryCatch({
    source(theme_script, local = TRUE)
    message("✅ Theme loaded successfully")
  }, error = function(e) {
    message("⚠️ Theme loading failed: ", e$message)
  })
} else {
  warning("⚠️ Theme file not found at: ", theme_script)
}

# Helper function
map_column <- function(df, possible_names, default_value = NA) {
  for (name in possible_names) {
    if (name %in% colnames(df)) {
      return(df[[name]])
    }
  }
  return(rep(default_value, nrow(df)))
}

# ---- Load Data Files ----
xig_resources <- data.frame()
ig_all <- data.frame()
matrix_tbl <- data.frame()
stable_tbl <- data.frame()
transitions_tbl <- data.frame()

# Load xig_resources from JSON (36 MB file - may take time)
tryCatch({
  message("Loading xig_resources.json (36 MB)...")
  xig_resources_raw <- jsonlite::fromJSON(file.path(PROC_DIR, "xig_resources.json"))
  
  # Convert to data frame if it's a list
  if (is.list(xig_resources_raw) && !is.data.frame(xig_resources_raw)) {
    xig_resources <- as.data.frame(xig_resources_raw)
  } else {
    xig_resources <- xig_resources_raw
  }
  
  message(paste("✅ xig_resources.json loaded:", nrow(xig_resources), "rows"))
}, error = function(e) {
  message("⚠️ xig_resources.json failed: ", e$message)
  
  # Fallback: Try loading fhir_resources.parquet
  tryCatch({
    xig_resources <<- arrow::read_parquet(file.path(PROC_DIR, "fhir_resources.parquet"))
    message(paste("✅ Loaded fhir_resources.parquet as fallback:", nrow(xig_resources), "rows"))
  }, error = function(e2) {
    message("⚠️ Both JSON and Parquet failed")
  })
})

# Load ig_all from CSV
tryCatch({
  ig_all <- read_csv(file.path(PROC_DIR, "fhir_ig_all_official.csv"), 
                     col_types = cols(.default = "c"),
                     show_col_types = FALSE)
  message(paste("✅ fhir_ig_all_official.csv loaded:", nrow(ig_all), "rows"))
}, error = function(e) {
  message("⚠️ fhir_ig_all_official.csv not found")
})

# Load matrix from CSV
tryCatch({
  matrix_tbl <- read_csv(file.path(PROC_DIR, "resource_presence_matrix.csv"), 
                         col_types = cols(.default = "c"),
                         show_col_types = FALSE)
  message(paste("✅ resource_presence_matrix.csv loaded:", nrow(matrix_tbl), "rows"))
}, error = function(e) {
  message("⚠️ resource_presence_matrix.csv not found")
})

# Load stable resources
tryCatch({
  stable_tbl <- read_csv(file.path(PROC_DIR, "stable_resources_ge4.csv"), 
                         col_types = cols(.default = "c"),
                         show_col_types = FALSE)
  message(paste("✅ stable_resources_ge4.csv loaded:", nrow(stable_tbl), "rows"))
}, error = function(e) {
  message("⚠️ stable_resources_ge4.csv not found")
})

# Load transitions data
tryCatch({
  transitions_tbl <- read_csv(file.path(PROC_DIR, "resources_added_removed_by_transition.csv"), 
                              col_types = cols(.default = "c"),
                              show_col_types = FALSE)
  message(paste("✅ resources_added_removed_by_transition.csv loaded:", nrow(transitions_tbl), "rows"))
}, error = function(e) {
  message("⚠️ resources_added_removed_by_transition.csv not found")
})

# Build resources_tbl (normalized column view used by all modules)
resources_tbl <- if (nrow(xig_resources) > 0) create_resources_tbl(xig_resources) else data.frame()
raw_preview <- if (nrow(xig_resources) > 0) head(xig_resources, 100) else data.frame()

# Meta object (used by mod_overview for "Built at" display)
meta <- list(
  built_at    = Sys.time(),
  source_dirs = list(processed = PROC_DIR, raw = DATA_DIR)
)

# Global metrics
TOTAL_RESOURCES <- nrow(xig_resources)
TOTAL_PACKAGES <- if (nrow(xig_resources) > 0) {
  if ("package_text" %in% colnames(xig_resources)) {
    n_distinct(xig_resources$package_text)
  } else if ("package" %in% colnames(xig_resources)) {
    n_distinct(xig_resources$package)
  } else {
    0
  }
} else {
  0
}

# Aggregations for visualization
ig_us <- data.frame()
ig_us_by_version <- tibble(fhir_version = character(), n = integer())
ig_us_by_category <- tibble(category = character(), n = integer())
ig_realm_count <- tibble(realm = character(), n = integer())

if (nrow(ig_all) > 0 && "realm" %in% colnames(ig_all)) {
  ig_us <- ig_all %>% filter(tolower(realm) == "us")
  
  if (nrow(ig_us) > 0 && "fhir_version" %in% colnames(ig_us)) {
    ig_us_by_version <- ig_us %>% count(fhir_version, sort = TRUE)
  }
  
  if (nrow(ig_us) > 0 && "category" %in% colnames(ig_us)) {
    ig_us_by_category <- ig_us %>% count(category, sort = TRUE) %>% head(10)
  }
  
  ig_realm_count <- ig_all %>% count(realm, sort = TRUE) %>% head(12)
}

# ---- Derived aggregations used by mod_verification.R modules ----

# Realm summary for mod_global (resource-level)
realm_summary <- if (nrow(xig_resources) > 0 && "realm" %in% colnames(xig_resources)) {
  xig_resources %>%
    count(realm, sort = TRUE) %>%
    mutate(count = n, percentage = round(100 * n / sum(n), 1), pct_label = sprintf("%.1f%%", percentage)) %>%
    filter(!is.na(realm), !realm %in% c("", "na", "none", "NA")) %>%
    head(15)
} else {
  tibble(realm = character(), n = integer(), count = integer(), percentage = numeric(), pct_label = character())
}

# Version summary for mod_hierarchy
version_summary <- if (nrow(xig_resources) > 0 && "version" %in% colnames(xig_resources)) {
  xig_resources %>%
    count(version, sort = TRUE) %>%
    mutate(fhir_version = version, count = n,
           percentage = round(100 * n / sum(n), 1), pct_label = sprintf("%.1f%%", percentage))
} else {
  tibble(fhir_version = character(), count = integer(), percentage = numeric(), pct_label = character())
}

# Top 10 resource types for mod_hierarchy
resource_type_top10 <- if (nrow(xig_resources) > 0) {
  rt_col <- if ("identity_text" %in% colnames(xig_resources)) xig_resources$identity_text else xig_resources$identity
  tibble(raw = rt_col) %>%
    mutate(resource_type = stringr::str_extract(raw, "^[^/]+")) %>%
    filter(!is.na(resource_type)) %>%
    count(resource_type, sort = TRUE) %>%
    slice_head(n = 10) %>%
    mutate(count = n, percentage = round(100 * n / sum(n), 1))
} else {
  tibble(resource_type = character(), count = integer(), percentage = numeric())
}

# All resource types for mod_catalog
resource_types <- if (nrow(xig_resources) > 0) {
  rt_col <- if ("identity_text" %in% colnames(xig_resources)) xig_resources$identity_text else xig_resources$identity
  tibble(raw = rt_col) %>%
    mutate(resource_type = stringr::str_extract(raw, "^[^/]+")) %>%
    filter(!is.na(resource_type)) %>%
    count(resource_type, sort = TRUE) %>%
    mutate(count = n, percentage = round(100 * n / sum(n), 1))
} else {
  tibble(resource_type = character(), count = integer(), percentage = numeric())
}

# Resources per package for mod_hierarchy
resources_per_package <- if (nrow(xig_resources) > 0) {
  pkg_col <- if ("package_text" %in% colnames(xig_resources)) "package_text" else "package"
  xig_resources %>%
    group_by(package = .data[[pkg_col]]) %>%
    summarise(
      resource_count = n(),
      versions = paste(unique(version), collapse = ", "),
      realms   = if ("realm" %in% colnames(xig_resources)) paste(unique(realm), collapse = ", ") else "",
      .groups  = "drop"
    ) %>%
    arrange(desc(resource_count))
} else {
  tibble(package = character(), resource_count = integer(), versions = character(), realms = character())
}

# Verification metrics for mod_verification
TOTAL_IGs          <- if (nrow(ig_all) > 0) n_distinct(ig_all$package_id) else 0L
MATCHED_RESOURCES  <- 0L  # updated below if ig_all is available
MATCH_PERCENTAGE   <- 0
AVG_RESOURCES_PER_PACKAGE <- if (TOTAL_PACKAGES > 0) round(TOTAL_RESOURCES / TOTAL_PACKAGES, 1) else 0

# Top packages with IG info for mod_verification table
top_packages_ig <- if (nrow(ig_all) > 0 && nrow(xig_resources) > 0) {
  pkg_col <- if ("package_text" %in% colnames(xig_resources)) "package_text" else "package"
  joined <- xig_resources %>%
    left_join(ig_all %>% select(package_id, name, ig_realm = realm, fhir_version, category) %>%
                rename(!!pkg_col := package_id), by = pkg_col)
  MATCHED_RESOURCES <<- sum(!is.na(joined$name))
  MATCH_PERCENTAGE  <<- if (nrow(joined) > 0) round(100 * MATCHED_RESOURCES / nrow(joined), 1) else 0
  joined %>%
    group_by(package = .data[[pkg_col]], ig_name = name, ig_realm = ig_realm, fhir_version = version) %>%
    summarise(resource_count = n(), .groups = "drop") %>%
    arrange(desc(resource_count)) %>%
    head(20)
} else {
  tibble(package = character(), ig_name = character(), ig_realm = character(),
         fhir_version = character(), resource_count = integer())
}

# delta_counts used by mod_evolution
delta_counts <- if (nrow(transitions_tbl) > 0) {
  load_delta_counts(file.path(PROC_DIR, "resources_added_removed_by_transition.csv"))
} else {
  tibble(transition = character(), added_resources = character(),
         removed_resources = character(), added = numeric(), removed = numeric())
}

message("✅ Global environment ready")
message(paste("   Resources:", TOTAL_RESOURCES))
message(paste("   Packages:", TOTAL_PACKAGES))
