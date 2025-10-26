# ==============================================================
# 13_build_shiny_data.R
# Bundle all processed datasets + original XIG JSON for Shiny app
# ==============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fs)
  library(jsonlite)
})

# --- Ensure app folders exist ---------------------------------
dir_create("shiny_app")
dir_create("shiny_app/data")
dir_create("shiny_app/www")

# --- Copy brand CSS -------------------------------------------
if (file_exists("styles/custom-dark-lite.css")) {
  file_copy("styles/custom-dark-lite.css",
            "shiny_app/www/custom-dark-lite.css",
            overwrite = TRUE)
}

# --- Helper to safely load CSVs -------------------------------
load_csv <- function(name) {
  p <- file.path("data/processed", name)
  if (file.exists(p)) read_csv(p, show_col_types = FALSE) else NULL
}

# --- Load all known processed datasets ------------------------
data_list <- list(
  resources                             = load_csv("fhir_resources.csv"),
  top_resource_types                    = load_csv("top_resource_types.csv"),
  packages_by_version                   = load_csv("packages_by_version.csv"),
  resource_types_by_version             = load_csv("resource_types_by_version.csv"),
  summary_resource_counts_by_version    = load_csv("summary_resource_counts_by_version.csv"),
  resource_presence_matrix              = load_csv("resource_presence_matrix.csv"),
  resources_added_removed_by_transition = load_csv("resources_added_removed_by_transition.csv"),
  delta_counts                          = load_csv("delta_counts.csv"),
  stable_resources_ge4                  = load_csv("stable_resources_ge4.csv"),
  resource_presence_long                = load_csv("resource_presence_long.csv")
)

# --- Load original raw JSON (if present) -----------------------
raw_json_path <- "data/raw/xig_resources.json"
if (!file.exists(raw_json_path)) {
  raw_json_path <- "xig_resources.json"  # fallback at project root
}

raw_json_preview <- NULL
if (file.exists(raw_json_path)) {
  message("📦 Found XIG raw JSON: ", raw_json_path)
  # store a lightweight preview (first 5 entries) + file info
  try({
    preview <- fromJSON(raw_json_path, flatten = TRUE)
    raw_json_preview <- utils::head(preview, 5)
  }, silent = TRUE)
} else {
  message("⚠️ No xig_resources.json found.")
}

# --- Metadata -------------------------------------------------
meta <- list(
  built_at = Sys.time(),
  source_dirs = list(
    processed = "data/processed",
    raw = dirname(raw_json_path)
  ),
  raw_json_file = basename(raw_json_path),
  notes = "Bundle generated for FHIR Packages Shiny dashboard. Includes preview of raw XIG JSON (first 5 rows)."
)

# --- Bundle all together --------------------------------------
bundle <- list(
  data = data_list,
  meta = meta,
  raw_json_preview = raw_json_preview
)

# --- Save RDS -------------------------------------------------
bundle_path <- "shiny_app/data/fhir_dashboard.rds"
saveRDS(bundle, file = bundle_path)
message("💾 Saved bundle to: ", bundle_path)

# --- Optional: copy full JSON for archival --------------------
if (file.exists(raw_json_path)) {
  file_copy(raw_json_path,
            "shiny_app/data/xig_resources.json",
            overwrite = TRUE)
  message("📁 Copied full xig_resources.json → shiny_app/data/")
}

# --- Summary --------------------------------------------------
n_main <- if (!is.null(data_list$resources)) nrow(data_list$resources) else 0L
message("✅ Bundle completed. fhir_resources rows: ", n_main)
