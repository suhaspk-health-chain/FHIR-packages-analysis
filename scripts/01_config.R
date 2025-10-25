# ==============================================================
# 01_config.R — central paths & folders
# ==============================================================

# Rooted relative to the project (.Rproj) directory
OUT_DIR     <- "data"
RAW_DIR     <- file.path(OUT_DIR, "raw")
INTERIM_DIR <- file.path(OUT_DIR, "interim")
PROC_DIR    <- file.path(OUT_DIR, "processed")
FIG_DIR     <- "figs"

# Input raw files (adjust names if different)
DATA_RAW_JSON  <- file.path(RAW_DIR, "xig_resources.json")
DATA_RAW_NDJSON <- file.path(RAW_DIR, "xig_resources.ndjson")

# Ensure folders exist
dirs <- c(OUT_DIR, RAW_DIR, INTERIM_DIR, PROC_DIR, FIG_DIR)
invisible(lapply(dirs, dir.create, showWarnings = FALSE, recursive = TRUE))
