# ==========================================================
# SCRIPT: 01_config.R
# PURPOSE: Define global project paths and ensure directory structure.
# AUTHOR: Suhas. P. K
# LAST MODIFIED: 2025-11-17
# ==========================================================

# ---- Directory Paths (relative to project root) ----
OUT_DIR     <- "data"                    # Main project data folder
RAW_DIR     <- file.path(OUT_DIR, "raw") # Raw input data
INTERIM_DIR <- file.path(OUT_DIR, "interim") # Intermediate data
PROC_DIR    <- file.path(OUT_DIR, "processed") # Final processed data
FIG_DIR     <- "figs"                    # Plots and figures

# ---- Data File Names ----
DATA_RAW_JSON   <- file.path(RAW_DIR, "xig_resources.json")
DATA_RAW_NDJSON <- file.path(RAW_DIR, "xig_resources.ndjson")

# ---- Auto-create Required Directories ----
dirs <- c(OUT_DIR, RAW_DIR, INTERIM_DIR, PROC_DIR, FIG_DIR)
invisible(lapply(dirs, dir.create, showWarnings = FALSE, recursive = TRUE))
