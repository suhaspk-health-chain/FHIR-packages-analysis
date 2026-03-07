# ==========================================================
# SCRIPT: 00_setup_packages.R
# PURPOSE: Install and load all required R packages for the project.
# AUTHOR: Suhas. P. K
# LAST MODIFIED: 2025-11-17
# ==========================================================

# ---- Define Required Packages ----
pkgs <- c(
  "jsonlite",     # JSON parsing
  "dplyr",        # Data manipulation
  "tidyr",        # Data tidying
  "stringr",      # String operations
  "lubridate",    # Date/time manipulation
  "readr",        # Fast data reading
  "arrow",        # Apache Arrow I/O
  "ggplot2",      # Data visualization
  "scales",       # Scale functions for viz
  "forcats",      # Factor handling
  "DataExplorer", # Data exploration
  "cowplot",      # Plot composition
  "magick",       # Image handling
  "grid"          # Grid graphics
)

# ---- Install Missing Packages ----
new_pkgs <- setdiff(pkgs, rownames(installed.packages()))
if (length(new_pkgs) > 0) {
  install.packages(new_pkgs, repos = "https://cloud.r-project.org")
}

# ---- Load All Packages ----
invisible(lapply(pkgs, library, character.only = TRUE))
