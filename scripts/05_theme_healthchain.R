# ============================================
# ggplot Theme — FHIR Packages EDA
# ============================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(cowplot)
  library(magick)
})

HC_SCRIPT_DIR <- tryCatch({
  this_file <- sys.frames()[[1]]$ofile
  if (is.null(this_file)) getwd() else dirname(this_file)
}, error = function(e) getwd())

# --- Color Palette ---
HC_COLORS <- c(
  "#f26d21", "#0c223f", "#123358", "#4a90e2", "#e67e22",
  "#16a085", "#8e44ad", "#c0392b", "#27ae60", "#f39c12"
)

# --- Base Theme ---
theme_healthchain <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title        = element_text(face = "bold", color = "#f26d21", hjust = 0.5, size = base_size + 2),
      axis.title        = element_text(color = "#0c223f"),
      axis.text         = element_text(color = "#0c223f"),
      panel.background  = element_rect(fill = "#ffffff", color = NA),
      plot.background   = element_rect(fill = "#ffffff", color = NA),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      legend.text       = element_text(color = "#f26d21"),
      legend.title      = element_text(color = "#0c223f", face = "bold"),
      strip.background  = element_rect(fill = "#123358", color = NA),
      strip.text        = element_text(color = "#ffffff", face = "bold"),
      panel.grid.major  = element_line(color = "#f26d21", linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      plot.margin       = margin(t = 28, r = 18, b = 16, l = 28)
    )
}
theme_set(theme_healthchain())

# --- Color Scales ---
scale_fill_healthchain  <- function(...) scale_fill_manual(values = HC_COLORS, ...)
scale_color_healthchain <- function(...) scale_color_manual(values = HC_COLORS, ...)

message("Theme loaded")
