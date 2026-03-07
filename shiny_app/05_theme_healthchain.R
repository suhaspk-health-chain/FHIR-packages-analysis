# ============================================
# ggplot Theme — FHIR Packages EDA
# ============================================

suppressPackageStartupMessages(library(ggplot2))

# --- Color Palette ---
HC_COLORS <- c(
  "#f26d21",  # Orange
  "#0c223f",  # Navy
  "#123358",  # Dark Blue
  "#4a90e2",  # Light Blue
  "#e67e22",  # Burnt Orange
  "#16a085",  # Teal
  "#8e44ad",  # Purple
  "#c0392b",  # Red
  "#27ae60",  # Green
  "#f39c12"   # Amber
)

# --- Base Theme ---
theme_healthchain <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title        = element_text(face = "bold", color = "#f26d21", hjust = 0.5, size = base_size + 2),
      plot.subtitle     = element_text(color = "#0c223f", hjust = 0.5, size = base_size - 2),
      axis.title        = element_text(color = "#0c223f", face = "bold"),
      axis.text         = element_text(color = "#0c223f"),
      panel.background  = element_rect(fill = "#ffffff", color = NA),
      plot.background   = element_rect(fill = "#ffffff", color = NA),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      legend.text       = element_text(color = "#0c223f"),
      legend.title      = element_text(color = "#0c223f", face = "bold"),
      strip.background  = element_rect(fill = "#123358", color = NA),
      strip.text        = element_text(color = "#ffffff", face = "bold", size = base_size - 1),
      panel.grid.major  = element_line(color = "#e0e0e0", linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      plot.margin       = margin(t = 20, r = 15, b = 60, l = 15)
    )
}

theme_set(theme_healthchain())

# --- Color Scales ---
scale_fill_healthchain <- function(...) scale_fill_manual(values = HC_COLORS, ...)
scale_color_healthchain <- function(...) scale_color_manual(values = HC_COLORS, ...)

message("Theme loaded")
