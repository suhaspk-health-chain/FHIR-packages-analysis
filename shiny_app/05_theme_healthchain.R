# ============================================
# Health Chain ggplot Theme with Embedded Logo
# ============================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(cowplot)
  library(magick)
})

# --- Dynamic Logo Path (Shiny-safe) ---
# For Shiny apps, the working directory is where app.R/server.R is located
HC_SCRIPT_DIR <- getwd()

# Try multiple possible locations for the logo
logo_candidates <- c(
  file.path(HC_SCRIPT_DIR, "assets", "health-chain-full-logo.png"),  # Shiny app root/assets
  file.path(HC_SCRIPT_DIR, "www", "health-chain-full-logo.png"),      # Shiny www folder
  file.path(dirname(HC_SCRIPT_DIR), "assets", "health-chain-full-logo.png")  # Parent directory
)

# Find the first existing logo path
HC_LOGO_PATH <- NULL
for (path in logo_candidates) {
  if (file.exists(path)) {
    HC_LOGO_PATH <- path
    break
  }
}

# If no logo found, set a default that will trigger warnings
if (is.null(HC_LOGO_PATH)) {
  HC_LOGO_PATH <- file.path(HC_SCRIPT_DIR, "assets", "health-chain-full-logo.png")
  warning("⚠️ Health Chain logo not found. Checked locations:\n",
          paste("  -", logo_candidates, collapse = "\n"))
}

# --- Default Config for Logo Placement (BOTTOM-RIGHT) ---
HC_LOGO_POSITION <- "bottom-right"  # Changed from top-left
HC_LOGO_SCALE <- 0.08  # Slightly smaller
HC_LOGO_X_NUDGE <- -0.02  # Nudge left from right edge
HC_LOGO_Y_NUDGE <- 0.02   # Nudge up from bottom

# --- Define Health Chain Theme (light background palette) ---
theme_healthchain <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", color = "#f26d21", hjust = 0.5, size = base_size + 2),
      plot.subtitle = element_text(color = "#0c223f", hjust = 0.5, size = base_size - 2),
      axis.title = element_text(color = "#0c223f"),
      axis.text = element_text(color = "#0c223f"),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      legend.text = element_text(color = "#f26d21"),
      legend.title = element_text(color = "#0c223f", face = "bold"),
      strip.background = element_rect(fill = "#123358", color = NA),
      strip.text = element_text(color = "#ffffff", face = "bold", size = base_size - 1),
      panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 28, r = 18, b = 28, l = 28)  # Increased bottom margin for logo
    )
}

theme_set(theme_healthchain())

# --- Helper: Add logo using cowplot (BOTTOM-RIGHT PLACEMENT) ---
add_logo_cowplot <- function(plot,
                             logo_path = HC_LOGO_PATH,
                             position = HC_LOGO_POSITION,
                             logo_scale = HC_LOGO_SCALE,
                             x_nudge = HC_LOGO_X_NUDGE,
                             y_nudge = HC_LOGO_Y_NUDGE) {
  
  if (!file.exists(logo_path)) {
    warning(paste("⚠️ Health Chain logo not found at:", logo_path))
    return(plot)
  }
  
  logo <- magick::image_read(logo_path)
  
  # Position anchors - using plot-relative coordinates
  anchors <- list(
    "top-left" = list(x = 0.01, y = 0.99, hjust = 0, vjust = 1),
    "top-right" = list(x = 0.99, y = 0.99, hjust = 1, vjust = 1),
    "bottom-left" = list(x = 0.01, y = 0.01, hjust = 0, vjust = 0),
    "bottom-right" = list(x = 0.99, y = 0.01, hjust = 1, vjust = 0)
  )
  
  a <- anchors[[position]]
  
  # Use ggdraw with plot.margin to position logo in plot area
  cowplot::ggdraw(plot) +
    cowplot::draw_image(
      logo,
      x = a$x + x_nudge,
      y = a$y + y_nudge,
      hjust = a$hjust,
      vjust = a$vjust,
      width = logo_scale,
      halign = a$hjust,
      valign = a$vjust
    )
}

# --- Store the original ggsave before overriding (avoid recursion) ---
.original_ggsave <- ggplot2::ggsave

# --- Define wrapper that adds logo, then calls original ggsave ---
ggsave_healthchain <- function(filename, plot = last_plot(), ...) {
  branded_plot <- tryCatch(
    add_logo_cowplot(plot),
    error = function(e) {
      warning("⚠️ Failed to add logo: ", conditionMessage(e))
      plot
    }
  )
  .original_ggsave(filename, branded_plot, ..., bg = "white")
}

# --- Override global ggsave only after defining safe wrapper ---
assign("ggsave", ggsave_healthchain, envir = globalenv())

message("✅ Health Chain theme loaded successfully with auto-logo support.")
message(paste("   Logo Path:", HC_LOGO_PATH))
message(paste("   Logo exists:", file.exists(HC_LOGO_PATH)))
message(paste("   Logo position:", HC_LOGO_POSITION))
