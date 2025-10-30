# ============================================
# Health Chain ggplot Theme with Embedded Logo
# ============================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(cowplot)
  library(magick)
})

# --- Dynamic Logo Path (repo-safe) ---
HC_SCRIPT_DIR <- tryCatch({
  this_file <- sys.frames()[[1]]$ofile
  if (is.null(this_file)) getwd() else dirname(this_file)
}, error = function(e) getwd())

HC_LOGO_PATH <- file.path(HC_SCRIPT_DIR, "assets", "health-chain-full-logo.png")

# --- Default Config for Logo Placement ---
# --- Default Config for Logo Placement ---
HC_LOGO_POSITION  <- "top-left"
HC_LOGO_SCALE     <- 0.096       # ✅ Reduced from 0.12 to 0.096 (20% smaller)
HC_LOGO_X_NUDGE   <- 0.02
HC_LOGO_Y_NUDGE   <- 0.02


# --- Define Health Chain Theme (light background palette) ---
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
      panel.grid.major  = element_line(color = "#1d3557", linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      plot.margin       = margin(t = 28, r = 18, b = 16, l = 28)
    )
}
theme_set(theme_healthchain())

# --- Helper: Add logo INSIDE plot panel area ---
add_logo <- function(plot,
                     logo_path = HC_LOGO_PATH,
                     position  = HC_LOGO_POSITION,
                     logo_scale = HC_LOGO_SCALE,
                     x_nudge    = HC_LOGO_X_NUDGE,
                     y_nudge    = HC_LOGO_Y_NUDGE) {
  if (!file.exists(logo_path)) {
    warning(paste("⚠️ Health Chain logo not found at:", logo_path))
    return(plot)
  }
  
  # Add annotation layer with logo
  plot +
    annotation_custom(
      grob = grid::rasterGrob(
        magick::image_read(logo_path),
        interpolate = TRUE
      ),
      xmin = -Inf, xmax = Inf,  # Will be adjusted based on position
      ymin = -Inf, ymax = Inf   # Will be adjusted based on position
    ) +
    coord_cartesian(clip = "off")  # Allow drawing outside strict plot bounds
}

# --- Alternative: Use cowplot with plot-relative coordinates ---
add_logo_cowplot <- function(plot,
                             logo_path = HC_LOGO_PATH,
                             position  = HC_LOGO_POSITION,
                             logo_scale = HC_LOGO_SCALE,
                             x_nudge    = HC_LOGO_X_NUDGE,
                             y_nudge    = HC_LOGO_Y_NUDGE) {
  if (!file.exists(logo_path)) {
    warning(paste("⚠️ Health Chain logo not found at:", logo_path))
    return(plot)
  }
  
  logo <- magick::image_read(logo_path)
  
  # Position anchors - using plot-relative coordinates
  anchors <- list(
    "top-left"     = list(x = 0.01, y = 0.99, hjust = 0, vjust = 1),
    "top-right"    = list(x = 0.99, y = 0.99, hjust = 1, vjust = 1),
    "bottom-left"  = list(x = 0.01, y = 0.01, hjust = 0, vjust = 0),
    "bottom-right" = list(x = 0.99, y = 0.01, hjust = 1, vjust = 0)
  )
  
  a <- anchors[[position]]
  
  # Use ggdraw with plot.margin to position logo in plot area
  cowplot::ggdraw(plot) +
    cowplot::draw_image(
      logo,
      x = a$x + x_nudge, 
      y = a$y - y_nudge,
      hjust = a$hjust, 
      vjust = a$vjust,
      width = logo_scale,
      halign = 0,  # Left align
      valign = 1   # Top align
    )
}

# --- Store the original ggsave before overriding (avoid recursion) ---
.original_ggsave <- ggplot2::ggsave

# --- Define wrapper that adds logo, then calls original ggsave ---
ggsave_healthchain <- function(filename, plot = last_plot(), ...) {
  branded_plot <- tryCatch(
    add_logo_cowplot(plot),  # Use the cowplot version
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
