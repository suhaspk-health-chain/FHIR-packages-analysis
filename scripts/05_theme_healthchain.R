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

# --- Default Config for Logo Placement (TOP-LEFT with comfy padding) ---
HC_LOGO_POSITION  <- "top-left"  # "top-left", "top-right", "bottom-left", "bottom-right"
HC_LOGO_SCALE     <- 0.15        # relative width of full canvas (kept modest for consistency)
HC_LOGO_X_OFFSET  <- 0.05        # extra left padding (canvas units)
HC_LOGO_Y_OFFSET  <- 0.05        # extra top padding  (canvas units)

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
      strip.text        = element_text(color = "#0c223f", face = "bold"),
      panel.grid.major  = element_line(color = "#1d3557", linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      # Extra top/left margin so the logo never feels cramped
      plot.margin       = margin(t = 28, r = 18, b = 16, l = 28)
    )
}
theme_set(theme_healthchain())

# --- Helper: Add logo to any ggplot ---
add_logo <- function(plot,
                     logo_path = HC_LOGO_PATH,
                     position  = HC_LOGO_POSITION,
                     logo_scale = HC_LOGO_SCALE,
                     x_offset   = HC_LOGO_X_OFFSET,
                     y_offset   = HC_LOGO_Y_OFFSET) {
  if (!file.exists(logo_path)) {
    warning(paste("⚠️ Health Chain logo not found at:", logo_path))
    return(plot)
  }
  
  logo <- magick::image_read(logo_path)
  pos <- match.arg(position, c("top-right", "top-left", "bottom-right", "bottom-left"))
  anchors <- list(
    "top-right"    = list(x = 1 - x_offset, y = 1 - y_offset, hjust = 1, vjust = 1),
    "top-left"     = list(x = 0 + x_offset, y = 1 - y_offset, hjust = 0, vjust = 1),
    "bottom-right" = list(x = 1 - x_offset, y = 0 + y_offset, hjust = 1, vjust = 0),
    "bottom-left"  = list(x = 0 + x_offset, y = 0 + y_offset, hjust = 0, vjust = 0)
  )
  a <- anchors[[pos]]
  
  ggdraw(plot) +
    draw_image(
      logo,
      x = a$x, y = a$y,
      hjust = a$hjust, vjust = a$vjust,
      width = logo_scale
    )
}

# --- Store the original ggsave before overriding (avoid recursion) ---
.original_ggsave <- ggplot2::ggsave

# --- Define wrapper that adds logo, then calls original ggsave ---
ggsave_healthchain <- function(filename, plot = last_plot(), ...) {
  branded_plot <- tryCatch(
    add_logo(plot),
    error = function(e) {
      warning("⚠️ Failed to add logo: ", conditionMessage(e))
      plot
    }
  )
  .original_ggsave(filename, branded_plot, ..., bg = "transparent")
}

# --- Override global ggsave only after defining safe wrapper ---
assign("ggsave", ggsave_healthchain, envir = globalenv())

message("✅ Health Chain theme loaded successfully with auto-logo support.")
message(paste("   Logo Path:", HC_LOGO_PATH))
