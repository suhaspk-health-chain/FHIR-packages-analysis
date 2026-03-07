# ============================================
# Health Chain ggplot Theme with Embedded Logo
# ============================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(cowplot)
  library(magick)
  library(showtext)
})

# # --- Resolve script dir ---
# get_script_dir <- function() {
#   if (!is.null(sys.frames()[[1]]$ofile)) {
#     return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
#   }
#   normalizePath(getwd())
# }
# 
# HC_SCRIPT_DIR <- get_script_dir()
# 
# # --- Base font dir ---
# FONT_DIR <- file.path(HC_SCRIPT_DIR, "assets", "Figtree")
# 
# # Helper to pick the first existing candidate
# pick_font <- function(candidates) {
#   for (p in candidates) {
#     if (file.exists(p)) return(p)
#   }
#   return(NA_character_)
# }
# 
# regular_candidates <- c(
#   file.path(FONT_DIR, "Figtree-Regular.ttf"),
#   file.path(FONT_DIR, "static", "Figtree-Regular.ttf"),
#   file.path(FONT_DIR, "Figtree-VariableFont_wght.ttf")
# )
# 
# bold_candidates <- c(
#   file.path(FONT_DIR, "Figtree-Bold.ttf"),
#   file.path(FONT_DIR, "static", "Figtree-Bold.ttf"),
#   file.path(FONT_DIR, "Figtree-VariableFont_wght.ttf")
# )
# 
# FONT_REGULAR_PATH <- pick_font(regular_candidates)
# FONT_BOLD_PATH    <- pick_font(bold_candidates)
# 
# cat("Script dir    :", HC_SCRIPT_DIR, "\n")
# cat("Font dir      :", FONT_DIR, "\n")
# cat("Regular path  :", FONT_REGULAR_PATH, "exists:", file.exists(FONT_REGULAR_PATH), "\n")
# cat("Bold path     :", FONT_BOLD_PATH, "exists:", file.exists(FONT_BOLD_PATH), "\n")
# 
# if (!is.na(FONT_REGULAR_PATH) && !is.na(FONT_BOLD_PATH)) {
#   font_add("Figtree",
#            regular = FONT_REGULAR_PATH,
#            bold    = FONT_BOLD_PATH)
#   showtext_auto()
# } else {
#   warning(
#     "Could not locate Figtree regular/bold font.\n",
#     "Searched candidates:\n  ",
#     paste(c(regular_candidates, bold_candidates), collapse = "\n  "),
#     "\nUsing default system fonts instead."
#   )
# }


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


# Make sure showtext knows your intended DPI
#showtext_opts(dpi = 150)   # 96 for screen, 150–300 for export

theme_healthchain <- function(base_size = 14) {
  theme_minimal(base_size = base_size, base_family = "poppins") +
    theme(
      plot.title        = element_text(face = "bold", color = "#f26d21", hjust = 0.5, size = base_size + 4),
      axis.title        = element_text(color = "#0c223f", family = "poppins", size = base_size + 2),
      axis.text         = element_text(color = "#0c223f", family = "poppins", size = base_size),
      legend.title      = element_text(color = "#0c223f", face = "bold", family = "poppins", size = base_size + 1),
      legend.text       = element_text(color = "#f26d21", family = "poppins", size = base_size),
      strip.background  = element_rect(fill = "#123358", color = NA),
      strip.text        = element_text(color = "#ffffff", face = "bold", size = base_size),
      plot.caption      = element_text(size = base_size * 0.9, color = "#0c223f", family = "poppins", hjust = 1),
      plot.subtitle     = element_text(size = base_size + 2, color = "#f26d21", family = "poppins", hjust = 0.5),
      panel.background  = element_rect(fill = "#ffffff", color = NA),
      plot.background   = element_rect(fill = "#ffffff", color = NA),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      panel.grid.major  = element_line(color = "#f26d21", linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      plot.margin       = margin(t = base_size * 2, r = base_size, b = base_size, l = base_size * 2)
    )
}
# Set as global default (example: change base_size depending on plotting context)
theme_set(theme_healthchain(base_size = 16))


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