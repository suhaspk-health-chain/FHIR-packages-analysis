# ==============================================================
# utils.R - Utility Functions - FIXED
# ==============================================================

# Create resources_tbl with flexible column mapping
create_resources_tbl <- function(xig_resources) {
  resources_tbl <- xig_resources
  
  resources_tbl$version       <- map_column(xig_resources, c("version", "fhir_version", "Version"), "R4")
  resources_tbl$realm         <- map_column(xig_resources, c("realm", "Realm"), "unknown")
  resources_tbl$package_text  <- map_column(xig_resources, c("package_text", "package", "package_id", "Package"), "unknown")
  resources_tbl$identity_text <- map_column(xig_resources, c("identity_text", "identity", "type", "Identity"), "unknown")
  resources_tbl$auth          <- map_column(xig_resources, c("auth", "author", "Author"), "unknown")
  resources_tbl$status        <- map_column(xig_resources, c("status", "Status"), "active")
  resources_tbl$title         <- map_column(xig_resources, c("name_title", "title", "Title", "name"), "")
  resources_tbl$wg            <- map_column(xig_resources, c("wg", "working_group", "WG"), "")
  resources_tbl$fmm           <- suppressWarnings(as.numeric(
    map_column(xig_resources, c("fmm", "FMM", "maturity"), NA_character_)))
  # Derived: extract resource type prefix from identity_text
  resources_tbl$resource_type <- stringr::str_extract(resources_tbl$identity_text, "^[^/]+")
  
  message(paste("✅ resources_tbl created:", nrow(resources_tbl), "rows"))
  return(resources_tbl)
}

# Map column helper — returns a vector the same length as df rows
map_column <- function(df, possible_names, default) {
  for (name in possible_names) {
    if (name %in% names(df)) return(df[[name]])
  }
  return(rep(default, nrow(df)))
}

# Load delta counts with chronological ordering
load_delta_counts <- function(delta_file) {
  if (file.exists(delta_file)) {
    dc <- read_csv(delta_file, col_types = cols(), show_col_types = FALSE)
    
    if ("added" %in% names(dc) && "removed" %in% names(dc)) {
      dc <- dc %>%
        rename(added_resources = added, removed_resources = removed) %>%
        mutate(
          added = sapply(added_resources, count_comma_separated),
          removed = sapply(removed_resources, count_comma_separated)
        )
    }
    
    version_order <- c("R2", "R3", "R4", "R4B", "R5", "R6")
    dc <- dc %>%
      mutate(
        from_version = str_trim(str_extract(transition, "^[^→]+")),
        to_version = str_trim(str_extract(transition, "[^→]+$"))
      ) %>%
      arrange(match(from_version, version_order), match(to_version, version_order)) %>%
      select(-from_version, -to_version)
    
    message(paste("✅ Delta counts loaded:", nrow(dc), "transitions"))
    return(dc)
  } else {
    message("⚠️ Delta counts not found")
    return(tibble(transition = character(), added_resources = character(), removed_resources = character(), added = numeric(), removed = numeric()))
  }
}

# Count comma-separated items
count_comma_separated <- function(x) {
  if (is.na(x) || x == "" || x == "NA") return(0)
  length(strsplit(as.character(x), ",\\s*")[[1]])
}

# Load file if exists
load_if_exists <- function(file_path, fallback = tibble()) {
  if (file.exists(file_path)) {
    read_csv(file_path, col_types = cols(), show_col_types = FALSE)
  } else {
    fallback
  }
}

# Load metadata
load_meta <- function(meta_file) {
  if (file.exists(meta_file)) {
    fromJSON(meta_file)
  } else {
    list(built_at = Sys.time(), source_dirs = list(processed = "data/processed", raw = "data/raw"))
  }
}

# Create realm summary
create_realm_summary <- function(realm_dist) {
  realm_dist %>%
    filter(realm != "") %>%
    arrange(desc(count)) %>%
    head(15) %>%
    mutate(
      realm_label = case_when(
        realm == "uv" ~ "International (UV)",
        realm == "us" ~ "United States",
        TRUE ~ str_to_upper(realm)
      ),
      pct_label = sprintf("%.1f%%", percentage)
    )
}

# Create version summary
create_version_summary <- function(fhir_version_dist) {
  fhir_version_dist %>%
    mutate(
      version_label = case_when(
        fhir_version == "R4" ~ "FHIR R4 (Current)",
        fhir_version == "R5" ~ "FHIR R5 (Next-Gen)",
        fhir_version == "R3" ~ "FHIR R3 (Legacy)",
        fhir_version == "R4B" ~ "FHIR R4B (Interim)",
        fhir_version == "R6" ~ "FHIR R6 (Experimental)",
        TRUE ~ fhir_version
      ),
      pct_label = sprintf("%.1f%%", percentage)
    )
}

# Color palette used by all plots
HC_PALETTE <- c(
  "#f26d21", "#0c223f", "#123358", "#33d17a", "#11253f",
  "#4a90d9", "#e8a838", "#6c5ce7", "#00b894", "#d63031"
)

scale_fill_healthchain <- function(...) {
  ggplot2::discrete_scale(
    "fill", "healthchain",
    palette = colorRampPalette(HC_PALETTE),
    ...
  )
}

scale_color_healthchain <- function(...) {
  ggplot2::discrete_scale(
    "colour", "healthchain",
    palette = colorRampPalette(HC_PALETTE),
    ...
  )
}

# Unified ggplot theme - SIMPLIFIED
hc_theme <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 15, color = "#f26d21", hjust = 0.5, margin = margin(b = 8)),
      plot.subtitle = element_text(size = 12, color = "#0c223f", hjust = 0.5, margin = margin(b = 8)),
      axis.title = element_text(size = 12, face = "bold", color = "#0c223f"),
      axis.text = element_text(size = 11, color = "#333333"),
      panel.grid.major = element_line(color = "#eeeeee", size = 0.2),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 20, r = 15, b = 60, l = 15)
    )
}

# Add a standardised caption to any ggplot before saving
add_caption_style <- function(p, caption_text) {
  p +
    labs(caption = caption_text) +
    theme(plot.caption = element_text(
      size = 8, color = "#666", hjust = 0, face = "italic",
      margin = margin(t = 10)
    ))
}

# Save plot — no logo
save_plot_with_logo <- function(plot, filename, width = 10, height = 7) {
  ggplot2::ggsave(filename, plot = plot, width = width, height = height,
                  dpi = 200, bg = "white")
}

# CONSOLIDATED: Create bar plot with consistent styling
create_bar_plot <- function(data, x_var, y_var, title, x_label, y_label, 
                            fill_var = NULL, facet_var = NULL, coord_flip = TRUE) {
  
  fill_var <- if(is.null(fill_var)) x_var else fill_var
  
  p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(fill_var))) +
    geom_col(show.legend = FALSE, color = "white", size = 0.3) +
    geom_text(aes(label = scales::comma(!!sym(y_var))), 
              hjust = if(coord_flip) -0.2 else 0.5, 
              vjust = if(coord_flip) 0.5 else -0.5,
              size = 4, color = "#0c223f", fontface = "bold") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    scale_fill_healthchain() +
    labs(title = title, x = x_label, y = y_label) +
    hc_theme()
  
  if (coord_flip) p <- p + coord_flip()
  if (!is.null(facet_var)) p <- p + facet_wrap(as.formula(paste("~", facet_var)), scales = "free_y", ncol = 2)
  
  return(p)
}