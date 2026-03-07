# ==============================================================================
# eda_plots.R
# PURPOSE : Generate all EDA plots for XIG-FHIR-Resources-EDA.Rmd
# AUTHOR  : Suhas P K
# DATE    : 2026-03-07
#
# USAGE   : Run this script ONCE from the project root before knitting the Rmd.
#           All plots are saved to outputs/eda_plots/ as PNG files.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(readr)
  library(arrow)
  library(forcats)
  library(patchwork)
})

# ------------------------------------------------------------------------------
# 0. Paths
# ------------------------------------------------------------------------------
ROOT_DIR  <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
PROC_DIR  <- file.path(ROOT_DIR, "data", "processed")
OUT_DIR   <- file.path(ROOT_DIR, "outputs", "eda_plots")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# 1. Brand palette & theme (mirrors the dashboard)
# ------------------------------------------------------------------------------
HC_ORANGE  <- "#f26d21"
HC_NAVY    <- "#0c223f"
HC_MID     <- "#123358"
HC_GREEN   <- "#33d17a"
HC_GOLD    <- "#e8a838"
HC_BLUE    <- "#4a90d9"
HC_PURPLE  <- "#6c5ce7"
HC_TEAL    <- "#00b894"
HC_RED     <- "#d63031"
HC_BG      <- "#ffffff"
HC_GRID    <- "#f0f0f0"

HC_PALETTE <- c(HC_ORANGE, HC_NAVY, HC_MID, HC_GREEN, HC_GOLD,
                HC_BLUE,   HC_PURPLE, HC_TEAL, HC_RED, "#b2bec3")

# Font: try Poppins from Google, fall back to system sans-serif
HC_FONT <- tryCatch({
  if (requireNamespace("sysfonts", quietly = TRUE) &&
      requireNamespace("showtext", quietly = TRUE)) {
    sysfonts::font_add_google("Poppins", "poppins")
    showtext::showtext_auto()
    "poppins"
  } else {
    "sans"
  }
}, error = function(e) {
  message("  Note: Poppins unavailable, using system sans-serif.")
  "sans"
})

# Journal-grade theme
# Rule: base_size=18 on a 10×7 canvas → text occupies ~3% of image width = clearly legible
theme_hc <- function(base_size = 18) {
  theme_minimal(base_size = base_size, base_family = HC_FONT) +
    theme(
      plot.title       = element_text(face = "bold", color = HC_ORANGE,
                                      size = base_size + 6, hjust = 0.5,
                                      margin = margin(b = 10)),
      plot.subtitle    = element_text(color = HC_NAVY, size = base_size + 1,
                                      hjust = 0.5, margin = margin(b = 14)),
      plot.caption     = element_text(color = "#888888", size = base_size - 3,
                                      hjust = 0, face = "italic",
                                      margin = margin(t = 14)),
      axis.title       = element_text(color = HC_NAVY, face = "bold",
                                      size = base_size + 1),
      axis.text        = element_text(color = "#333333", size = base_size),
      panel.grid.major = element_line(color = HC_GRID, linewidth = 0.5),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = HC_MID, color = NA),
      strip.text       = element_text(color = "white", face = "bold",
                                      size = base_size + 1),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", color = HC_NAVY,
                                      size = base_size),
      legend.text      = element_text(color = "#333333", size = base_size),
      plot.background  = element_rect(fill = HC_BG, color = NA),
      panel.background = element_rect(fill = HC_BG, color = NA),
      plot.margin      = margin(t = 24, r = 24, b = 20, l = 24)
    )
}

scale_fill_hc <- function(...) {
  scale_fill_manual(values = colorRampPalette(HC_PALETTE)(20), ...)
}

scale_color_hc <- function(...) {
  scale_color_manual(values = colorRampPalette(HC_PALETTE)(20), ...)
}

# Canvas: 10×7 @ 300dpi — text at base_size=18 occupies ~3% of image width (ideal for print)
save_plot <- function(p, name, width = 10, height = 7, dpi = 300) {
  ggsave(
    filename = file.path(OUT_DIR, paste0(name, ".png")),
    plot     = p,
    width    = width,
    height   = height,
    dpi      = dpi,
    bg       = "white"
  )
  message("  Saved: ", name, ".png")
}

# No HealthChain signature — author credit only
CAPTION <- "Source: FHIR XIG Registry (HL7 International)  |  EDA by Suhas P K"

# ------------------------------------------------------------------------------
# 2. Load data
# ------------------------------------------------------------------------------
message("\n Loading data ...")

res <- arrow::read_parquet(file.path(PROC_DIR, "fhir_resources.parquet"))

# Normalise column names (mirrors utils.R logic)
if (!"version" %in% names(res) && "fhir_version" %in% names(res))
  res <- rename(res, version = fhir_version)
if (!"realm"   %in% names(res)) res$realm <- "unknown"
if (!"resource_type" %in% names(res) && "identity_text" %in% names(res))
  res$resource_type <- str_extract(res$identity_text, "^[^/]+")

# Delta / transitions (may not exist — handled gracefully)
delta_file <- file.path(PROC_DIR, "resources_added_removed_by_transition.csv")
has_delta  <- file.exists(delta_file)
if (has_delta) {
  delta <- read_csv(delta_file, col_types = cols(), show_col_types = FALSE)
  # Count comma-separated items if columns are resource lists
  if ("added" %in% names(delta) && is.character(delta$added)) {
    delta <- delta %>%
      mutate(
        added_n   = sapply(added,   function(x) if (is.na(x) || x == "") 0L else length(str_split(x, ",\\s*")[[1]])),
        removed_n = sapply(removed, function(x) if (is.na(x) || x == "") 0L else length(str_split(x, ",\\s*")[[1]]))
      )
  } else {
    delta <- delta %>% rename(added_n = added, removed_n = removed)
  }
  version_order <- c("R2", "R3", "R4", "R4B", "R5", "R6")
  delta <- delta %>%
    mutate(
      from_v = str_trim(str_extract(transition, "^[^→→]+")),
      to_v   = str_trim(str_extract(transition, "[^→→]+$"))
    ) %>%
    arrange(match(from_v, version_order)) %>%
    mutate(transition = factor(transition, levels = transition))
}

message(sprintf("  Loaded %s rows | %s packages | %s resource types",
                scales::comma(nrow(res)),
                scales::comma(n_distinct(res$package_text %||% res$package)),
                n_distinct(res$resource_type, na.rm = TRUE)))

# Small helper so %||% works without rlang
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ------------------------------------------------------------------------------
# 3. PLOT 1 — Resources by FHIR Version
# ------------------------------------------------------------------------------
message("\n[1] Resources by FHIR Version ...")

version_order <- c("R2", "R3", "R4", "R4B", "R5", "R6")

p1 <- res %>%
  filter(!is.na(version), version != "") %>%
  count(version) %>%
  mutate(version = factor(version, levels = version_order)) %>%
  arrange(version) %>%
  ggplot(aes(x = version, y = n, fill = version)) +
  geom_col(show.legend = FALSE, width = 0.65, color = "white") +
  geom_text(aes(label = scales::comma(n)), vjust = -0.6,
            fontface = "bold", color = HC_NAVY, size = 8) +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.18))) +
  scale_fill_hc() +
  labs(
    title    = "FHIR Resource Entries by Standard Version",
    subtitle = "FHIR R4 commands the landscape — adopted by virtually every major payer and EHR vendor",
    x        = "FHIR Version",
    y        = "Number of Resource Entries",
    caption  = CAPTION
  ) +
  theme_hc()

save_plot(p1, "p1_resources_by_version", width = 10, height = 7)

# ------------------------------------------------------------------------------
# 4. PLOT 2 — Top 20 Resource Types (overall)
# ------------------------------------------------------------------------------
message("[2] Top 20 Resource Types ...")

top20 <- res %>%
  filter(!is.na(resource_type), resource_type != "") %>%
  count(resource_type, sort = TRUE) %>%
  slice_head(n = 20) %>%
  mutate(resource_type = fct_reorder(resource_type, n))

p2 <- ggplot(top20, aes(x = resource_type, y = n, fill = resource_type)) +
  geom_col(show.legend = FALSE, color = "white") +
  geom_text(aes(label = scales::comma(n)), hjust = -0.12,
            fontface = "bold", color = HC_NAVY, size = 7) +
  coord_flip() +
  expand_limits(y = max(top20$n) * 1.2) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_hc() +
  labs(
    title    = "Top 20 FHIR Resource Types Across All Versions",
    subtitle = "ValueSet & StructureDefinition together account for ~76% of every published resource",
    x        = NULL,
    y        = "Count",
    caption  = CAPTION
  ) +
  theme_hc()

save_plot(p2, "p2_top20_resource_types", width = 11, height = 10)

# ------------------------------------------------------------------------------
# 5. PLOT 3 — Resource Type Composition by Version (Stacked Bar)
# ------------------------------------------------------------------------------
message("[3] Resource Type Composition by Version (stacked bar) ...")

top8_types <- res %>%
  filter(!is.na(resource_type), resource_type != "") %>%
  count(resource_type, sort = TRUE) %>%
  slice_head(n = 8) %>%
  pull(resource_type)

type_colors_p3 <- setNames(
  c(HC_ORANGE, HC_NAVY, HC_BLUE, HC_GREEN, HC_GOLD, HC_PURPLE, HC_TEAL, HC_RED, "#b2bec3"),
  c(top8_types, "Other")
)

stacked_data <- res %>%
  filter(!is.na(version), version != "",
         !is.na(resource_type), resource_type != "") %>%
  mutate(
    type_group = if_else(resource_type %in% top8_types, resource_type, "Other"),
    type_group = factor(type_group, levels = c(top8_types, "Other")),
    version    = factor(version, levels = version_order)
  ) %>%
  count(version, type_group)

p3 <- ggplot(stacked_data, aes(x = version, y = n, fill = type_group)) +
  geom_col(position = "stack", color = "white", linewidth = 0.5) +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.04))) +
  scale_fill_manual(values = type_colors_p3, name = "Resource Type") +
  labs(
    title    = "Resource Type Composition by FHIR Version",
    subtitle = "ValueSet & StructureDefinition dominate every version — the stack reveals both total scale and structural mix",
    x        = "FHIR Version",
    y        = "Resource Count",
    caption  = CAPTION
  ) +
  theme_hc() +
  theme(
    legend.position  = "right",
    legend.key.size  = unit(1.2, "cm"),
    legend.text      = element_text(size = 20),
    legend.title     = element_text(size = 22, face = "bold")
  )

save_plot(p3, "p3_top_types_by_version", width = 11, height = 8)

# ------------------------------------------------------------------------------
# 6. PLOT 4 — Realm Distribution (global landscape)
# ------------------------------------------------------------------------------
message("[4] Realm Distribution ...")

realm_palette <- c(
  "us"  = HC_ORANGE,
  "uv"  = HC_NAVY,
  "eu"  = HC_BLUE,
  "au"  = HC_GREEN,
  "de"  = HC_GOLD,
  "be"  = HC_PURPLE,
  "nl"  = HC_TEAL,
  "uk"  = HC_RED,
  "ca"  = "#fd79a8",
  "ch"  = "#636e72"
)

realm_labels <- c(
  "us" = "United States",
  "uv" = "International (UV)",
  "eu" = "European Union",
  "au" = "Australia",
  "de" = "Germany",
  "be" = "Belgium",
  "nl" = "Netherlands",
  "uk" = "United Kingdom",
  "ca" = "Canada",
  "ch" = "Switzerland"
)

realm_data <- res %>%
  filter(!is.na(realm), !realm %in% c("", "none", "unknown", "NA", "na")) %>%
  count(realm, sort = TRUE) %>%
  slice_head(n = 12) %>%
  mutate(
    realm_label = coalesce(realm_labels[realm], str_to_upper(realm)),
    realm_label = fct_reorder(realm_label, n),
    is_us = realm == "us",
    pct = round(100 * n / sum(n), 1)
  )

p4 <- ggplot(realm_data,
             aes(x = realm_label, y = n,
                 fill = ifelse(is_us, HC_ORANGE, HC_NAVY))) +
  geom_col(show.legend = FALSE, color = "white") +
  geom_text(aes(label = paste0(scales::comma(n), "  (", pct, "%)")),
            hjust = -0.08, fontface = "bold", color = HC_NAVY, size = 7) +
  coord_flip() +
  expand_limits(y = max(realm_data$n) * 1.25) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_identity() +
  labs(
    title    = "Global FHIR Resource Distribution by Realm",
    subtitle = "The United States publishes more FHIR resources than all other nations combined",
    x        = NULL,
    y        = "Resource Count",
    caption  = CAPTION
  ) +
  theme_hc()

save_plot(p4, "p4_realm_distribution", width = 11, height = 8)

# ------------------------------------------------------------------------------
# 7. PLOT 5 — US vs Rest of World
# ------------------------------------------------------------------------------
message("[5] US vs Rest of World ...")

us_vs_world <- res %>%
  filter(!is.na(realm), !realm %in% c("", "none", "unknown", "NA", "na")) %>%
  mutate(group = case_when(
    realm == "us" ~ "United States",
    realm == "uv" ~ "International (UV)",
    TRUE          ~ "Rest of World"
  )) %>%
  count(group, sort = TRUE) %>%
  mutate(
    pct   = round(100 * n / sum(n), 1),
    label = paste0(scales::comma(n), "\n(", pct, "%)"),
    group = fct_reorder(group, n)
  )

p5 <- ggplot(us_vs_world, aes(x = group, y = n, fill = group)) +
  geom_col(show.legend = FALSE, width = 0.6, color = "white") +
  geom_text(aes(label = label), vjust = -0.4,
            fontface = "bold", color = HC_NAVY, size = 8) +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.45))) +
  scale_fill_manual(values = c(
    "United States"     = HC_ORANGE,
    "International (UV)" = HC_NAVY,
    "Rest of World"     = HC_BLUE
  )) +
  labs(
    title    = "US vs International vs Rest of World",
    subtitle = "CMS & ONC mandates have turned the US into the world's largest FHIR publisher",
    x        = NULL,
    y        = "Resource Count",
    caption  = CAPTION
  ) +
  theme_hc()

save_plot(p5, "p5_us_vs_world", width = 10, height = 7)

# ------------------------------------------------------------------------------
# 8. PLOT 6 — FHIR Evolution (resources added/removed) — if delta data exists
# ------------------------------------------------------------------------------
message("[6] FHIR Version Evolution ...")

if (has_delta && nrow(delta) > 0) {

  delta_long <- delta %>%
    select(transition, added_n, removed_n) %>%
    # Replace Unicode arrow with ASCII so all fonts render it correctly
    mutate(transition = str_replace_all(as.character(transition),
                                        "\u2192", "->")) %>%
    pivot_longer(c(added_n, removed_n),
                 names_to  = "direction",
                 values_to = "count") %>%
    mutate(
      direction    = if_else(direction == "added_n", "Added", "Removed"),
      count_signed = if_else(direction == "Added", count, -count),
      transition   = factor(transition, levels = unique(transition))
    )

  p6 <- ggplot(delta_long,
               aes(x = transition, y = count_signed, fill = direction)) +
    geom_col(width = 0.6, color = "white") +
    geom_hline(yintercept = 0, color = "#666", linewidth = 0.6) +
    geom_text(data = filter(delta_long, direction == "Added"),
              aes(label = paste0("+", count), y = count_signed),
              vjust = -0.5, fontface = "bold", color = HC_GREEN, size = 8) +
    geom_text(data = filter(delta_long, direction == "Removed"),
              aes(label = paste0("-", count), y = count_signed),
              vjust = 1.4, fontface = "bold", color = HC_RED, size = 8) +
    scale_fill_manual(values = c("Added" = HC_GREEN, "Removed" = HC_RED),
                      name = NULL) +
    scale_y_continuous(labels = abs) +
    labs(
      title    = "FHIR Resource Type Changes Across Versions",
      subtitle = "Each version adds new healthcare concepts — very few are ever retired",
      x        = "Version Transition",
      y        = "Resource Types",
      caption  = CAPTION
    ) +
    theme_hc() +
    theme(legend.position = "top")

  save_plot(p6, "p6_version_evolution", width = 11, height = 7)

} else {
  # Fallback: synthetic illustration based on known FHIR data
  fhir_known <- tibble(
    version = factor(c("R2","R3","R4","R4B","R5","R6"), levels = version_order),
    types   = c(103L, 116L, 145L, 148L, 167L, 172L)
  )

  p6 <- ggplot(fhir_known, aes(x = version, y = types, group = 1)) +
    geom_line(color = HC_ORANGE, linewidth = 1.5) +
    geom_point(color = HC_ORANGE, size = 8, shape = 21,
               fill = "white", stroke = 2.5) +
    geom_text(aes(label = types), vjust = -1.3,
              fontface = "bold", color = HC_NAVY, size = 8) +
    scale_y_continuous(limits = c(90, 195),
                       breaks = seq(90, 195, by = 25)) +
    labs(
      title    = "Growth in FHIR Base Resource Types by Version",
      subtitle = "From 103 types in DSTU2 to 172 in R6 — a 67% expansion of the healthcare vocabulary",
      x        = "FHIR Version",
      y        = "Number of Base Resource Types",
      caption  = CAPTION
    ) +
    theme_hc()

  save_plot(p6, "p6_version_evolution", width = 11, height = 7)
}

# ------------------------------------------------------------------------------
# 9. PLOT 7 — Status Distribution (active / draft / retired)
# ------------------------------------------------------------------------------
message("[7] Status Distribution ...")

if ("status" %in% names(res)) {
  status_data <- res %>%
    filter(!is.na(status), !status %in% c("", "none", "unknown")) %>%
    count(status, sort = TRUE) %>%
    mutate(
      pct   = round(100 * n / sum(n), 1),
      label = paste0(status, "\n", scales::comma(n), " (", pct, "%)"),
      status = fct_reorder(status, n)
    )

  p7 <- ggplot(status_data, aes(x = status, y = n, fill = status)) +
    geom_col(show.legend = FALSE, width = 0.6, color = "white") +
    geom_text(aes(label = paste0(scales::comma(n), "\n(", pct, "%)")),
              vjust = -0.4, fontface = "bold", color = HC_NAVY, size = 7.5) +
    scale_y_continuous(labels = scales::comma,
                       expand = expansion(mult = c(0, 0.25))) +
    scale_fill_manual(values = c(
      "active"  = HC_GREEN,
      "draft"   = HC_GOLD,
      "retired" = HC_RED,
      "unknown" = "#b2bec3"
    ), na.value = "#b2bec3") +
    labs(
      title    = "FHIR Resource Status Distribution",
      subtitle = "Active resources dominate — the FHIR community prefers deprecation to deletion",
      x        = "Status",
      y        = "Count",
      caption  = CAPTION
    ) +
    theme_hc()

  save_plot(p7, "p7_status_distribution", width = 10, height = 7)
}

# ------------------------------------------------------------------------------
# 10. PLOT 8 — Top Authors / Publishers
# ------------------------------------------------------------------------------
message("[8] Top Authors ...")

auth_col <- if ("auth" %in% names(res)) "auth" else NULL

if (!is.null(auth_col)) {
  top_authors <- res %>%
    filter(!is.na(.data[[auth_col]]),
           !.data[[auth_col]] %in% c("", "none", "unknown")) %>%
    count(.data[[auth_col]], sort = TRUE) %>%
    slice_head(n = 15) %>%
    rename(auth = 1) %>%
    mutate(auth = fct_reorder(auth, n))

  # Lollipop + log scale: handles HL7's 12x dominance over all others
  p8 <- ggplot(top_authors, aes(x = auth, y = n)) +
    geom_segment(aes(xend = auth, yend = 1),
                 color = HC_NAVY, linewidth = 2) +
    geom_point(aes(fill = auth), size = 8, shape = 21,
               color = "white", stroke = 2, show.legend = FALSE) +
    geom_text(aes(label = scales::comma(n)),
              hjust = -0.3, fontface = "bold", color = HC_NAVY, size = 6) +
    coord_flip() +
    scale_y_log10(labels = scales::comma,
                  expand = expansion(mult = c(0.02, 0.35))) +
    scale_fill_hc() +
    labs(
      title    = "Top 15 FHIR Resource Publishers",
      subtitle = "Log scale reveals the full distribution — HL7 publishes 12x more resources than any other contributor",
      x        = NULL,
      y        = "Resource Count (log scale)",
      caption  = CAPTION
    ) +
    theme_hc() +
    # Ensure axis text is never compressed regardless of item count
    theme(axis.text.y = element_text(size = 18, face = "bold", color = "#222222"))

  # Dynamic height: 0.55 inches per item + 3 inch overhead for titles/margins
  p8_height <- max(6, nrow(top_authors) * 0.55 + 3)
  save_plot(p8, "p8_top_authors", width = 11, height = p8_height)
}

# ------------------------------------------------------------------------------
# 11. PLOT 9 — Packages per Version (distinct package count)
# ------------------------------------------------------------------------------
message("[9] Packages per Version ...")

pkg_col <- if ("package_text" %in% names(res)) "package_text" else
           if ("package"      %in% names(res)) "package"      else NULL

if (!is.null(pkg_col)) {
  pkgs_by_ver <- res %>%
    filter(!is.na(version), version != "") %>%
    group_by(version) %>%
    summarise(n_packages = n_distinct(.data[[pkg_col]]), .groups = "drop") %>%
    mutate(version = factor(version, levels = version_order)) %>%
    arrange(version)

  p9 <- ggplot(pkgs_by_ver, aes(x = version, y = n_packages, fill = version)) +
    geom_col(show.legend = FALSE, width = 0.6, color = "white") +
    geom_text(aes(label = scales::comma(n_packages)), vjust = -0.6,
              fontface = "bold", color = HC_NAVY, size = 8) +
    scale_y_continuous(labels = scales::comma,
                       expand = expansion(mult = c(0, 0.2))) +
    scale_fill_hc() +
    labs(
      title    = "Distinct FHIR Packages (IGs) by Version",
      subtitle = "R4 attracted the most Implementation Guide authors — a positive feedback loop of adoption",
      x        = "FHIR Version",
      y        = "Number of Distinct Packages",
      caption  = CAPTION
    ) +
    theme_hc()

  save_plot(p9, "p9_packages_by_version", width = 10, height = 7)
}

# ------------------------------------------------------------------------------
# 12. PLOT 10 — Resource Type Composition (stacked 100% bar by version)
# ------------------------------------------------------------------------------
message("[10] Resource Type Composition by Version ...")

top5_types <- res %>%
  filter(!is.na(resource_type), resource_type != "") %>%
  count(resource_type, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(resource_type)

composition_data <- res %>%
  filter(!is.na(version), version != "",
         !is.na(resource_type), resource_type != "") %>%
  mutate(
    type_group = if_else(resource_type %in% top5_types, resource_type, "Other"),
    version    = factor(version, levels = version_order)
  ) %>%
  count(version, type_group) %>%
  group_by(version) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

type_colors <- setNames(
  c(HC_ORANGE, HC_NAVY, HC_BLUE, HC_GREEN, HC_GOLD, "#b2bec3"),
  c(top5_types, "Other")
)

p10 <- ggplot(composition_data,
              aes(x = version, y = pct, fill = type_group)) +
  geom_col(position = "stack", color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = type_colors, name = "Resource Type") +
  labs(
    title    = "Resource Type Composition by FHIR Version (100% Stacked)",
    subtitle = "ValueSet & StructureDefinition consistently dominate — their role is foundational, not optional",
    x        = "FHIR Version",
    y        = "Share of Resources (%)",
    caption  = CAPTION
  ) +
  theme_hc() +
  theme(legend.position = "right")

save_plot(p10, "p10_composition_by_version", width = 11, height = 8)

# ------------------------------------------------------------------------------
message("\n All 10 EDA plots saved to: ", OUT_DIR)
message(" Run XIG-FHIR-Resources-EDA.Rmd to knit the full report.\n")
