# ==============================================================
# 08_plot_version_resource_comparison.R
# Compare official FHIR resource definitions across versions
# and generate plots + CSV summaries
# ==============================================================

suppressPackageStartupMessages({
  library(httr2)
  library(rvest)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(kableExtra)
  library(forcats)
})

# ---- Load config for paths ---------------------------------------------------
# If you run from project root, this will work:
source("scripts/01_config.R")

# ---- Optional dark theme loader ---------------------------------------------
apply_theme <- function(p, base_size = 14) {
  if (exists("theme_healthchain")) p + theme_healthchain(base_size) else
    p + theme_minimal(base_size = base_size)
}

# ---- FHIR resource list URLs -------------------------------------------------
fhir_versions <- tibble::tibble(
  version = c("R2", "R3", "R4", "R4B", "R5", "R6"),
  url = c(
    "https://hl7.org/fhir/DSTU2/resourcelist.html",
    "https://hl7.org/fhir/STU3/resourcelist.html",
    "https://hl7.org/fhir/R4/resourcelist.html",
    "https://hl7.org/fhir/R4B/resourcelist.html",
    "https://hl7.org/fhir/R5/resourcelist.html",
    "https://build.fhir.org/resourcelist.html"
  )
)

# ---- Fetchers ---------------------------------------------------------------
fetch_html <- function(u, timeout_sec = 30) {
  resp <- request(u) |>
    req_user_agent("HC-FHIR-EDA/1.0 (R; contact: you@example.com)") |>
    req_timeout(timeout_sec) |>
    req_perform()
  read_html(resp_body_string(resp))
}

NOT_RESOURCE_TEXT <- c(
  "FHIR","Index","Downloads","Profiles","Datatypes","DataTypes","Definitions",
  "Extensions","Base","Summary","Examples","Modules","Narrative","Security",
  "Version","Versions","Conformance","Search"
)

get_resources <- function(ver, url) {
  message("Fetching resources for ", ver)
  pg <- fetch_html(url)
  a  <- html_elements(pg, "a")
  tibble(
    text = html_text2(a),
    href = html_attr(a, "href")
  ) |>
    mutate(text = str_trim(text), href = str_trim(href)) |>
    filter(
      !is.na(text), !is.na(href),
      str_detect(text, "^[A-Z][A-Za-z]+$"),            # CamelCase resource names
      !text %in% NOT_RESOURCE_TEXT,
      str_detect(href, "^[A-Za-z0-9]+\\.html$")        # same-dir .html
    ) |>
    transmute(version = ver, resource = text) |>
    distinct() |>
    arrange(resource)
}

# ---- Build canonical lists & comparisons ------------------------------------
all_resources <- map2_dfr(fhir_versions$version, fhir_versions$url, get_resources)

summary_counts <- all_resources |>
  count(version, name = "n_resources") |>
  arrange(factor(version, levels = c("R2","R3","R4","R4B","R5","R6")))

res_matrix <- all_resources |>
  mutate(present = 1L) |>
  pivot_wider(names_from = version, values_from = present, values_fill = 0L) |>
  arrange(resource)

compare_versions <- function(v1, v2) {
  s1 <- filter(all_resources, version == v1) |> pull(resource)
  s2 <- filter(all_resources, version == v2) |> pull(resource)
  tibble(
    transition = paste(v1, "→", v2),
    added      = list(setdiff(s2, s1)),
    removed    = list(setdiff(s1, s2))
  )
}
version_pairs <- list(c("R2","R3"), c("R3","R4"), c("R4","R4B"), c("R4B","R5"), c("R5","R6"))
diffs <- map_dfr(version_pairs, ~compare_versions(.x[1], .x[2]))

# ---- Flatten and save CSVs → data/processed ---------------------------------
flatten_vec <- function(x) {
  if (length(x) == 0 || is.null(x)) return("")
  paste(sort(as.character(x)), collapse = ", ")
}
diffs_flat <- diffs %>%
  mutate(
    added   = vapply(added,   flatten_vec, character(1)),
    removed = vapply(removed, flatten_vec, character(1))
  )

write.csv(summary_counts,
          file.path(PROC_DIR, "summary_resource_counts_by_version.csv"),
          row.names = FALSE)
write.csv(res_matrix,
          file.path(PROC_DIR, "resource_presence_matrix.csv"),
          row.names = FALSE)
write.csv(diffs_flat,
          file.path(PROC_DIR, "resources_added_removed_by_transition.csv"),
          row.names = FALSE)

# ---- PLOT 1: count of resources per version ---------------------------------
p_counts <- ggplot(summary_counts,
                   aes(x = factor(version, levels = c("R2","R3","R4","R4B","R5","R6")),
                       y = n_resources, fill = version)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n_resources), vjust = -0.3, color = "white", size = 4) +
  labs(title = "FHIR Resources by Version (Official Spec)",
       x = "FHIR Version", y = "Number of Resources")
p_counts <- apply_theme(p_counts, base_size = 14)
ggsave(file.path(FIG_DIR, "ver_resource_counts.png"), p_counts, width = 8, height = 5, dpi = 150)

# ---- PLOT 2: presence heatmap (resource x version) ---------------------------
heat_long <- res_matrix |>
  pivot_longer(-resource, names_to = "version", values_to = "present") |>
  mutate(version = factor(version, levels = c("R2","R3","R4","R4B","R5","R6")))

# Order resources by stability across versions
order_by_presence <- heat_long |>
  group_by(resource) |>
  summarise(k = sum(present, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(k), resource) |>
  pull(resource)

heat_long$resource <- factor(heat_long$resource, levels = rev(order_by_presence))

p_heat <- ggplot(heat_long, aes(x = version, y = resource, fill = present)) +
  geom_tile() +
  scale_fill_gradient(limits = c(0,1), breaks = c(0,1),
                      labels = c("Absent","Present"),
                      low = "#11253f", high = "#33d17a", name = NULL) +
  labs(title = "FHIR Resource Presence Across Versions",
       x = "FHIR Version", y = "Resource")
p_heat <- apply_theme(p_heat, base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = 7))

ggsave(file.path(FIG_DIR, "ver_resource_presence_heatmap.png"),
       p_heat, width = 9, height = 22, dpi = 150)

# ---- PLOT 3: stable (≥4-version) resources heatmap ---------------------------
stable <- heat_long |>
  group_by(resource) |> summarise(k = sum(present), .groups = "drop") |> filter(k >= 4)
heat_stable <- semi_join(heat_long, stable, by = "resource")

p_heat_stable <- ggplot(heat_stable, aes(x = version, y = resource, fill = present)) +
  geom_tile() +
  scale_fill_gradient(limits = c(0,1), breaks = c(0,1),
                      labels = c("Absent","Present"),
                      low = "#11253f", high = "#33d17a", name = NULL) +
  labs(title = "Stable FHIR Resources (Present in ≥ 4 Versions)",
       x = "FHIR Version", y = "Resource")
p_heat_stable <- apply_theme(p_heat_stable, base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = 8))

ggsave(file.path(FIG_DIR, "ver_resource_presence_heatmap_stable.png"),
       p_heat_stable, width = 9, height = 12, dpi = 150)

# ---- PLOT 4: added/removed bar charts ----------------------------------------
delta_counts <- diffs %>%
  transmute(
    transition = factor(transition, levels = paste(c("R2","R3","R4","R4B","R5"),
                                                   "→", c("R3","R4","R4B","R5","R6"))),
    added   = lengths(map(diffs$added, identity)),
    removed = lengths(map(diffs$removed, identity))
  )

p_added <- ggplot(delta_counts, aes(x = transition, y = added, fill = transition)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = added), vjust = -0.3, color = "white", size = 4) +
  labs(title = "Resources Added at Each Version Transition",
       x = "Transition (previous → current)", y = "Count Added")
p_added <- apply_theme(p_added, base_size = 14)
ggsave(file.path(FIG_DIR, "ver_resources_added.png"), p_added, width = 9, height = 5, dpi = 150)

p_removed <- ggplot(delta_counts, aes(x = transition, y = removed, fill = transition)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = removed), vjust = -0.3, color = "white", size = 4) +
  labs(title = "Resources Removed at Each Version Transition",
       x = "Transition (previous → current)", y = "Count Removed")
p_removed <- apply_theme(p_removed, base_size = 14)
ggsave(file.path(FIG_DIR, "ver_resources_removed.png"), p_removed, width = 9, height = 5, dpi = 150)

# ---- Completion message ------------------------------------------------------
cat("\n✅ FHIR version comparison completed.\n",
    "CSVs saved to:   ", normalizePath(PROC_DIR), "\n",
    "PNGs saved to:   ", normalizePath(FIG_DIR), "\n")
