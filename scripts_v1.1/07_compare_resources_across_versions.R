# ==========================================================
# SCRIPT: 08_compare_fhir_versions_and_visualize.R
# PURPOSE: Compare official FHIR resource definitions across versions, export CSVs, and visualize key results.
# AUTHOR: Suhas. P. K
# LAST MODIFIED: 2025-11-17
# REQUIREMENTS: dplyr, tidyr, purrr, ggplot2, scales, kableExtra, forcats
# ==========================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(kableExtra)
  library(forcats)
})

# ---- Load project config (paths, etc.) ----
source("scripts/01_config.R")
if (exists("theme_healthchain")) theme_set(theme_healthchain())

# ---- Data Dependency: expects all_resources from scraper script ----
# If running standalone, load or define all_resources here!
# load("data/processed/all_resources.RData")

# ---- Summarize resource counts per version ----
summary_counts <- all_resources |> 
  count(version, name = "n_resources") |> 
  arrange(factor(version, levels = c("R2", "R3", "R4", "R4B", "R5", "R6")))

write.csv(summary_counts, file.path(PROC_DIR, "summary_resource_counts_by_version.csv"), row.names = FALSE)

# ---- Create presence matrix ----
res_matrix <- all_resources |> 
  mutate(present = 1L) |> 
  pivot_wider(names_from = version, values_from = present, values_fill = 0L) |> 
  arrange(resource)
write.csv(res_matrix, file.path(PROC_DIR, "resource_presence_matrix.csv"), row.names = FALSE)

# ---- Calculate added and removed resources for each version transition ----
compare_versions <- function(v1, v2) {
  s1 <- filter(all_resources, version == v1) |> pull(resource)
  s2 <- filter(all_resources, version == v2) |> pull(resource)
  tibble(
    transition = paste(v1, "→", v2),
    added = list(setdiff(s2, s1)),
    removed = list(setdiff(s1, s2))
  )
}

version_pairs <- list(c("R2", "R3"), c("R3", "R4"), c("R4", "R4B"), c("R4B", "R5"), c("R5", "R6"))
diffs <- map_dfr(version_pairs, ~compare_versions(.x[1], .x[2]))

flatten_vec <- function(x) {
  if (length(x) == 0 || is.null(x)) return("")
  paste(sort(as.character(x)), collapse = ", ")
}

diffs_flat <- diffs |> 
  mutate(
    added = vapply(added, flatten_vec, character(1)),
    removed = vapply(removed, flatten_vec, character(1))
  )

write.csv(diffs_flat, file.path(PROC_DIR, "resources_added_removed_by_transition.csv"), row.names = FALSE)

# ---- Plot 1: Bar chart of resources per version ----
p_counts <- ggplot(summary_counts, aes(x = version, y = n_resources, fill = version)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n_resources), vjust = -0.3, color = "#0c223f", size = 4) +
  labs(title = "FHIR Resources by Version", x = "FHIR Version", y = "Number of Resources")
if (exists("apply_theme")) p_counts <- apply_theme(p_counts, base_size = 14)
ggsave(file.path(FIG_DIR, "ver_resource_counts.png"), p_counts, width = 8, height = 5, dpi = 150)

# ---- Plot 2: Heatmap of resource presence ----
heat_long <- res_matrix |> 
  pivot_longer(-resource, names_to = "version", values_to = "present") |> 
  mutate(version = factor(version, levels = c("R2", "R3", "R4", "R4B", "R5", "R6")))

order_by_presence <- heat_long |> 
  group_by(resource) |> 
  summarise(k = sum(present, na.rm = TRUE), .groups = "drop") |> 
  arrange(desc(k), resource) |> 
  pull(resource)
heat_long$resource <- factor(heat_long$resource, levels = rev(order_by_presence))

p_heat <- ggplot(heat_long, aes(x = version, y = resource, fill = present)) +
  geom_tile() +
  scale_fill_gradient(limits = c(0,1), breaks = c(0,1),
                      labels = c("Absent", "Present"),
                      low = "#11253f", high = "#33d17a", name = NULL) +
  labs(title = "FHIR Resource Presence Across Versions",
       x = "FHIR Version", y = "Resource") +
  theme(panel.grid = element_blank(), axis.text.y = element_text(size = 7))
if (exists("apply_theme")) p_heat <- apply_theme(p_heat, base_size = 12)
ggsave(file.path(FIG_DIR, "ver_resource_presence_heatmap.png"), p_heat, width = 9, height = 22, dpi = 150)

# ---- Plot 3: Stable resources (present in ≥4 versions) heatmap ----
stable <- heat_long |> group_by(resource) |> summarise(k = sum(present), .groups = "drop") |> filter(k >= 4)
heat_stable <- semi_join(heat_long, stable, by = "resource")
p_heat_stable <- ggplot(heat_stable, aes(x = version, y = resource, fill = present)) +
  geom_tile() +
  scale_fill_gradient(limits = c(0,1), breaks = c(0,1),
                      labels = c("Absent", "Present"),
                      low = "#11253f", high = "#33d17a", name = NULL) +
  labs(title = "Stable FHIR Resources (Present in ≥ 4 Versions)",
       x = "FHIR Version", y = "Resource") +
  theme(panel.grid = element_blank(), axis.text.y = element_text(size = 8))
if (exists("apply_theme")) p_heat_stable <- apply_theme(p_heat_stable, base_size = 12)
ggsave(file.path(FIG_DIR, "ver_resource_presence_heatmap_stable.png"), p_heat_stable, width = 9, height = 12, dpi = 150)

# ---- Plot 4: Added/Removed resources at each transition ----
delta_counts <- diffs %>%
  transmute(
    transition = factor(transition, levels = paste(c("R2", "R3", "R4", "R4B", "R5"), "→", c("R3", "R4", "R4B", "R5", "R6"))),
    added = lengths(map(diffs$added, identity)),
    removed = lengths(map(diffs$removed, identity))
  )
p_added <- ggplot(delta_counts, aes(x = transition, y = added, fill = transition)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = added), vjust = -0.3, color = "#0c223f", size = 4) +
  labs(title = "Resources Added at Each Version Transition", x = "Transition (previous → current)", y = "Count Added")
if (exists("apply_theme")) p_added <- apply_theme(p_added, base_size = 14)
ggsave(file.path(FIG_DIR, "ver_resources_added.png"), p_added, width = 9, height = 5, dpi = 150)

p_removed <- ggplot(delta_counts, aes(x = transition, y = removed, fill = transition)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = removed), vjust = -0.3, color = "#0c223f", size = 4) +
  labs(title = "Resources Removed at Each Version Transition", x = "Transition (previous → current)", y = "Count Removed")
if (exists("apply_theme")) p_removed <- apply_theme(p_removed, base_size = 14)
ggsave(file.path(FIG_DIR, "ver_resources_removed.png"), p_removed, width = 9, height = 5, dpi = 150)

# ---- Completion message ----
cat("\n✅ FHIR version comparison and visualization completed.\n",
    "CSVs saved to: ", normalizePath(PROC_DIR), "\n",
    "PNGs saved to: ", normalizePath(FIG_DIR), "\n")
