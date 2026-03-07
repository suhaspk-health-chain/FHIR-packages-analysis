# ==============================================================
# 08_plot_version_resource_comparison.R
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
  library(patchwork)
  library(jsonlite)
})

source("scripts/01_config.R")
source("scripts/05_theme_healthchain.R")

apply_theme <- function(p, base_size = 14) {
  if (exists("theme_healthchain")) p + theme_healthchain(base_size) else p + theme_minimal(base_size = base_size)
}

# ---- Resource Scraping ----
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
fetch_html <- function(u, timeout_sec = 30) {
  resp <- request(u) |> req_user_agent("HC-FHIR-EDA/1.0 (R; contact: you@example.com)") |> req_timeout(timeout_sec) |> req_perform()
  read_html(resp_body_string(resp))
}
NOT_RESOURCE_TEXT <- c(
  "FHIR", "Index", "Downloads", "Profiles", "Datatypes", "DataTypes", "Definitions",
  "Extensions", "Base", "Summary", "Examples", "Modules", "Narrative", "Security",
  "Version", "Versions", "Conformance", "Search"
)
get_resources <- function(ver, url) {
  message("Fetching resources for ", ver)
  pg <- fetch_html(url)
  a  <- html_elements(pg, "a")
  tibble(
    text = html_text2(a),
    href = html_attr(a, "href")
  ) |> mutate(text = str_trim(text), href = str_trim(href)) |>
    filter(
      !is.na(text), !is.na(href),
      str_detect(text, "^[A-Z][A-Za-z]+$"),
      !text %in% NOT_RESOURCE_TEXT,
      str_detect(href, "^[A-Za-z0-9]+\\.html$")
    ) |> transmute(version = ver, resource = text) |> distinct() |> arrange(resource)
}
all_resources <- map2_dfr(fhir_versions$version, fhir_versions$url, get_resources)
saveRDS(all_resources, file.path(PROC_DIR, "all_resources.rds"))

summary_counts <- all_resources |> count(version, name = "n_resources") |> arrange(factor(version, levels = fhir_versions$version))

res_matrix <- all_resources |> mutate(present = 1L) |> pivot_wider(names_from = version, values_from = present, values_fill = 0L) |> arrange(resource)

# ---- Version Diffs ----
compare_versions <- function(v1, v2) {
  s1 <- filter(all_resources, version == v1) |> pull(resource)
  s2 <- filter(all_resources, version == v2) |> pull(resource)
  tibble(transition = paste(v1, "→", v2), added = list(setdiff(s2, s1)), removed = list(setdiff(s1, s2)))
}
version_pairs <- list(c("R2", "R3"), c("R3", "R4"), c("R4", "R4B"), c("R4B", "R5"), c("R5", "R6"))
diffs <- map_dfr(version_pairs, ~compare_versions(.x[1], .x[2]))

flatten_vec <- function(x) {
  if (length(x) == 0 || is.null(x)) return("")
  paste(sort(as.character(x)), collapse = ", ")
}
diffs_flat <- diffs %>% mutate(
  added = vapply(added, flatten_vec, character(1)),
  removed = vapply(removed, flatten_vec, character(1))
)
write.csv(summary_counts, file.path(PROC_DIR, "summary_resource_counts_by_version.csv"), row.names = FALSE)
write.csv(res_matrix, file.path(PROC_DIR, "resource_presence_matrix.csv"), row.names = FALSE)
write.csv(diffs_flat, file.path(PROC_DIR, "resources_added_removed_by_transition.csv"), row.names = FALSE)

# ---- Resource Presence Heatmaps ----
heat_long <- res_matrix |>
  pivot_longer(-resource, names_to = "version", values_to = "present") |>
  mutate(version = factor(version, levels = fhir_versions$version))
order_by_presence <- heat_long |> group_by(resource) |> summarise(k = sum(present, na.rm = TRUE), .groups = "drop") |> arrange(desc(k), resource) |> pull(resource)
heat_long$resource <- factor(heat_long$resource, levels = rev(order_by_presence))

p_heat <- ggplot(heat_long, aes(x = version, y = resource, fill = present)) +
  geom_tile() +
  scale_fill_gradient(limits = c(0,1), breaks = c(0,1), labels = c("Absent","Present"), low = "#11253f", high = "#33d17a", name = NULL) +
  labs(title = "FHIR Resource Presence Across Versions", x = "FHIR Version", y = "Resource") +
  theme(panel.grid = element_blank(), axis.text.y = element_text(size = 7))
p_heat <- apply_theme(p_heat, base_size = 12)
ggsave(file.path(FIG_DIR, "ver_resource_presence_heatmap.png"), p_heat, width = 9, height = 22, dpi = 150)

stable <- heat_long |> group_by(resource) |> summarise(k = sum(present), .groups = "drop") |> filter(k >= 4)
heat_stable <- semi_join(heat_long, stable, by = "resource")
p_heat_stable <- ggplot(heat_stable, aes(x = version, y = resource, fill = present)) +
  geom_tile() +
  scale_fill_gradient(limits = c(0,1), breaks = c(0,1), labels = c("Absent","Present"), low = "#11253f", high = "#33d17a", name = NULL) +
  labs(title = "Stable FHIR Resources (Present in ≥ 4 Versions)", x = "FHIR Version", y = "Resource") +
  theme(panel.grid = element_blank(), axis.text.y = element_text(size = 8))
p_heat_stable <- apply_theme(p_heat_stable, base_size = 12)
ggsave(file.path(FIG_DIR, "ver_resource_presence_heatmap_stable.png"), p_heat_stable, width = 9, height = 12, dpi = 150)

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
p_added <- apply_theme(p_added, base_size = 14)
ggsave(file.path(FIG_DIR, "ver_resources_added.png"), p_added, width = 9, height = 5, dpi = 150)

p_removed <- ggplot(delta_counts, aes(x = transition, y = removed, fill = transition)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = removed), vjust = -0.3, color = "#0c223f", size = 4) +
  labs(title = "Resources Removed at Each Version Transition", x = "Transition (previous → current)", y = "Count Removed")
p_removed <- apply_theme(p_removed, base_size = 14)
ggsave(file.path(FIG_DIR, "ver_resources_removed.png"), p_removed, width = 9, height = 5, dpi = 150)

# ==== Realm Diffs & 2x2 Grid ====
xig_resources <- fromJSON(file.path(PROC_DIR, "xig_resources.json"), simplifyDataFrame = TRUE)
xig_resources <- as_tibble(xig_resources)
names(xig_resources) <- str_replace_all(tolower(names(xig_resources)), "\\.", "_")
resource_versions <- xig_resources %>%
  mutate(
    version = factor(version, levels = fhir_versions$version),
    realm = ifelse(is.na(realm) | realm == "" | toupper(realm) %in% c("NONE", "NA"), NA, realm)
  ) %>%
  select(resource = identity_text, version, realm)

versions <- fhir_versions$version

get_resource_set <- function(v, realm_filter = NULL) {
  df <- resource_versions %>% filter(version == v)
  if (!is.null(realm_filter)) df <- df %>% filter(realm == realm_filter)
  unique(df$resource)
}

global_diffs <- lapply(seq_along(versions)[-1], function(i) {
  prev <- get_resource_set(versions[i-1])
  curr <- get_resource_set(versions[i])
  list(
    transition = paste(versions[i-1], "→", versions[i]),
    added = setdiff(curr, prev),
    deleted = setdiff(prev, curr)
  )
})

us_diffs <- lapply(seq_along(versions)[-1], function(i) {
  prev <- get_resource_set(versions[i-1], "US")
  curr <- get_resource_set(versions[i], "US")
  list(
    transition = paste(versions[i-1], "→", versions[i]),
    added = setdiff(curr, prev),
    deleted = setdiff(prev, curr)
  )
})

global_df <- tibble(
  transition = sapply(global_diffs, `[[`, "transition"),
  added = sapply(global_diffs, function(x) length(x$added)),
  deleted = sapply(global_diffs, function(x) length(x$deleted))
)
us_df <- tibble(
  transition = sapply(us_diffs, `[[`, "transition"),
  added = sapply(us_diffs, function(x) length(x$added)),
  deleted = sapply(us_diffs, function(x) length(x$deleted))
)

p_add_global <- ggplot(global_df, aes(x = transition, y = added, fill = transition)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = added), vjust = -0.3, color = "#0c223f", size = 4) +
  labs(title = "Resource Additions (Global)", x = "Version Transition", y = "Count Added")
p_add_global <- apply_theme(p_add_global, base_size = 12)

p_del_global <- ggplot(global_df, aes(x = transition, y = deleted, fill = transition)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = deleted), vjust = -0.3, color = "#0c223f", size = 4) +
  labs(title = "Resource Deletions (Global)", x = "Version Transition", y = "Count Deleted")
p_del_global <- apply_theme(p_del_global, base_size = 12)

p_add_us <- ggplot(us_df, aes(x = transition, y = added, fill = transition)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = added), vjust = -0.3, color = "#0c223f", size = 4) +
  labs(title = "Resource Additions (US Realm)", x = "Version Transition", y = "Count Added")
p_add_us <- apply_theme(p_add_us, base_size = 12)

p_del_us <- ggplot(us_df, aes(x = transition, y = deleted, fill = transition)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = deleted), vjust = -0.3, color = "#0c223f", size = 4) +
  labs(title = "Resource Deletions (US Realm)", x = "Version Transition", y = "Count Deleted")
p_del_us <- apply_theme(p_del_us, base_size = 12)

grid_plot <- (p_add_global | p_del_global) / (p_add_us | p_del_us)
ggsave(file.path(FIG_DIR, "resource_adds_deletes_grid.png"), grid_plot, width = 16, height = 11, dpi = 150)

cat("\n✅ FHIR resource additions/deletions by version and realm grid saved to: ", normalizePath(FIG_DIR), "\n")
cat("\n✅ FHIR version comparison completed.\nCSVs saved to: ", normalizePath(PROC_DIR), "\nPNGs saved to: ", normalizePath(FIG_DIR), "\n")
