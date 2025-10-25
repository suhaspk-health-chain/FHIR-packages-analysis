# ==============================================================
# 07_compare_resources_across_versions.R
# Fetch official FHIR resource lists and compare versions
# ==============================================================

suppressPackageStartupMessages({
  library(httr2)
  library(rvest)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(kableExtra)
})

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
  resp <- request(u) |>
    req_user_agent("HC-FHIR-EDA/1.0 (R; contact: you@example.com)") |>
    req_timeout(timeout_sec) |>
    req_perform()
  read_html(resp_body_string(resp))
}

# Heuristic to identify resource links on resourcelist pages:
#  - anchor text looks like a single CamelCase token (e.g., ValueSet, Patient, DeviceMetric)
#  - href ends with ".html" and has no "/" (same directory)
#  - exclude a small set of known non-resource anchors by text
NOT_RESOURCE_TEXT <- c(
  "FHIR", "Index", "Downloads", "Profiles", "Datatypes", "DataTypes",
  "Definitions", "Extensions", "Base", "Summary", "Examples", "Modules",
  "Narrative", "Security", "Version", "Versions", "Conformance", "Search"
)

get_resources <- function(ver, url) {
  message("Fetching resources for ", ver)
  pg <- fetch_html(url)
  a  <- html_elements(pg, "a")
  tibble(
    text = html_text2(a),
    href = html_attr(a, "href")
  ) |>
    mutate(
      text = str_trim(text),
      href = str_trim(href)
    ) |>
    filter(
      is.na(text) == FALSE, is.na(href) == FALSE,
      # CamelCase-ish resource names, no spaces/dashes, start with upper
      str_detect(text, "^[A-Z][A-Za-z]+$"),
      !text %in% NOT_RESOURCE_TEXT,
      # href like "patient.html" (no subdirs), ignore external/anchors
      str_detect(href, "^[A-Za-z0-9]+\\.html$")
    ) |>
    transmute(version = ver, resource = text) |>
    distinct() |>
    arrange(resource)
}

all_resources <- map2_dfr(fhir_versions$version, fhir_versions$url, get_resources)

# --- Summary counts (quick sanity) ---
summary_counts <- all_resources |>
  count(version, name = "n_resources") |>
  arrange(version)
print(summary_counts)

# --- Presence matrix (✅ / ❌) ---
res_matrix <- all_resources |>
  mutate(present = "✅") |>
  tidyr::pivot_wider(
    names_from = version,
    values_from = present,
    values_fill = "❌"
  ) |>
  arrange(resource)

# --- Differences between consecutive versions ---
compare_versions <- function(v1, v2) {
  s1 <- all_resources |> filter(version == v1) |> pull(resource)
  s2 <- all_resources |> filter(version == v2) |> pull(resource)
  tibble(
    added_in = v2,
    new_resources = list(setdiff(s2, s1)),
    removed_in = v2,
    removed_resources = list(setdiff(s1, s2))
  )
}

version_pairs <- list(
  c("R2", "R3"),
  c("R3", "R4"),
  c("R4", "R4B"),
  c("R4B", "R5"),
  c("R5", "R6")
)

diffs <- map_dfr(version_pairs, ~compare_versions(.x[1], .x[2]))

# --- Display (first 60 rows for preview) ---
kable(head(res_matrix, 60), caption = "FHIR Resource Presence (preview)") |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) |>
  scroll_box(height = "420px")

# If you want to inspect all additions/removals:
View(diffs)
