# ==============================================================
# 12_export_summary_tables.R
# Save all summary tables into /outputs/tables/
# ==============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(openxlsx)
  library(jsonlite)
})

# ---- Load paths ----
OUT_TABLE_DIR <- "outputs/tables"
PROC_DIR <- "data/processed"

# Create output folder if not exists
dir.create(OUT_TABLE_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- Helper function ----
load_csv <- function(name) {
  p <- file.path(PROC_DIR, name)
  if (file.exists(p)) readr::read_csv(p, show_col_types = FALSE) else NULL
}

save_table <- function(df, name) {
  if (!is.null(df) && nrow(df) > 0) {
    csv_path <- file.path(OUT_TABLE_DIR, paste0(name, ".csv"))
    xlsx_path <- file.path(OUT_TABLE_DIR, paste0(name, ".xlsx"))
    write_csv(df, csv_path)
    openxlsx::write.xlsx(df, xlsx_path)
    message("✅ Saved: ", basename(csv_path))
  } else {
    message("⚠️ No data to save for: ", name)
  }
}

# ---- Load key processed tables ----
resources     <- load_csv("fhir_resources.csv")
top_types     <- load_csv("top_resource_types.csv")
pkg_versions  <- load_csv("packages_by_version.csv")
res_versions  <- load_csv("resource_types_by_version.csv")
summary_counts <- load_csv("summary_resource_counts_by_version.csv")
presence_matrix <- load_csv("resource_presence_matrix.csv")
added_removed <- load_csv("resources_added_removed_by_transition.csv")

# ---- Generate and save useful summaries ----

# 1️⃣ Package count by version
tbl_pkg_by_ver <- resources %>%
  count(version, name = "package_count") %>%
  arrange(desc(package_count))
save_table(tbl_pkg_by_ver, "package_count_by_version")

# 2️⃣ Top 20 authors
tbl_authors <- resources %>%
  filter(!is.na(auth) & auth != "") %>%
  count(auth, sort = TRUE) %>%
  slice_head(n = 20)
save_table(tbl_authors, "top_20_authors")

# 3️⃣ Missingness summary
tbl_missing <- resources %>%
  summarise(across(everything(), ~sum(is.na(.x) | .x == ""), .names = "missing_{col}")) %>%
  tidyr::pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
  arrange(desc(missing_count))
save_table(tbl_missing, "missing_summary")

# 4️⃣ Already processed summary tables
save_table(top_types, "top_resource_types")
save_table(pkg_versions, "packages_by_version")
save_table(res_versions, "resource_types_by_version")
save_table(summary_counts, "summary_resource_counts_by_version")
save_table(presence_matrix, "resource_presence_matrix")
save_table(added_removed, "resources_added_removed_by_transition")

# 5️⃣ Combined metadata overview
overview <- tibble(
  total_resources = nrow(resources),
  unique_packages = n_distinct(resources$package_text),
  unique_resources = n_distinct(resources$identity_text),
  total_versions = n_distinct(resources$version),
  total_authors = n_distinct(resources$auth)
)
save_table(overview, "dataset_overview")


# ---- Paths for table outputs ----
TABLE_DIR <- file.path("outputs", "tables")
dir.create(TABLE_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- Helpers to save to processed and outputs/tables ----
save_both <- function(df, fname) {
  stopifnot(is.data.frame(df))
  write.csv(df, file.path(PROC_DIR,  fname), row.names = FALSE)
  write.csv(df, file.path(TABLE_DIR, fname), row.names = FALSE)
}

# ---- Flatten list-columns and build export tables ----
flatten_vec <- function(x) if (length(x) == 0 || is.null(x)) "" else paste(sort(as.character(x)), collapse = ", ")

diffs_flat <- diffs %>%
  mutate(
    added   = vapply(added,   flatten_vec, character(1)),
    removed = vapply(removed, flatten_vec, character(1))
  )

# Long presence (useful for dashboards/pivots)
heat_long <- res_matrix |>
  tidyr::pivot_longer(-resource, names_to = "version", values_to = "present") |>
  dplyr::mutate(version = factor(version, levels = c("R2","R3","R4","R4B","R5","R6")))

# Stable resources (present in ≥ 4 versions)
stable_tbl <- heat_long |>
  dplyr::group_by(resource) |>
  dplyr::summarise(versions_present = sum(present, na.rm = TRUE), .groups = "drop") |>
  dplyr::filter(versions_present >= 4) |>
  dplyr::arrange(desc(versions_present), resource)

# Added/Removed counts per transition (used for plots too)
delta_counts <- diffs %>%
  dplyr::transmute(
    transition = factor(transition, levels = paste(c("R2","R3","R4","R4B","R5"),
                                                   "→", c("R3","R4","R4B","R5","R6"))),
    added   = lengths(added),
    removed = lengths(removed)
  )

# ---- Save all tables to both locations ----
save_both(summary_counts,                       "summary_resource_counts_by_version.csv")
save_both(res_matrix,                           "resource_presence_matrix.csv")
save_both(diffs_flat,                           "resources_added_removed_by_transition.csv")
save_both(delta_counts,                         "delta_counts.csv")
save_both(stable_tbl,                           "stable_resources_ge4.csv")
save_both(heat_long,                            "resource_presence_long.csv")


# ---- Completion message ----
cat("\n✅ All summary tables saved to:\n", normalizePath(OUT_TABLE_DIR), "\n")
