source("D:/health-chain-repository/HC Data analysis/FHIR-packages-analysis/scripts/00_setup_packages.R"); 
source("D:/health-chain-repository/HC Data analysis/FHIR-packages-analysis/scripts/01_config.R")
df <- arrow::read_parquet(file.path(OUT_DIR, "interim/fhir_packages_clean.parquet"))

# Derive resource_type + resource_id from Identity like "Patient/USCorePatient"
parse_identity <- function(x) {
  tibble(
    resource_type = ifelse(str_detect(x, "/"), str_replace(x, "/.*$", ""), NA_character_),
    resource_id   = ifelse(str_detect(x, "/"), str_replace(x, "^[^/]*/", ""), x)
  )
}

id_parts <- parse_identity(df$identity)
resources <- bind_cols(df, id_parts) %>%
  mutate(
    # Minimal keys
    package_key = paste0(package, "#", version),
    resource_key = paste0(package, "#", version, ":", resource_type, "/", resource_id)
  ) %>%
  relocate(package_key, resource_key, package, version, resource_type, resource_id)

# Save resource-level table
arrow::write_parquet(resources, file.path(OUT_DIR, "processed/fhir_resources.parquet"))
readr::write_csv(resources, file.path(OUT_DIR, "processed/fhir_resources.csv"))

# Quick sanity export (unique counts)
summary_counts <- list(
  n_rows = nrow(resources),
  n_packages = n_distinct(resources$package_key),
  n_resource_types = n_distinct(resources$resource_type)
)
writeLines(capture.output(str(summary_counts)), file.path(OUT_DIR, "interim/summary_resources.txt"))

