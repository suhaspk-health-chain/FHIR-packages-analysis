source("D:/health-chain-repository/HC Data analysis/FHIR-packages-analysis/scripts/00_setup_packages.R"); 
source("D:/health-chain-repository/HC Data analysis/FHIR-packages-analysis/scripts/01_config.R")

res <- arrow::read_parquet(file.path(OUT_DIR, "processed/fhir_resources.parquet"))

# Completeness snapshot
miss <- sapply(res, function(x) sum(is.na(x)))
readr::write_csv(
  tibble(variable = names(miss), missing = as.integer(miss)),
  file.path(OUT_DIR, "interim/missing_by_column.csv")
)

# Top resource types overall & by version
top_types <- res %>% count(resource_type, sort = TRUE)
readr::write_csv(top_types, file.path(OUT_DIR, "processed/top_resource_types.csv"))

types_by_version <- res %>% count(version, resource_type, sort = TRUE)
readr::write_csv(types_by_version, file.path(OUT_DIR, "processed/resource_types_by_version.csv"))

# Packages per version
pkgs_by_version <- res %>% distinct(package_key, version) %>% count(version, name = "n_packages")
readr::write_csv(pkgs_by_version, file.path(OUT_DIR, "processed/packages_by_version.csv"))
