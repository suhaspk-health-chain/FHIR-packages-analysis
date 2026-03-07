# ==============================================================
# 11_US_realm_resources.R - FINAL CORRECTED VERSION
# Extract from Official HL7 Implementation Guide Registry
# ==============================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(stringr)
})

# ---- Load config -------------------------------------------------------------
source("scripts/01_config.R")

# ---- Step 1: Fetch Official HL7 IG Registry from GitHub ----------------------
message("Fetching official HL7 Implementation Guide registry...")

IG_REGISTRY_URLS <- c(
  "https://raw.githubusercontent.com/FHIR/ig-registry/master/fhir-ig-list.json",
  "https://build.fhir.org/ig-registry.json"
)

ig_registry <- NULL

for (url in IG_REGISTRY_URLS) {
  message("Trying: ", url)
  
  tryCatch({
    resp_text <- readLines(url, n = -1, warn = FALSE)
    resp_json <- paste(resp_text, collapse = "\n")
    ig_registry <- fromJSON(resp_json, simplifyVector = FALSE)
    
    message("✅ Successfully loaded from: ", url)
    break
    
  }, error = function(e) {
    message("  ❌ Failed: ", e$message)
  })
}

if (is.null(ig_registry)) {
  stop("❌ Could not fetch IG registry from any source")
}

# ---- Step 2: Extract guides from registry structure --------------------------
message("\nProcessing registry structure...")

if ("guides" %in% names(ig_registry)) {
  ig_list <- ig_registry$guides
  message("  Found 'guides' array")
} else {
  ig_list <- ig_registry
}

message("✅ Found ", length(ig_list), " implementation guides")

# ---- Step 3: Parse each guide manually (FLATTEN ALL DATA) -------------------
message("\nExtracting realm and version information...")

parse_guide <- function(guide) {
  tryCatch({
    # Get FHIR version from first edition - FLATTEN
    fhir_ver <- NA_character_
    if (!is.null(guide$editions) && length(guide$editions) > 0) {
      first_edition <- guide$editions[[1]]
      if (is.list(first_edition)) {
        fv_raw <- first_edition$`fhir-version`
        if (!is.null(fv_raw)) {
          # Flatten to character
          if (is.list(fv_raw)) {
            fhir_ver <- paste(unlist(fv_raw), collapse = "; ")
          } else {
            fhir_ver <- as.character(fv_raw[1])
          }
        }
      }
    }
    
    # Extract realm from country - FLATTEN
    realm_code <- tolower(as.character((guide$country %||% "uv")[1]))
    realm <- case_when(
      realm_code == "us" ~ "US",
      realm_code == "au" ~ "AU",
      realm_code == "nz" ~ "NZ",
      realm_code == "ca" ~ "CA",
      realm_code == "gb" ~ "UK",
      realm_code == "nl" ~ "NL",
      realm_code == "de" ~ "DE",
      realm_code == "no" ~ "NO",
      realm_code == "fr" ~ "FR",
      realm_code == "it" ~ "IT",
      realm_code == "at" ~ "AT",
      realm_code == "be" ~ "BE",
      realm_code == "ch" ~ "CH",
      realm_code == "dk" ~ "DK",
      realm_code == "se" ~ "SE",
      realm_code == "fi" ~ "FI",
      realm_code == "eu" ~ "EU",
      realm_code == "uv" ~ "International",
      TRUE ~ str_to_upper(realm_code)
    )
    
    data.frame(
      package_id = as.character((guide$`npm-name` %||% guide$name %||% NA_character_)[1]),
      realm = realm,
      fhir_version = fhir_ver,
      name = as.character((guide$name %||% NA_character_)[1]),
      country = realm_code,
      category = as.character((guide$category %||% NA_character_)[1]),
      description = as.character((guide$description %||% NA_character_)[1]),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      package_id = NA_character_,
      realm = NA_character_,
      fhir_version = NA_character_,
      name = NA_character_,
      country = NA_character_,
      category = NA_character_,
      description = NA_character_,
      stringsAsFactors = FALSE
    )
  })
}

# Parse all guides
ig_parsed <- lapply(ig_list, parse_guide)

# Combine into single data frame - THIS FLATTENS EVERYTHING
ig_with_realm <- do.call(rbind, ig_parsed) %>%
  as_tibble() %>%
  filter(!is.na(package_id), package_id != "") %>%
  mutate(
    package_id = as.character(package_id),
    realm = as.character(realm),
    fhir_version = as.character(fhir_version),
    name = as.character(name),
    country = as.character(country),
    category = as.character(category),
    description = as.character(description)
  )

message("✅ Processed ", nrow(ig_with_realm), " guides with realm information")

# ---- Step 4: Display Summary -----------------------------------------------
cat("\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("          FHIR IMPLEMENTATION GUIDE REGISTRY ANALYSIS SUMMARY          \n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat(sprintf("Total Implementation Guides: %d\n", nrow(ig_with_realm)))

cat("\n📊 Distribution by Realm:\n")
cat("─────────────────────────────────────────────────────────────────────\n")

realm_summary <- ig_with_realm %>%
  count(realm, sort = TRUE)

print(realm_summary, n = 30)

# ---- Step 5: US Analysis -------------------------------------------------------
us_guides <- ig_with_realm %>%
  filter(realm == "US")

cat("\n📊 US Implementation Guides:\n")
cat("─────────────────────────────────────────────────────────────────────\n")
cat(sprintf("Total US Guides: %d (%.1f%% of total)\n", 
            nrow(us_guides), 
            100 * nrow(us_guides) / nrow(ig_with_realm)))

if (nrow(us_guides) > 0) {
  us_by_version <- us_guides %>%
    count(fhir_version, sort = TRUE) %>%
    filter(!is.na(fhir_version), fhir_version != "NA")
  
  if (nrow(us_by_version) > 0) {
    cat("\nUS Guides by FHIR Version:\n")
    print(us_by_version)
  } else {
    cat("\n⚠️ FHIR version information not available for US guides\n")
  }
  
  cat("\n📋 Sample US Implementation Guides (top 20):\n")
  cat("─────────────────────────────────────────────────────────────────────\n")
  print(
    us_guides %>% 
      select(name, package_id, fhir_version) %>%
      head(20), 
    n = 20
  )
}

# ---- Step 6: Version Analysis -------------------------------------------------
cat("\n📊 Implementation Guides by FHIR Version:\n")
cat("─────────────────────────────────────────────────────────────────────\n")

by_version <- ig_with_realm %>%
  filter(!is.na(fhir_version), fhir_version != "NA") %>%
  count(fhir_version, sort = TRUE)

if (nrow(by_version) > 0) {
  print(by_version)
}

# ---- Step 7: International Analysis -----------------------------------------
international_guides <- ig_with_realm %>%
  filter(realm == "International")

cat("\n📊 International (UV) Implementation Guides:\n")
cat("─────────────────────────────────────────────────────────────────────\n")
cat(sprintf("Total International Guides: %d\n", nrow(international_guides)))

if (nrow(international_guides) > 0) {
  print(
    international_guides %>%
      select(name, package_id, fhir_version) %>%
      head(15),
    n = 20
  )
}

# ---- Step 8: Save Results (now with proper flattened data) ----------------------------
message("\nSaving results to CSV files...")

tryCatch({
  all_file <- file.path(PROC_DIR, "fhir_ig_all_official.csv")
  write.csv(ig_with_realm, all_file, row.names = FALSE)
  message("✅ ", all_file)
}, error = function(e) {
  message("❌ Error saving all_file: ", e$message)
})

tryCatch({
  us_file <- file.path(PROC_DIR, "fhir_ig_us_official.csv")
  write.csv(us_guides, us_file, row.names = FALSE)
  message("✅ ", us_file)
}, error = function(e) {
  message("❌ Error saving us_file: ", e$message)
})

tryCatch({
  realm_file <- file.path(PROC_DIR, "fhir_ig_realms_summary.csv")
  write.csv(realm_summary, realm_file, row.names = FALSE)
  message("✅ ", realm_file)
}, error = function(e) {
  message("❌ Error saving realm_file: ", e$message)
})

tryCatch({
  version_file <- file.path(PROC_DIR, "fhir_ig_by_version.csv")
  by_version_realm <- ig_with_realm %>%
    filter(!is.na(fhir_version), fhir_version != "NA") %>%
    count(fhir_version, realm, sort = TRUE)
  
  if (nrow(by_version_realm) > 0) {
    write.csv(by_version_realm, version_file, row.names = FALSE)
    message("✅ ", version_file)
  }
}, error = function(e) {
  message("⚠️ Note: ", e$message)
})

# ---- Step 9: Final Summary ---------------------------------------------------
cat("\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("                            FINAL SUMMARY                             \n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat(sprintf("✅ Total Implementation Guides: %d\n", nrow(ig_with_realm)))
cat(sprintf("✅ US Implementation Guides: %d (%.1f%%)\n", 
            nrow(us_guides), 
            100 * nrow(us_guides) / nrow(ig_with_realm)))
cat(sprintf("✅ International Guides: %d (%.1f%%)\n",
            nrow(international_guides),
            100 * nrow(international_guides) / nrow(ig_with_realm)))
cat(sprintf("✅ Realms Identified: %d\n", n_distinct(ig_with_realm$realm)))

# Show realms with guides
realms_list <- ig_with_realm %>%
  distinct(realm) %>%
  pull(realm) %>%
  sort()

cat(sprintf("✅ Realms: %s\n\n", paste(realms_list, collapse = ", ")))

cat("📊 Top 10 Realms by Implementation Guide Count:\n")
print(realm_summary %>% head(10))

cat("\n✅ Official IG Registry analysis complete!\n")
cat("   All files saved to: ", normalizePath(PROC_DIR), "\n\n")

# Display flattened data check
cat("Data Type Check:\n")
cat(sprintf("  package_id class: %s\n", class(ig_with_realm$package_id)[1]))
cat(sprintf("  realm class: %s\n", class(ig_with_realm$realm)[1]))
cat(sprintf("  fhir_version class: %s\n", class(ig_with_realm$fhir_version)[1]))
cat("\n")
