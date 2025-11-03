# ==============================================================
# 11_us_realm_profiles_registry.R
# US Realm FHIR IGs → download from packages.fhir.org
# Extract profiled base resources, save CSVs + heatmap
# ==============================================================

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(stringr)
})

# ---------------- paths / theme ----------------
try({ if (file.exists("scripts/01_config.R")) source("scripts/01_config.R") }, silent = TRUE)
if (!exists("PROC_DIR")) PROC_DIR <- file.path(getwd(), "data", "processed")
if (!exists("FIG_DIR"))  FIG_DIR  <- file.path(getwd(), "figures")
dir.create(PROC_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR,  showWarnings = FALSE, recursive = TRUE)

apply_theme <- function(p, base_size = 14) {
  if (exists("theme_healthchain")) p + theme_healthchain(base_size) else
    p + theme_minimal(base_size = base_size)
}

# ---------------- config ----------------
HL7_US_ROOT <- "https://hl7.org/fhir/us"

US_IGS <- c(
  "core",           # US Core
  "qicore",         # QI-Core
  "carin-bb",       # CARIN Blue Button
  "davinci-pdex",   # Da Vinci PDex
  "davinci-pas",    # Da Vinci PAS
  "davinci-dtr",    # Da Vinci DTR
  "davinci-crd"     # Da Vinci CRD
)

# ---------------- helpers ----------------
`%||%` <- function(a, b) if (is.null(a)) b else a

safe_req <- function(u, timeout_sec = 60) {
  request(u) |>
    req_user_agent("HC-FHIR-EDA/1.0 (R; contact: you@example.com)") |>
    req_timeout(timeout_sec)
}

fetch_json <- function(u) {
  resp <- safe_req(u) |> req_perform()
  fromJSON(resp_body_string(resp), simplifyVector = TRUE)
}

# Read package-list.json and return (packageId, currentVersion)
get_pkg_meta <- function(slug) {
  url <- sprintf("%s/%s/package-list.json", HL7_US_ROOT, slug)
  pl <- fetch_json(url)
  
  pkg_id <- pl[["package-id"]] %||% pl[["packageId"]] %||% NA_character_
  cur_row <- if (!is.null(pl$list)) pl$list[pl$list$current %in% TRUE, , drop = FALSE] else NULL
  if (is.null(cur_row) || nrow(cur_row) == 0) {
    # If none marked current, take the first entry as a fallback
    if (!is.null(pl$list) && nrow(pl$list) > 0) cur_row <- pl$list[1, , drop = FALSE]
  }
  version <- cur_row$version[[1]] %||% pl[["version"]] %||% NA_character_
  tibble(slug = slug, packageId = pkg_id, version = version)
}

# Download npm package tgz from the registry
download_package_tgz <- function(packageId, version) {
  # packages.fhir.org serves the npm tarball directly
  url <- sprintf("https://packages.fhir.org/%s/%s", packageId, version)
  td <- tempfile("igpkg_"); dir.create(td)
  tgz <- file.path(td, "package.tgz")
  resp <- safe_req(url) |> req_perform()
  resp_body_file(resp, tgz)
  list(tgz = tgz, exdir = td)
}

# Extract StructureDefinition resource profiles from a package.tgz
extract_profiles <- function(tgz, exdir) {
  utils::untar(tgz, exdir = exdir)
  sdefs <- list.files(file.path(exdir, "package"), pattern = "^StructureDefinition-.*\\.json$", full.names = TRUE)
  if (length(sdefs) == 0) return(tibble(profile_url = character(), base_resource = character()))
  
  map_dfr(sdefs, function(fp) {
    j <- tryCatch(fromJSON(fp, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(j)) return(NULL)
    kind  <- j$kind %||% NA_character_
    deriv <- j$derivation %||% NA_character_
    btype <- j$type %||% NA_character_
    url   <- j$url %||% NA_character_
    
    if (!is.na(kind) && kind == "resource" &&
        !is.na(deriv) && deriv == "constraint" &&
        !is.na(btype) && nzchar(btype)) {
      tibble(profile_url = url, base_resource = btype)
    } else NULL
  })
}

# ---------------- main ----------------
message("==> Resolving package IDs + current versions...")
pkg_meta <- map_dfr(US_IGS, get_pkg_meta)

# Log table for fetch results/errors
fetch_log <- tibble(slug = character(), packageId = character(), version = character(),
                    ok = logical(), note = character())

us_ig_profiles <- map_dfr(seq_len(nrow(pkg_meta)), function(i) {
  row <- pkg_meta[i, ]
  slug <- row$slug
  pkg  <- row$packageId
  ver  <- row$version
  message(sprintf("US IG: %-14s | packageId=%s | version=%s", slug, pkg, ver))
  
  if (is.na(pkg) || is.na(ver) || !nzchar(pkg) || !nzchar(ver)) {
    assign("fetch_log", bind_rows(fetch_log, tibble(slug = slug, packageId = pkg, version = ver, ok = FALSE, note = "Missing packageId/version")), envir = .GlobalEnv)
    return(tibble(ig = slug, profile_url = character(), base_resource = character()))
  }
  
  profs <- tryCatch({
    dl <- download_package_tgz(pkg, ver)
    extract_profiles(dl$tgz, dl$exdir)
  }, error = function(e) {
    # Fallback attempt: try <path>/package.tgz from package-list (rarely needed)
    note <- paste("Registry download failed:", conditionMessage(e))
    assign("fetch_log", bind_rows(fetch_log, tibble(slug = slug, packageId = pkg, version = ver, ok = FALSE, note = note)), envir = .GlobalEnv)
    return(NULL)
  })
  
  if (is.null(profs) || nrow(profs) == 0) {
    assign("fetch_log", bind_rows(fetch_log, tibble(slug = slug, packageId = pkg, version = ver, ok = FALSE, note = "No StructureDefinitions found")), envir = .GlobalEnv)
    return(tibble(ig = slug, profile_url = character(), base_resource = character()))
  } else {
    assign("fetch_log", bind_rows(fetch_log, tibble(slug = slug, packageId = pkg, version = ver, ok = TRUE, note = "OK")), envir = .GlobalEnv)
    mutate(profs, ig = slug, .before = 1)
  }
})

# Save fetch log so you can inspect failures
write.csv(fetch_log, file.path(PROC_DIR, "usrealm_fetch_log.csv"), row.names = FALSE)

# ---------------- clean + presence matrix ----------------
us_ig_profiles <- us_ig_profiles |>
  filter(!is.na(base_resource), nzchar(base_resource)) |>
  distinct(ig, base_resource, profile_url)

if (nrow(us_ig_profiles) == 0) {
  message("⚠ No profiles extracted. Check ", file.path(PROC_DIR, "usrealm_fetch_log.csv"))
} else {
  message("Extracted profiles: ", nrow(us_ig_profiles))
}

us_presence_matrix <- us_ig_profiles |>
  mutate(present = 1L) |>
  distinct(ig, base_resource, .keep_all = TRUE) |>
  pivot_wider(names_from = ig, values_from = present, values_fill = 0L) |>
  arrange(base_resource) |>
  mutate(across(-base_resource, ~ as.integer(suppressWarnings(as.numeric(.)))))

us_counts_by_ig <- us_presence_matrix |>
  pivot_longer(-base_resource, names_to = "ig", values_to = "present") |>
  mutate(present = as.integer(suppressWarnings(as.numeric(present)))) |>
  filter(present == 1L) |>
  count(ig, name = "n_profiled_resources") |>
  arrange(desc(n_profiled_resources))

# ---------------- save ----------------
write.csv(us_ig_profiles,     file.path(PROC_DIR, "usrealm_profiles_long.csv"), row.names = FALSE)
write.csv(us_presence_matrix, file.path(PROC_DIR, "usrealm_resource_presence_matrix_by_ig.csv"), row.names = FALSE)
write.csv(us_counts_by_ig,    file.path(PROC_DIR, "usrealm_profiled_resource_counts_by_ig.csv"), row.names = FALSE)

# ---------------- plot ----------------
if (nrow(us_ig_profiles) > 0) {
  us_heat_long <- us_presence_matrix |>
    pivot_longer(-base_resource, names_to = "ig", values_to = "present") |>
    mutate(present = as.integer(suppressWarnings(as.numeric(present))))
  
  order_by_presence_us <- us_heat_long |>
    group_by(base_resource) |>
    summarise(k = sum(present, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(k), base_resource) |>
    pull(base_resource)
  
  us_heat_long$base_resource <- factor(us_heat_long$base_resource, levels = rev(order_by_presence_us))
  
  p_us_heat <- ggplot(us_heat_long, aes(x = ig, y = base_resource, fill = present)) +
    geom_tile() +
    scale_fill_gradient(limits = c(0,1), breaks = c(0,1),
                        labels = c("Absent","Present"),
                        low = "#11253f", high = "#33d17a", name = NULL) +
    labs(title = "US Realm IGs: Profiled Base Resources",
         x = "US Realm Implementation Guide", y = "Base Resource")
  p_us_heat <- apply_theme(p_us_heat, base_size = 12) +
    theme(panel.grid = element_blank(),
          axis.text.y = element_text(size = 8))
  
  ggsave(file.path(FIG_DIR, "usrealm_profile_presence_heatmap.png"),
         p_us_heat, width = 9, height = 14, dpi = 150)
}

message("\n✅ Done.")
message("CSVs → ", normalizePath(PROC_DIR))
message("PNG  → ", normalizePath(FIG_DIR))
