# xig_resource_v1.2.R

# --- Required Libraries ---
if (!requireNamespace("rvest")) install.packages("rvest")
if (!requireNamespace("xml2")) install.packages("xml2")
if (!requireNamespace("httr2")) install.packages("httr2")
if (!requireNamespace("jsonlite")) install.packages("jsonlite")
if (!requireNamespace("cli")) install.packages("cli")
library(rvest)
library(xml2)
library(httr2)
library(jsonlite)
library(cli)

# --- Config ---
base_url     <- "https://packages2.fhir.org/xig"
# Output to data/raw/ with names the pipeline expects (run from project root)
ndjson_file  <- file.path("data", "raw", "xig_resources.ndjson")
json_file    <- file.path("data", "raw", "xig_resources.json")
dir.create(file.path("data", "raw"), showWarnings = FALSE, recursive = TRUE)
polite_sleep <- 0.3
max_retries  <- 3
timeout_sec  <- 30

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --- Utility: Robust Date Parsing ---
clean_date <- function(x) {
  x <- trimws(x)
  # Accept yyyy-mm, yyyy-mm-dd, or yyyy
  m <- regexpr("^(20[0-9][0-9](?:-[01][0-9])?(?:-[0-3][0-9])?)$", x)
  if (m > 0) {
    return(regmatches(x, m)[[1]])
  }
  return(NA_character_)
}

# --- Fetch HTML Doc with Retry ---
fetch_doc <- function(url, retries = max_retries, wait = 1) {
  for (i in seq_len(retries)) {
    ok <- try({
      resp <- request(url) |>
        req_user_agent("FHIR-XIG-EDA-Scraper/1.4 (R rvest; academic research)") |>
        req_timeout(timeout_sec) |>
        req_perform()
      s <- resp_status(resp)
      if (s >= 200 && s < 300)
        return(read_html(resp_body_string(resp)))
      else
        cli_alert_warning(sprintf("HTTP %s for %s", s, url))
    }, silent = TRUE)
    Sys.sleep(wait * i)
  }
  stop(sprintf("Failed to fetch: %s after %d retries", url, retries))
}

# --- Extract Table Rows Robustly ---
extract_rows <- function(doc) {
  # 1. Try table with known headers
  tbl <- html_element(doc, xpath =
                        "//table[.//th[normalize-space()='Package'] and .//th[normalize-space()='Identity'] and .//th[normalize-space()='Name/Title']]"
  )
  # 2. Fallback to Resources - All Kinds heading
  if (is.null(tbl)) {
    hdr <- html_element(doc, xpath =
                          "//*[@id='rendering']//h3[contains(normalize-space(), 'Resources') and contains(normalize-space(), 'All Kinds')] | //h3[contains(normalize-space(), 'Resources') and contains(normalize-space(), 'All Kinds')] | //h2[contains(normalize-space(), 'Resources') and contains(normalize-space(), 'All Kinds')]"
    )
    tbl <- if (!is.null(hdr)) html_element(hdr, xpath = "following-sibling::table[1]") else NULL
    if (is.null(tbl)) return(list())
  }
  trs <- html_elements(tbl, xpath = ".//tr[td]")
  if (length(trs) == 0) return(list())
  out <- vector("list", length(trs))
  for (i in seq_along(trs)) {
    tds <- html_elements(trs[[i]], "td")
    text <- vapply(tds, html_text2, "", USE.NAMES = FALSE)
    text <- c(text, rep("", max(0, 10 - length(text)))) # normalize to 10 cols
    pkg_a <- if (length(tds) >= 1) html_element(tds[[1]], "a") else NULL
    id_a  <- if (length(tds) >= 3) html_element(tds[[3]], "a") else NULL
    out[[i]] <- list(
      package_text  = text[1],
      package_href  = if (!is.null(pkg_a)) html_attr(pkg_a, "href") else NA_character_,
      version       = text[2],
      identity_text = text[3],
      identity_href = if (!is.null(id_a)) html_attr(id_a, "href") else NA_character_,
      name_title    = text[4],
      status        = text[5],
      fmm           = text[6],
      wg            = text[7],
      date_raw      = text[8],
      date          = clean_date(text[8]),
      realm         = text[9],
      auth          = text[10]
    )
  }
  out
}

# --- Find Next Page URL ---
find_next <- function(doc, current_url) {
  anchors <- html_elements(doc, "a")
  if (length(anchors) == 0) return(NA_character_)
  texts <- trimws(html_text2(anchors))
  hrefs <- html_attr(anchors, "href")
  cand <- which(grepl("^Next\\b", texts, ignore.case = TRUE) & grepl("offset=", hrefs %||% ""))
  if (length(cand) > 0) return(url_absolute(hrefs[cand[1]], current_url))
  cand <- which(grepl("^Next\\b", texts, ignore.case = TRUE))
  if (length(cand) > 0) return(url_absolute(hrefs[cand[1]], current_url))
  u <- httr2::url_parse(current_url)
  qs <- u$query
  if (!is.null(qs$offset)) {
    off <- suppressWarnings(as.integer(qs$offset))
    if (!is.na(off)) {
      qs$offset <- as.character(off + 200L)
      u$query <- qs
      return(httr2::url_build(u))
    }
  }
  NA_character_
}

# --- Save NDJSON ---
append_ndjson <- function(recs, con) {
  for (rec in recs) writeLines(toJSON(rec, auto_unbox = TRUE, null = "null"), con)
}

# --- Main ---
cli_h1("XIG Resource Scraper v1.2")
if (file.exists(ndjson_file)) file.remove(ndjson_file)
con <- file(ndjson_file, open = "a", encoding = "UTF-8")
all_rows <- list()
total <- 0L
page_no <- 0L
url <- base_url
repeat {
  page_no <- page_no + 1L
  cli_h2(sprintf("Page %d", page_no))
  doc <- fetch_doc(url)
  rows <- extract_rows(doc)
  n <- length(rows)
  cli_alert_success(sprintf("Extracted %d rows", n))
  if (n == 0) {
    cli_alert_warning(sprintf("No rows extracted on page %d. Terminating.", page_no))
    break
  }
  append_ndjson(rows, con)
  all_rows <- c(all_rows, rows)
  total <- total + n
  next_url <- find_next(doc, url)
  if (is.na(next_url)) {
    cli_alert_info(sprintf("No more pages. Rows: %d", total))
    break
  }
  Sys.sleep(polite_sleep)
  url <- next_url
}
close(con)   # Only close after scraping is complete

if (length(all_rows) > 0) {
  write_json(all_rows, json_file, pretty = TRUE, auto_unbox = TRUE)
  cli_alert_success(sprintf("Saved data: %d rows to %s", length(all_rows), json_file))
} else {
  cli_alert_warning("No data collected — JSON not written.")
}
ndjson_count <- if (file.exists(ndjson_file)) length(readLines(ndjson_file)) else 0L
cli_h1("Done")
cli_alert_success(sprintf("NDJSON lines: %d | JSON array length: %d", ndjson_count, length(all_rows)))
if (length(all_rows) > 0) print(utils::head(all_rows, 2))
