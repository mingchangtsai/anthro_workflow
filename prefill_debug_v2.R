# prefill_debug_v2.R — deeper diagnostics

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(tibble)
library(glue)
library(purrr)

API_URL <- "https://script.google.com/macros/s/AKfycbwr-PjJpu-UmrOnEAM_IK9kuHjXOoOgK2TsEF0Zbh-zhIVpyF2Q0rJ_Rw6FcYfQmq4Y/exec"
API_KEY <- "cCkbo3tpI9CQLTBTY8bkeWeHt-6oHlCsZ8O7YuQ4fClB64LG1z_nq-oUdyZ4KsAf"

api_get <- function(params = list()) {
  params$key <- API_KEY
  resp <- httr::GET(API_URL, query = params, httr::timeout(30))
  stop_for_status(resp, task = paste("GET", params$action %||% ""))
  txt <- httr::content(resp, "text", encoding = "UTF-8")
  fromJSON(txt, simplifyVector = FALSE)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# 0) Show athletes and practitioners
cat("== Athletes ==\n")
aa <- api_get(list(action = "athletes"))
stopifnot(isTRUE(aa$ok))
athletes <- unlist(aa$data)
print(athletes)
if (length(athletes) == 0) stop("No athletes found in 'athlete' tab.")

# Pick athlete (edit here if needed)
athlete <- athletes[1]
cat(glue("Using athlete: '{athlete}'\n\n"))

# 1) Try get_anthro_latest
cat("== get_anthro_latest ==\n")
res_latest <- try(api_get(list(action = "get_anthro_latest", athlete = athlete)), silent = TRUE)
if (!inherits(res_latest, "try-error") && isTRUE(res_latest$ok)) {
  df_latest <- as_tibble(fromJSON(toJSON(res_latest$data)))
  cat(glue("Rows: {nrow(df_latest)}\n"))
  if (nrow(df_latest) > 0) {
    print(head(df_latest, 5))
  } else {
    cat("No rows.\n")
  }
} else {
  cat("Endpoint not implemented or returned !ok.\n")
}

# 2) Fallback: recent_anthro
cat("\n== recent_anthro (first 15) ==\n")
rec <- api_get(list(action = "recent_anthro", limit = 500))
stopifnot(isTRUE(rec$ok))
rdf <- as_tibble(fromJSON(toJSON(rec$data)))
print(head(rdf, 15))

# Candidate dates for this athlete
rdf <- rdf %>% mutate(ath_lower = tolower(trimws(athlete)))
target <- tolower(trimws(athlete))
cand <- rdf %>% mutate(athlete_lower_resp = tolower(trimws(athlete))) %>% filter(athlete_lower_resp == target)
cat(glue("\nCandidate rows for athlete in recent list: {nrow(cand)}\n"))
if (nrow(cand) > 0) {
  cand <- cand %>% mutate(date_parsed = suppressWarnings(ymd(date))) %>% arrange(desc(date_parsed))
  print(head(cand, 10))
  latest_date <- as.character(cand$date[1])
  cat(glue("Trying get_anthro with date = {latest_date}\n"))
  got <- api_get(list(action = "get_anthro", athlete = athlete, date = latest_date))
  if (isTRUE(got$ok) && length(got$data) > 0) {
    gdf <- as_tibble(fromJSON(toJSON(got$data)))
    cat(glue("get_anthro rows: {nrow(gdf)}\n"))
    print(head(gdf, 5))
    # Column names present
    cat("\nColumns returned:\n")
    print(names(gdf))
    # Summarize per measure
    ts_col <- intersect(names(gdf), c("timestamp","Submitted_At","submitted_at","ts"))
    if (length(ts_col) == 0) {
      gdf$.__ord <- as.numeric(suppressWarnings(ymd(gdf$date)))
    } else {
      gdf$.__ord <- as.numeric(suppressWarnings(ymd_hms(gdf[[ts_col[1]]])))
    }
    latest_per <- gdf %>% group_by(Category, Measure) %>% slice_max(order_by = .__ord, n = 1, with_ties = FALSE) %>% ungroup()
    cat(glue("\nLatest per measure: {nrow(latest_per)} rows\n"))
    print(head(latest_per %>% select(Category, Measure, m1, m2, m3), 15))
  } else {
    cat("get_anthro returned no rows for that date.\n")
  }
} else {
  cat("This athlete name did not appear in recent list. Check spelling/casing in master tab.\n")
}

cat("\nDone.\n")
