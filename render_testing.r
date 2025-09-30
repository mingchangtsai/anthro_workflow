# --- render_report_from_api.R -----------------------------------------------
# Render anthro_report_PDF.Rmd using the same API as app.R (no googlesheets4).
# Output PDF is written to the current working directory.

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite); library(httr);  library(rmarkdown)
})

# ================== API CONFIG (copied from app.R) ==================
API_URL <- "https://script.google.com/macros/s/AKfycbxPfNlemEphi3kKsT_A9tDw3PLapodl73vfCGLp05bnvLwmGsPCJkARZhvGQSdt4MkB/exec"
API_KEY <- "cCkbo3tpI9CQLTBTY8bkeWeHt-6oHlCsZ8O7YuQ4fClB64LG1z_nq-oUdyZ4KsAf"

`%||%` <- function(a, b) if (is.null(a)) b else a
norm_name <- function(x) tolower(trimws(x))

api_get <- function(params = list()) {
  stopifnot(is.list(params))
  params$key <- API_KEY
  action <- if (is.null(params$action)) "" else as.character(params$action)
  resp <- httr::GET(API_URL, query = params, httr::timeout(30))
  httr::stop_for_status(resp, task = paste("GET", action))
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

# ================== FETCH HELPERS (same semantics as in app.R) ==================
fetch_recent <- function(limit = 10) {
  res <- try(api_get(list(action = "recent_anthro", limit = limit, cb = as.integer(Sys.time()))), silent = TRUE)
  if (inherits(res, "try-error") || !isTRUE(res$ok) || length(res$data) == 0) {
    return(tibble::tibble(date = character(), athlete = character()))
  }
  tibble::as_tibble(jsonlite::fromJSON(jsonlite::toJSON(res$data)))
}

fetch_dates_for_athlete <- function(ath) {
  # Try endpoint
  res <- try(api_get(list(action = "dates_for_athlete", athlete = ath, cb = as.integer(Sys.time()))), silent = TRUE)
  if (!inherits(res, "try-error") && isTRUE(res$ok) && length(res$data) > 0) {
    d <- try(tibble::as_tibble(jsonlite::fromJSON(jsonlite::toJSON(res$data))), silent = TRUE)
    if (!inherits(d, "try-error")) {
      cand <- if ("norm" %in% names(d)) d$norm else if ("date" %in% names(d)) d$date else character()
      cand <- unique(trimws(as.character(cand)))
      cand <- cand[nzchar(cand)]
      if (length(cand)) return(sort(cand))
    }
  }
  # Fallback to recent
  rec <- fetch_recent(2000)
  out <- rec %>%
    dplyr::filter(norm_name(athlete) == norm_name(ath)) %>%
    dplyr::pull(date) %>%
    as.character() %>%
    trimws() %>%
    unique()
  sort(out)
}

fetch_athlete_history <- function(ath) {
  dates <- fetch_dates_for_athlete(ath)
  if (length(dates) == 0) return(tibble())
  
  rows <- vector("list", length(dates))
  for (i in seq_along(dates)) {
    d0 <- as.character(dates[[i]])
    got <- try(api_get(list(action = "get_anthro", athlete = ath, date = d0, cb = as.integer(Sys.time()))), silent = TRUE)
    if (!inherits(got, "try-error") && isTRUE(got$ok) && length(got$data) > 0) {
      dat <- try(tibble::as_tibble(jsonlite::fromJSON(jsonlite::toJSON(got$data), flatten = TRUE)), silent = TRUE)
      if (!inherits(dat, "try-error") && nrow(dat) > 0) {
        rowi <- dat[1, , drop = FALSE]
        # force the requested date into the row to avoid API-side inconsistencies
        rowi$date <- d0
        rows[[i]] <- rowi
      }
    }
  }
  
  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0) return(out)
  
  # normalize types
  names(out) <- tolower(names(out))
  if (!inherits(out$date, "Date")) {
    suppressWarnings({
      out$date <- as.Date(out$date)
    })
  }
  
  # clean + sort
  out <- out %>%
    tibble::as_tibble() %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::arrange(date)
  
  out
}

# ================== USER CONFIG FOR THIS TEST RUN ==================
# 1) Render a single athlete by name (exact, case-insensitive):
ATHLETE_NAME <- "Mikaila Aalhus"  # e.g. "Mikaila Aalhus". If NULL, the most recent athlete is used.
# Heather Abadie

# 2) Files
RMD_FILE <- "anthro_report_PDF.Rmd"
STOP_IF_MISSING_RMD <- TRUE

# ================== PICK ATHLETE ==================
if (is.null(ATHLETE_NAME)) {
  recent <- fetch_recent(50)
  if (nrow(recent) == 0) stop("No recent entries available from API.")
  ATHLETE_NAME <- as.character(recent$athlete[1])
  message("ATHLETE_NAME not set; using most recent athlete: ", ATHLETE_NAME)
} else {
  message("Using ATHLETE_NAME = ", ATHLETE_NAME)
}

# ================== BUILD AthleteData FOR THE Rmd ==================
hist <- fetch_athlete_history(ATHLETE_NAME)
if (nrow(hist) == 0) stop("No history returned for athlete: ", ATHLETE_NAME)

# Ensure expected columns exist and types look sane for the Rmd’s prep chunk
names(hist) <- tolower(names(hist))

req_base <- c("practitioner","athlete","date","comments")
for (nm in req_base) if (!nm %in% names(hist)) hist[[nm]] <- NA_character_

# coerce date
if (!inherits(hist$date, "Date")) suppressWarnings(hist$date <- as.Date(hist$date))

# coerce *_value numeric (the Rmd renames to drop suffixes)
value_cols <- grep("(_cm_value|_mm_value|_kg_value|_value)$", names(hist), value = TRUE)
hist[value_cols] <- lapply(hist[value_cols], function(x) suppressWarnings(as.numeric(x)))

AthleteData <- hist %>%
  select(all_of(req_base), all_of(value_cols)) %>%
  arrange(desc(date))

# ================== RENDER ==================
if (!file.exists(RMD_FILE)) {
  msg <- paste0("Template '", RMD_FILE, "' not found in: ", normalizePath(getwd(), winslash = "/"))
  if (STOP_IF_MISSING_RMD) stop(msg) else warning(msg)
}

out_pdf <- paste0(ATHLETE_NAME, "_", format(Sys.Date(), "%Y-%m-%d"), ".pdf")
message("Output PDF: ", file.path(getwd(), out_pdf))

# Provide AthleteData via environment (like the app)
renv <- new.env(parent = globalenv())
renv$AthleteData <- AthleteData

# knit from the current working directory; write output here
rmarkdown::render(
  input         = RMD_FILE,
  output_format = "pdf_document",
  output_file   = out_pdf,
  output_dir    = getwd(),
  envir         = renv,
  quiet         = FALSE
)

if (file.exists(out_pdf)) {
  message("✅ Done. PDF created at: ", normalizePath(out_pdf, winslash = "/"))
} else {
  stop("❌ Render completed but PDF not found: ", out_pdf)
}
# -------------------------------------------------------------------
