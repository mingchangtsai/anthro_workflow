# app.R — CSI Pacific Anthropometry (cached-reads edition)

library(shiny)
library(shinyWidgets)
library(shinyTime)
library(shinyjs)
library(DT)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(glue)
library(lubridate)
library(jsonlite)
library(httr)
library(readxl)
library(readr)
library(stringr)
library(rmarkdown)   # for rendering PDF reports
library(cachem)      # cache for session fetches during hydrate
# library(future)
# library(future.apply)

# ================== API CONFIG ==================
API_URL <- "https://script.google.com/macros/s/AKfycbxPfNlemEphi3kKsT_A9tDw3PLapodl73vfCGLp05bnvLwmGsPCJkARZhvGQSdt4MkB/exec"
API_KEY <- "cCkbo3tpI9CQLTBTY8bkeWeHt-6oHlCsZ8O7YuQ4fClB64LG1z_nq-oUdyZ4KsAf"

# ---------- tiny utils ----------
`%||%` <- function(a, b) if (is.null(a)) b else a   # safe null-coalesce
safe_id   <- function(x) gsub("[^A-Za-z0-9_]", "_", x)
cap_first <- function(s) {
  if (is.null(s) || length(s) == 0) return(s)
  s <- trimws(s)
  ifelse(is.na(s) | nchar(s) == 0, s, paste0(toupper(substr(s,1,1)), substring(s,2)))
}
norm_name <- function(x) tolower(trimws(x))

# ---- tolerant coercion helpers ----
as_chr1 <- function(x) {
  if (is.list(x)) {
    vapply(x, function(v) if (length(v) == 0 || is.null(v)) NA_character_ else as.character(v[[1]]),
           character(1))
  } else as.character(x)
}

# Accept both named rows and positional arrays
rows_list_to_tibble <- function(x) {
  if (is.null(x)) return(tibble::tibble())
  purrr::map_dfr(x, function(r) {
    if (is.list(r) && length(names(r))) {
      d <- r
      date  <- d$date %||% d$collection_date %||% d$session_date %||% d$timestamp
      name  <- d$athlete %||% d$athletename %||% d$name
      ts    <- d$timestamp %||% d$date
      tibble::tibble(
        date      = as_chr1(date),
        athlete   = as_chr1(name),
        timestamp = as_chr1(ts)
      )
    } else {
      v <- unlist(r, recursive = FALSE, use.names = FALSE)
      tibble::tibble(
        date      = as_chr1(if (length(v) >= 1) v[[1]] else NA_character_),
        athlete   = as_chr1(if (length(v) >= 2) v[[2]] else NA_character_),
        timestamp = as_chr1(if (length(v) >= 3) v[[3]] else NA_character_)
      )
    }
  })
}

# Normalize col names + types (flattens list -> chr)
fix_ad_cols <- function(df) {
  df <- tibble::as_tibble(df)
  names(df) <- tolower(names(df))
  if (!"athlete" %in% names(df)) df$athlete <- NA_character_
  if (!"date"    %in% names(df)) df$date    <- NA_character_
  df$athlete <- as_chr1(df$athlete)
  df$date    <- as_chr1(df$date)
  if ("timestamp" %in% names(df)) df$timestamp <- as_chr1(df$timestamp)
  df
}

alpha_sort <- function(x) {
  x <- unique(na.omit(as.character(x)))
  x[order(tolower(x))]
}
# ---- schema normalizer (make sure we always have athlete + date) ----
normalize_cols <- function(df) {
  df <- safe_tibble(df)
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  names(df) <- tolower(names(df))
  # athlete
  if (!"athlete" %in% names(df)) {
    if ("athletename" %in% names(df)) df <- dplyr::rename(df, athlete = athletename)
    else if ("name" %in% names(df))   df <- dplyr::rename(df, athlete = name)
  }
  # date
  if (!"date" %in% names(df)) {
    if ("collection_date" %in% names(df)) df <- dplyr::rename(df, date = collection_date)
    else if ("session_date" %in% names(df)) df <- dplyr::rename(df, date = session_date)
    else if ("timestamp" %in% names(df)) df$date <- as.Date(df$timestamp)
  }
  df
}

safe_file <- function(x) {
  x <- gsub("[/:*?\"<>|\\s]+", "_", x)
  x <- gsub("_+", "_", x)
  trimws(x, which = "both", whitespace = "_")
}

# ---- safe helpers ----
safe_tibble <- function(x) {
  if (is.null(x)) return(tibble())
  if (inherits(x, "data.frame")) return(tibble::as_tibble(x))
  if (is.list(x)) {
    out <- try(tibble::as_tibble(x), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
  }
  tibble()
}

safe_bind_rows <- function(lst) {
  lst <- purrr::compact(lst)      # drop NULLs
  lst <- lapply(lst, safe_tibble) # coerce each to a tibble (or empty tibble)
  if (length(lst) == 0) return(tibble())
  dplyr::bind_rows(lst)
}

# ---------- API HELPERS ----------
api_get <- function(params = list()) {
  stopifnot(is.list(params))
  params$key <- API_KEY
  action <- if (is.null(params$action)) "" else as.character(params$action)
  
  resp <- httr::GET(API_URL, query = params, httr::timeout(30))
  httr::stop_for_status(resp, task = paste("GET", action))
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

# Generic POST caller (keeps your diagnostics)
api_post <- function(body) {
  stopifnot(is.list(body))
  body$key <- API_KEY
  payload <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
  
  resp <- try(httr::POST(
    API_URL,
    body = payload,
    httr::content_type("text/plain; charset=UTF-8"),
    httr::timeout(30)
  ), silent = TRUE)
  
  if (inherits(resp, "try-error")) {
    return(list(ok = FALSE, status_code = NA_integer_, message = as.character(resp), raw = NULL))
  }
  
  status <- httr::status_code(resp)
  raw <- try(httr::content(resp, as = "text", encoding = "UTF-8"), silent = TRUE)
  raw <- if (inherits(raw, "try-error")) NULL else raw
  parsed <- try(jsonlite::fromJSON(raw, simplifyVector = FALSE), silent = TRUE)
  
  if (!inherits(parsed, "try-error") && is.list(parsed)) {
    parsed$status_code <- status
    parsed$raw <- raw
    parsed$message <- parsed$message %||% parsed$error %||% paste("HTTP", status)
    return(parsed)
  }
  
  list(ok = (status >= 200 && status < 300), status_code = status,
       message = "Non-JSON response", raw = raw)
}

# Try a sequence of action names until one works
.try_actions <- function(payload_base, actions) {
  msgs <- c()
  raws <- c()
  for (a in actions) {
    res <- api_post(c(payload_base, list(action = a)))
    if (isTRUE(res$ok)) return(list(ok = TRUE, message = res$message %||% paste("ok via", a), raw = res$raw, action = a))
    msgs <- c(msgs, sprintf("%s → %s", a, res$message %||% "<no message>"))
    raws <- c(raws, res$raw %||% "<no raw>")
    # If server said explicitly "unknown action", keep trying; otherwise break early.
    if (!grepl("unknown action", tolower(res$message %||% ""))) break
  }
  list(ok = FALSE, message = paste(msgs, collapse = " | "), raw = paste(raws, collapse = "\n\n"), action = NA_character_)
}

append_rows <- function(rows_list) {
  # Try likely Apps Script routes
  actions <- c("append_rows", "append_anthro", "append", "save_anthro", "save")
  .try_actions(list(rows = rows_list), actions)
}

replace_rows_for_key <- function(athlete, date, rows_list) {
  actions <- c("replace_rows_for_key", "replace_anthro", "overwrite_anthro", "upsert_anthro")
  .try_actions(list(athlete = athlete, date = date, rows = rows_list), actions)
}

load_practitioners <- function() {
  res <- try(api_get(list(action = "practitioners", cb = as.integer(Sys.time()))), silent = TRUE)
  if (!inherits(res, "try-error") && isTRUE(res$ok) && length(res$data) > 0) {
    return(unlist(res$data))
  }
  character()
}

load_athletes <- function() {
  res <- try(api_get(list(action = "athletes", cb = as.integer(Sys.time()))), silent = TRUE)
  if (!inherits(res, "try-error") && isTRUE(res$ok) && length(res$data) > 0) {
    return(unlist(res$data))
  }
  character()
}

fetch_recent <- function(limit = 10) {
  res <- try(api_get(list(action = "recent_anthro", limit = limit, cb = as.integer(Sys.time()))), silent = TRUE)
  if (inherits(res, "try-error") || !isTRUE(res$ok) || length(res$data) == 0) {
    return(tibble::tibble(date = character(), athlete = character()))
  }
  # tolerant parse + normalize
  out <- rows_list_to_tibble(res$data) |> fix_ad_cols()
  if (!all(c("date","athlete") %in% names(out))) {
    return(tibble::tibble(date = character(), athlete = character()))
  }
  dplyr::select(out, date, athlete) |> dplyr::distinct()
}

# ---- Do we have more than just athlete/date? ----
has_measure_cols <- function(df) {
  if (!is.data.frame(df) || ncol(df) <= 2) return(FALSE)
  nm <- names(df)
  any(grepl("(_m1|_m2|_m3|_value|_suggest)$", nm)) ||
    any(nm %in% c("height_cm_m1","weight_kg_m1","usg","caliper","fasted","birth_control","creatine"))
}

# ---- History from cache ONLY (no API) ----

# ---- TitleCase aliases so older Rmds still work ----
make_title_aliases <- function(d) {
  nm <- names(d)
  for (nm0 in nm) {
    alias <- paste0(toupper(substr(nm0, 1, 1)), substring(nm0, 2))
    if (!identical(alias, nm0) && !(alias %in% nm)) d[[alias]] <- d[[nm0]]
  }
  d
}

# ---- Prefer PDF if TeX exists, else HTML (Connect fallback) ----
choose_output_format <- function() {
  if (requireNamespace("tinytex", quietly = TRUE)) {
    ok <- FALSE
    try({ ok <- tinytex::is_tinytex() || tinytex::is_tlmgr_ready() }, silent = TRUE)
    if (isTRUE(ok)) return("pdf_document")
  }
  "html_document"
}

# ================== CHOICES / MEASURES ==================
day_of_cycle_choices <- c("Not Tracking", as.character(1:35))
caliper_choices <- c(
  "VIC_SIR508","VIC_SIR619","VIC_VDH7033","VIC_ZAB634",
  "VAN_P1697","VAN_YDH7487","VAN_SHD097","WHI_SIR483"
)
scale_choices <- c("CSI Lab","Personal","Athlete's Training Center","Force Plate")
yes_no <- c("Yes","No")

measures_list <- list(
  "Height and Weight" = c(
    "Height (cm)",
    "Sitting Height (cm)",
    "Armspan (cm)",
    "Weight (kg)"
  ),
  "Skinfolds" = c(
    "Triceps (mm)",
    "Subscap (mm)",
    "Biceps (mm)",
    "Illiac (mm)",
    "Supraspinale (mm)",
    "Abdomen (mm)",
    "R Thigh (mm)",
    "R Calf (mm)",
    "L Thigh (mm)",
    "L Calf (mm)"
  ),
  "Girth" = c(
    "R Relaxed Bicep (cm)",
    "R Flexed Bicep (cm)",
    "L Relaxed Bicep (cm)",
    "L Flexed Bicep (cm)",
    "R Forearm (cm)",
    "L Forearm (cm)",
    "Waist (cm)",
    "Hips (cm)",
    "R Mid Thigh (cm)",
    "R Medial Calf (cm)",
    "L Mid Thigh (cm)",
    "L Medial Calf (cm)"
  )
)

measure_key <- list(
  "Height (cm)" = "height_cm",
  "Sitting Height (cm)" = "sitting_height_cm",
  "Armspan (cm)" = "armspan_cm",
  "Weight (kg)" = "weight_kg",
  "Triceps (mm)" = "triceps_mm",
  "Subscap (mm)" = "subscap_mm",
  "Biceps (mm)" = "biceps_mm",
  "Illiac (mm)" = "illiac_mm",
  "Supraspinale (mm)" = "supraspinale_mm",
  "Abdomen (mm)" = "abdomen_mm",
  "R Thigh (mm)" = "r_thigh_mm",
  "R Calf (mm)" = "r_calf_mm",
  "L Thigh (mm)" = "l_thigh_mm",
  "L Calf (mm)" = "l_calf_mm",
  "R Relaxed Bicep (cm)" = "r_relaxed_bicep_cm",
  "R Flexed Bicep (cm)" = "r_flexed_bicep_cm",
  "L Relaxed Bicep (cm)" = "l_relaxed_bicep_cm",
  "L Flexed Bicep (cm)" = "l_flexed_bicep_cm",
  "R Forearm (cm)" = "r_forearm_cm",
  "L Forearm (cm)" = "l_forearm_cm",
  "Waist (cm)" = "waist_cm",
  "Hips (cm)" = "hips_cm",
  "R Mid Thigh (cm)" = "r_mid_thigh_cm",
  "R Medial Calf (cm)" = "r_medial_calf_cm",
  "L Mid Thigh (cm)" = "l_mid_thigh_cm",
  "L Medial Calf (cm)" = "l_medial_calf_cm"
)

threshold_for <- function(category) ifelse(category == "Skinfolds", 5, 1)

bounds_for <- function(category, item) {
  if (category == "Girth") return(c(15, 200))
  if (category == "Skinfolds") return(c(1.5, 50))
  if (category == "Height and Weight") {
    if (grepl("Weight", item, fixed = TRUE)) return(c(30, 150))
    if (grepl("Height", item, ignore.case = TRUE) || grepl("Armspan", item, ignore.case = TRUE)) return(c(45, 300))
  }
  return(c(0, 210))
}

suggest_msg <- function(v1, v2, thresh) {
  if (is.na(v1) || is.na(v2)) return(NA_character_)
  avg <- mean(c(v1, v2))
  if (is.na(avg) || avg == 0) return(NA_character_)
  pct <- abs(v1 - v2) / avg * 100
  if (pct > thresh) glue("Yes — diff {round(pct,2)}% (> {thresh}%)") else "No"
}

calc_value <- function(v1, v2, v3, suggest_txt) {
  if (is.na(v1) || is.na(v2)) return(NA_real_)
  if (!is.character(suggest_txt)) return(NA_real_)
  if (startsWith(tolower(suggest_txt), "yes")) {
    if (is.na(v3)) return(NA_real_)
    return(stats::median(c(v1, v2, v3), na.rm = TRUE))
  } else {
    return(mean(c(v1, v2)))
  }
}

# ================== BRAND HEADER ==================
brand_header <- function() {
  tags$div(
    style = "background:#000; color:#fff; padding:14px 18px;",
    fluidRow(
      column(
        8,
        tags$span(
          style="font-weight:900; color:#fff; font-size:40px; letter-spacing:0.4px;",
          "CSI Pacific Anthropometry"
        )
      ),
      column(
        4, align = "right",
        tags$img(src = "csi-pacific.png", height = "120px", style = "filter:none;")
      )
    )
  )
}

# ================== UI ==================
ui <- tagList(
  useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app.css")),
  brand_header(),
  navbarPage(
    title = NULL,
    id = "main_nav",
    tabPanel(
      title = "Data Entry",
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectizeInput("practitioner", "Practitioner",
                           choices = NULL,
                           options = list(placeholder = "Select From List", 
                                          create = TRUE,
                                          sortField = list(list(field = "text", direction = "asc"))
                                          )),
            selectizeInput("athlete", "Athlete name",
                           choices = NULL,
                           options = list(placeholder = "Select From List", 
                                          create = FALSE,
                                          sortField = list(list(field = "text", direction = "asc"))
                                          )),
            dateInput("date", "Collection Date", value = Sys.Date()),
            shinyTime::timeInput("time", "Collection Time",
                                 value = strptime(format(Sys.time(), "%I:%M %p"), "%I:%M %p"),
                                 seconds = FALSE),
            pickerInput("scale", "Scale", choices = scale_choices, multiple = FALSE,
                        options = pickerOptions(style = "btn-outline-primary")),
            hr(),
            actionButton("prefill_btn", "Prefill Athlete's Recent Data",
                         class = "btn-outline-secondary", width = "100%"),
            br(), br(),
            actionButton("submit", "Submit & Save", class = "btn-primary", width = "100%"),
            br(),
            h4("Batch upload"),
            fileInput("batch_file", "Excel (.xlsx) file only", accept = c(".xlsx"),
                      buttonLabel = "Browse..."),
            actionButton("upload_btn", "Upload to Database", class = "btn-warning", width = "100%"),
            br(),
            textOutput("upload_status"),
            br(),
            verbatimTextOutput("status"),
            h4("Last 10 entries"),
            # verbatimTextOutput("db_debug"),
            # verbatimTextOutput("idx_debug"),
            # verbatimTextOutput("db_head"),
            DTOutput("recent_tbl"),
            br()
          ),
          mainPanel(
            width = 9,
            div(class = "card",
                h3("Session & Context"),
                fluidRow(
                  column(3, prettyRadioButtons("fasted","Fasted", choices = yes_no, animation = "jelly", inline = TRUE, status = "primary")),
                  column(3, pickerInput("doc","Day of Cycle", choices = day_of_cycle_choices, multiple = FALSE)),
                  column(3, prettyRadioButtons("bc","Birth Control", choices = yes_no, animation = "jelly", inline = TRUE)),
                  column(3, prettyRadioButtons("creatine","Creatine", choices = yes_no, animation = "jelly", inline = TRUE))
                ),
                fluidRow(
                  column(4, div(numericInput("usg","Urine Specific Gravity", value = NA, min = 0.950, max = 1.080, step = 0.001),
                                uiOutput("usg_warn"))),
                  column(4, pickerInput("caliper","Caliper", choices = caliper_choices, multiple = FALSE))
                )
            ),
            div(class = "card",
                h3("Measures"),
                uiOutput("measures_ui"),
                br(),
                textAreaInput("comments","Comments / Notes", width = "100%", height = "100px")
            )
          )
        )
      )
    ),
    tabPanel(
      title = "Report",
      fluidPage(
        fluidRow(
          column(4, actionButton("report_reload", "Reload list", class = "btn-outline-secondary", width = "100%")),
          column(4, actionButton("report_select_all", "Select all", class = "btn-outline-secondary", width = "100%")),
          column(4, actionButton("report_select_none", "Select none", class = "btn-outline-secondary", width = "100%"))
        ),
        br(),
        div(id="report-wrapper", DTOutput("report_tbl")),
        tags$script(HTML("
          $(document).on('change', 'input[type=checkbox][id^=sel_]', function(){
            var ids = $('input[type=checkbox][id^=sel_]:checked').map(function(){return this.id;}).get();
            Shiny.setInputValue('report_selected', ids, {priority:'event'});
          });
        ")),
        br(),
        fluidRow(
          column(4, downloadButton("report_print_zip", "Print Report(s)")),
          column(4, downloadButton("report_csv", "Download CSV")),
          column(4)
        ),
        br(),
        textOutput("report_status")
      )
    ),
    tabPanel(
      title = "Athlete Dashboard",
      fluidPage(
        h3("Athlete dashboard (coming next)"),
        p("This tab will visualize individual athlete trends and histories.")
      )
    )
  )
)

# ================== SERVER ==================
server <- function(input, output, session) {
  # ---------- Full DB cache (read-once model) ----------
  .DB <- reactiveVal(tibble::tibble())      # all session rows
  .DB_meta <- reactiveVal(list(last_loaded = NA))
  .session_cache <- cache_mem(max_age = 3600) # per-session fetch cache during hydrate
  .IDX <- reactiveVal(tibble::tibble())  # holds the recent_anthro index used for hydration
  
  fetch_history_cache_only <- function(ath) {
    db <- .DB()
    if (!is.data.frame(db) || nrow(db) == 0) return(tibble::tibble())
    target <- tolower(trimws(ath))
    out <- db %>%
      dplyr::filter(tolower(trimws(athlete)) == target) %>%
      tibble::as_tibble()
    if (nrow(out)) {
      out <- jsonlite::fromJSON(jsonlite::toJSON(out, auto_unbox = TRUE, null = "null"), flatten = TRUE) |>
        tibble::as_tibble() |>
        normalize_cols()
    }
    out
  }
  
  # Pull a single FULL session row; use API if cache row is skinny
  fetch_full_session_row <- function(a, d) {
    a <- trimws(a); d <- trimws(d)
    db <- .DB()
    if (is.data.frame(db) && nrow(db) > 0) {
      hit <- db %>%
        dplyr::filter(tolower(trimws(athlete)) == tolower(a),
                      as.character(date) == as.character(d)) %>%
        dplyr::slice(1)
      if (is.data.frame(hit) && nrow(hit) == 1 && has_measure_cols(hit)) {
        return(hit)
      }
    }
    got <- try(api_get(list(action = "get_anthro", athlete = a, date = d,
                            cb = as.integer(Sys.time()))), silent = TRUE)
    if (!inherits(got, "try-error") && isTRUE(got$ok) && length(got$data) > 0) {
      dat <- try(tibble::as_tibble(jsonlite::fromJSON(jsonlite::toJSON(got$data), flatten = TRUE)),
                 silent = TRUE)
      if (!inherits(dat, "try-error") && nrow(dat) > 0) {
        dat <- normalize_cols(dat)
        return(dat[1, ])
      }
    }
    tibble::tibble()
  }
  
  # Dates for athlete from the index you hydrated
  athlete_dates_from_index <- function(ath) {
    idx <- try(.IDX(), silent = TRUE)
    if (inherits(idx, "try-error") || !is.data.frame(idx) || nrow(idx) == 0) return(character(0))
    idx %>%
      dplyr::filter(tolower(trimws(athlete)) == tolower(trimws(ath))) %>%
      dplyr::distinct(date) %>%
      dplyr::arrange(dplyr::desc(as.character(date))) %>%
      dplyr::pull(date) %>%
      as.character()
  }
  
  # Build full history for an athlete (index -> per-date full rows)
  fetch_history_hybrid <- function(ath) {
    dates <- athlete_dates_from_index(ath)
    if (length(dates) == 0) {
      db <- .DB()
      if (is.data.frame(db) && nrow(db) > 0) {
        return(db %>%
                 dplyr::filter(tolower(trimws(athlete)) == tolower(trimws(ath))) %>%
                 tibble::as_tibble())
      }
      return(tibble::tibble())
    }
    rows <- lapply(dates, function(d) fetch_full_session_row(ath, d))
    out <- safe_bind_rows(rows)
    if (nrow(out) > 0) {
      out <- tibble::as_tibble(
        jsonlite::fromJSON(jsonlite::toJSON(out, auto_unbox = TRUE, null = "null"), flatten = TRUE),
        .name_repair = "universal"
      )
      out <- normalize_cols(out)
    }
    out
  }
  
  # Create TitleCase aliases so older Rmds can use either name form
  make_title_aliases <- function(d) {
    nm <- names(d)
    for (nm0 in nm) {
      alias <- paste0(toupper(substr(nm0, 1, 1)), substring(nm0, 2))
      if (!identical(alias, nm0) && !(alias %in% nm)) d[[alias]] <- d[[nm0]]
    }
    d
  }
  
  # Choose output format (PDF when TeX available; HTML fallback on Connect)
  choose_output_format <- function() {
    if (requireNamespace("tinytex", quietly = TRUE)) {
      ok <- FALSE
      try({ ok <- tinytex::is_tinytex() || tinytex::is_tlmgr_ready() }, silent = TRUE)
      if (isTRUE(ok)) return("pdf_document")
    }
    "html_document"
  }
  
  # helper to make a cache-safe key
  cache_key <- function(ath, d) {
    a <- tolower(gsub("[^a-z0-9]+", "", ath))
    dd <- tolower(gsub("[^a-z0-9]+", "", d))
    paste0(a, "_", dd)
  }
  
  hydrate_full_db <- function(limit = 5000) {
    withProgress(message = "Downloading database…", value = 0, {
      incProgress(0.1, detail = "Fetching index")
      idx <- fetch_recent(limit) |> fix_ad_cols()
      
      # Save index for Recent/Report immediately so UI has data even if enrichment fails
      if (is.data.frame(idx) && nrow(idx) > 0 && all(c("athlete","date") %in% names(idx))) {
        idx2 <- idx %>%
          dplyr::distinct(athlete, date) %>%
          dplyr::arrange(dplyr::desc(as.Date(as.character(date))))
        .IDX(idx2)
        .DB(idx2)  # temporary seed; will be replaced if enrichment succeeds
        .DB_meta(list(last_loaded = Sys.time()))
      } else {
        .IDX(tibble::tibble())
        .DB(tibble::tibble())
        .DB_meta(list(last_loaded = Sys.time()))
        showNotification("Index from API didn’t include 'athlete' and 'date' (or was empty).",
                         type = "error", duration = 6)
        return(invisible(NULL))
      }
      
      # 3.2 Enrich (fetch full session rows) — no futures, keep Connect-friendly
      incProgress(0.25, detail = "Fetching session rows")
      n <- nrow(idx)
      if (n == 0) { .DB(tibble()); .DB_meta(list(last_loaded = Sys.time())); return(invisible(NULL)) }
      
      rows <- vector("list", n)
      fetched <- 0
      chunks <- split(seq_len(n), ceiling(seq_len(n) / 25))
      for (ch in chunks) {
        res <- lapply(ch, function(i) {
          a <- as.character(idx$athlete[i]); d <- as.character(idx$date[i])
          get_session_row(a, d)  # returns tibble() on failure
        })
        rows[ch] <- res
        fetched <- fetched + length(ch)
        incProgress(0.25 + 0.7 * (fetched / n), detail = sprintf("Sessions %d/%d", fetched, n))
      }
      
      out <- safe_bind_rows(rows)
      if (is.data.frame(out) && nrow(out) > 0) {
        out <- jsonlite::fromJSON(jsonlite::toJSON(out, auto_unbox = TRUE, null = "null"), flatten = TRUE) |>
          tibble::as_tibble() |>
          normalize_cols()
        .DB(out)
      } else {
        .DB(tibble::tibble())  # keep index-only; tables still render from .IDX
        # showNotification("Could not enrich sessions; showing index list only.", type = "warning", duration = 1)
      }
      
      .DB_meta(list(last_loaded = Sys.time()))
      incProgress(1, detail = "Done")
    })
  }
  
  # ---------- Startup ----------
  session$onFlushed(function() {
    withProgress(message = 'Loading dashboard…', value = 0, {
      incProgress(0.2, detail = 'Loading database')
      hydrate_full_db(limit = 5000)
      
      incProgress(0.7, detail = 'Loading dropdowns')
      updateSelectizeInput(session, "practitioner", 
                           choices = alpha_sort(load_practitioners()), 
                           selected = character(0), server = TRUE)
      updateSelectizeInput(session, "athlete", 
                           choices = alpha_sort(load_athletes()), 
                           selected = character(0), server = TRUE)
      incProgress(1, detail = 'Ready')
    })
  }, once = TRUE)
  
  # USG inline warning
  output$usg_warn <- renderUI({
    v <- input$usg
    if (is.null(v) || is.na(v)) return(span(class="muted",""))
    if (v < 0.950 || v > 1.080) span(class="warn","Warning: USG out of range (expected 0.950–1.080).") else span(class="muted","")
  })
  
  # ---------- Recent list (from cache) ----------
  recent_from_cache <- reactive({
    db  <- .DB()
    idx <- .IDX()
    
    # normalize types BEFORE bind_rows
    src <- dplyr::bind_rows(
      fix_ad_cols(safe_tibble(db))  %>% dplyr::select(dplyr::any_of(c("date","athlete","timestamp"))),
      fix_ad_cols(safe_tibble(idx)) %>% dplyr::select(dplyr::any_of(c("date","athlete","timestamp")))
    )
    
    if (!is.data.frame(src) || nrow(src) == 0) {
      return(tibble::tibble(date = character(), athlete = character()))
    }
    
    has_ts <- "timestamp" %in% names(src)
    
    src %>%
      dplyr::mutate(
        date    = as.character(.data$date),
        athlete = as.character(.data$athlete),
        ts      = if (has_ts) as.character(.data$timestamp) else as.character(.data$date),
        date_parsed = suppressWarnings(as.Date(.data$date))
      ) %>%
      dplyr::distinct(.data$date, .data$athlete, .keep_all = TRUE) %>%
      dplyr::arrange(dplyr::desc(.data$ts), dplyr::desc(.data$date_parsed)) %>%
      dplyr::slice_head(n = 10) %>%
      dplyr::select(date, athlete)
  })
  
  output$recent_tbl <- renderDT({
    df2 <- recent_from_cache() %>% select(date, athlete) %>% rename(Date = date, Athlete = athlete)
    if (nrow(df2) == 0) {
      datatable(data.frame(Date = character(), Athlete = character()),
                options = list(dom = 't', paging = FALSE), rownames = FALSE)
    } else {
      datatable(df2,
                options = list(dom = 't', paging = FALSE,
                               columnDefs = list(list(width = "60%", targets = 0),
                                                 list(width = "40%", targets = 1))),
                rownames = FALSE)
    }
  })
  

  # ---------- Measures UI ----------
  # --- validate only when the user leaves the field (on blur) ---
  register_blur_guard <- function(id, lo, hi, label = "Value") {
    blur_id <- paste0(id, "_blur")
    
    # When the JS blur event fires, we receive the committed value here:
    observeEvent(input[[blur_id]], {
      v <- suppressWarnings(as.numeric(input[[blur_id]]))
      if (!is.null(v) && !is.na(v) && (v < lo || v > hi)) {
        showNotification(glue("{label} must be between {lo} and {hi}."), type = "error", duration = 3)
        updateNumericInput(session, id, value = NA)
      } else {
        # Keep exactly what the user committed (coerced to numeric)
        if (!is.na(v)) updateNumericInput(session, id, value = v)
      }
    }, ignoreInit = TRUE)
    
    # Attach a one-time JS listener to the numericInput's <input> element
    shinyjs::runjs(sprintf("
    $(document).off('blur', '#%s input');      // avoid duplicate bindings
    $(document).on('blur', '#%s input', function(){
      var val = $(this).val();
      Shiny.setInputValue('%s', val, {priority:'event'});
    });
  ", id, id, blur_id))
  }
  
  build_row <- function(category, item, idx) {
    base <- paste0(safe_id(category), "_", safe_id(item), "_", idx)
    id1 <- paste0("m1_", base)
    id2 <- paste0("m2_", base)
    id3 <- paste0("m3_", base)
    idsugg <- paste0("sugg_", base)
    idval <- paste0("val_", base)
    
    bnds <- bounds_for(category, item)
    lo <- bnds[1]; hi <- bnds[2]
    
    output[[idsugg]] <- renderUI({
      v1 <- input[[id1]]; v2 <- input[[id2]]
      th <- threshold_for(category)
      msg <- suggest_msg(v1, v2, th)
      if (is.na(msg)) span(class="muted","—") else span(msg)
    })
    
    output[[idval]] <- renderUI({
      v1 <- input[[id1]]; v2 <- input[[id2]]; v3 <- input[[id3]]
      th <- threshold_for(category); msg <- suggest_msg(v1, v2, th)
      vv <- calc_value(v1, v2, v3, msg)
      if (is.na(vv)) span(class="muted","—") else span(class="value-box", sprintf("%.2f", vv))
    })
    
    register_blur_guard(id1, lo, hi, "Measure 1")
    register_blur_guard(id2, lo, hi, "Measure 2")
    register_blur_guard(id3, lo, hi, "Measure 3")
    
    div(class="row-line measure-row",
        div(class="seg-name", div(style="padding-top:6px;", cap_first(item))),
        div(class="seg-in",   div(class="mini-num", numericInput(id1, "1", value = NA, min = lo, max = hi, step = 0.1, width = "100%"))),
        div(class="seg-in",   div(class="mini-num", numericInput(id2, "2", value = NA, min = lo, max = hi, step = 0.1, width = "100%"))),
        div(class="seg-in",   div(class="mini-num", numericInput(id3, "3", value = NA, min = lo, max = hi, step = 0.1, width = "100%"))),
        div(class="seg-sugg", div(tags$label("3rd Measure?"), uiOutput(idsugg))),
        div(class="seg-val",  div(tags$label("Value"), uiOutput(idval)))
    )
  }
  
  output$measures_ui <- renderUI({
    tagList(
      lapply(names(measures_list), function(cat) {
        items <- measures_list[[cat]]
        tagList(
          div(class="row-head", h4(cat)),
          lapply(seq_along(items), function(i) build_row(cat, items[[i]], i))
        )
      })
    )
  })
  
  # ---------- Prefill (from cache) ----------
  clear_form <- function() {
    updateSelectizeInput(session, "practitioner", selected = "")
    try(shinyTime::updateTimeInput(session, "time",
                                   value = strptime(format(Sys.time(), "%I:%M %p"), "%I:%M %p")), silent = TRUE)
    updatePickerInput(session, "scale", selected = character(0))
    updatePrettyRadioButtons(session, "fasted", selected = character(0))
    updatePickerInput(session, "doc", selected = "Not Tracking")
    updatePrettyRadioButtons(session, "bc", selected = character(0))
    updatePrettyRadioButtons(session, "creatine", selected = character(0))
    updateNumericInput(session, "usg", value = NA)
    updatePickerInput(session, "caliper", selected = character(0))
    updateTextAreaInput(session, "comments", value = "")
    for (cat in names(measures_list)) {
      items <- measures_list[[cat]]
      for (i in seq_along(items)) {
        meas <- items[[i]]
        base <- paste0(safe_id(cat), "_", safe_id(meas), "_", i)
        id1 <- paste0("m1_", base)
        id2 <- paste0("m2_", base)
        id3 <- paste0("m3_", base)
        if (!is.null(input[[id1]])) updateNumericInput(session, id1, value = NA)
        if (!is.null(input[[id2]])) updateNumericInput(session, id2, value = NA)
        if (!is.null(input[[id3]])) updateNumericInput(session, id3, value = NA)
      }
    }
  }
  
  observeEvent(input$prefill_btn, {
    withProgress(message = 'Prefilling…', value = 0, {
      req(input$athlete)
      ath <- trimws(input$athlete)
      
      # 1) figure out most-recent date for this athlete from the hydrated index
      dates <- athlete_dates_from_index(ath)
      if (length(dates) == 0) {
        showNotification("No cached sessions found for this athlete. Try Reload list.", type = "warning", duration = 5)
        return(invisible(NULL))
      }
      # dates already sorted desc in athlete_dates_from_index()
      d_latest <- as.character(dates[[1]])
      
      incProgress(0.4, detail = sprintf("Fetching %s (%s)…", ath, d_latest))
      
      # 2) pull a FULL row (uses cache first; falls back to API)
      row <- fetch_full_session_row(ath, d_latest)
      if (!is.data.frame(row) || nrow(row) == 0) {
        showNotification("Could not load the latest session details.", type = "error", duration = 5)
        return(invisible(NULL))
      }
      row <- row[1, , drop = FALSE]       # just in case
      row <- normalize_cols(row)
      
      # 3) fill the simple fields
      if ("practitioner" %in% names(row)) updateSelectizeInput(session, "practitioner", selected = row$practitioner %||% "")
      updateSelectizeInput(session, "athlete", selected = ath)
      
      if ("date" %in% names(row)) {
        d_try <- suppressWarnings(lubridate::ymd(row$date))
        if (is.na(d_try)) d_try <- suppressWarnings(lubridate::ymd_hms(row$date))
        if (!is.na(d_try)) updateDateInput(session, "date", value = as.Date(d_try))
      }
      if ("time" %in% names(row)) {
        t_try <- suppressWarnings(lubridate::ymd_hms(row$time))
        if (!is.na(t_try)) shinyTime::updateTimeInput(session, "time",
                                                      value = strptime(format(t_try, "%I:%M %p"), "%I:%M %p"))
      }
      if ("scale" %in% names(row))        updatePickerInput(session, "scale", selected = row$scale %||% "")
      if ("fasted" %in% names(row))       updatePrettyRadioButtons(session, "fasted", selected = row$fasted %||% "")
      if ("day_of_cycle" %in% names(row)) updatePickerInput(session, "doc", selected = as.character(row$day_of_cycle %||% "Not Tracking"))
      if ("birth_control" %in% names(row))updatePrettyRadioButtons(session, "bc", selected = row$birth_control %||% "")
      if ("creatine" %in% names(row))     updatePrettyRadioButtons(session, "creatine", selected = row$creatine %||% "")
      if ("usg" %in% names(row))          updateNumericInput(session, "usg", value = suppressWarnings(as.numeric(row$usg)))
      if ("caliper" %in% names(row))      updatePickerInput(session, "caliper", selected = row$caliper %||% "")
      if ("comments" %in% names(row))     updateTextAreaInput(session, "comments", value = row$comments %||% "")
      
      # 4) fill the measure triplicates
      for (cat in names(measures_list)) {
        items <- measures_list[[cat]]
        for (i in seq_along(items)) {
          meas <- items[[i]]
          base <- paste0(safe_id(cat), "_", safe_id(meas), "_", i)
          id1 <- paste0("m1_", base)
          id2 <- paste0("m2_", base)
          id3 <- paste0("m3_", base)
          
          key <- measure_key[[meas]]
          if (is.null(key)) next
          
          m1_col <- paste0(key, "_m1")
          m2_col <- paste0(key, "_m2")
          m3_col <- paste0(key, "_m3")
          
          v1 <- if (m1_col %in% names(row)) suppressWarnings(as.numeric(row[[m1_col]])) else NA_real_
          v2 <- if (m2_col %in% names(row)) suppressWarnings(as.numeric(row[[m2_col]])) else NA_real_
          v3 <- if (m3_col %in% names(row)) suppressWarnings(as.numeric(row[[m3_col]])) else NA_real_
          
          # enforce bounds before updating
          bnds <- bounds_for(cat, meas); lo <- bnds[1]; hi <- bnds[2]
          if (!is.na(v1) && (v1 < lo || v1 > hi)) v1 <- NA_real_
          if (!is.na(v2) && (v2 < lo || v2 > hi)) v2 <- NA_real_
          if (!is.na(v3) && (v3 < lo || v3 > hi)) v3 <- NA_real_
          
          if (!is.null(input[[id1]])) updateNumericInput(session, id1, value = v1)
          if (!is.null(input[[id2]])) updateNumericInput(session, id2, value = v2)
          if (!is.null(input[[id3]])) updateNumericInput(session, id3, value = v3)
        }
      }
      
      incProgress(1, detail = 'Done')
    })
    showNotification(glue("Prefilled latest session for {input$athlete}."), type="message", duration=5)
  })
  
  # ---------- Assemble wide row ----------
  assemble_wide <- function() {
    req(input$athlete, input$date)
    row <- list(
      timestamp      = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      practitioner   = input$practitioner %||% "",
      athlete        = input$athlete %||% "",
      date           = as.character(input$date %||% ""),
      time           = as.character(strptime(format(input$time, "%I:%M %p"), "%I:%M %p")),
      scale          = input$scale %||% "",
      fasted         = input$fasted %||% "",
      day_of_cycle   = if (!is.null(input$doc) && nzchar(input$doc)) input$doc else "Not Tracking",
      birth_control  = input$bc %||% "",
      creatine       = input$creatine %||% "",
      usg            = if (!is.null(input$usg)) round(input$usg, 3) else NA_real_,
      caliper        = input$caliper %||% "",
      comments       = input$comments %||% ""
    )
    for (cat in names(measures_list)) {
      items <- measures_list[[cat]]
      for (i in seq_along(items)) {
        meas <- items[[i]]
        base <- paste0(safe_id(cat), "_", safe_id(meas), "_", i)
        id1 <- paste0("m1_", base)
        id2 <- paste0("m2_", base)
        id3 <- paste0("m3_", base)
        v1 <- input[[id1]]; v2 <- input[[id2]]; v3 <- input[[id3]]
        key <- measure_key[[meas]]
        if (is.null(key)) next
        th <- threshold_for(cat)
        msg <- suggest_msg(v1, v2, th)
        val <- calc_value(v1, v2, v3, msg)
        row[[paste0(key,"_m1")]] <- ifelse(is.null(v1) || is.na(v1), NA, v1)
        row[[paste0(key,"_m2")]] <- ifelse(is.null(v2) || is.na(v2), NA, v2)
        row[[paste0(key,"_m3")]] <- ifelse(is.null(v3) || is.na(v3), NA, v3)
        row[[paste0(key,"_suggest")]] <- ifelse(is.na(msg), "", msg)
        row[[paste0(key,"_value")]] <- ifelse(is.na(val), "", round(val, 2))
      }
    }
    row
  }
  
  # ---------- Save with duplicate detection ----------
  output$status <- renderText("")
  pending_row <- reactiveVal(NULL)
  
  observeEvent(input$submit, {
    errs <- c()
    if (is.null(input$athlete) || !nzchar(trimws(input$athlete))) errs <- c(errs, "Athlete is required.")
    if (is.null(input$date)) errs <- c(errs, "Collection Date is required.")
    if (!is.null(input$usg) && !is.na(input$usg) && (input$usg < 0.950 || input$usg > 1.080)) {
      errs <- c(errs, "USG must be between 0.950 and 1.080.")
    }
    out <- assemble_wide()
    if (length(errs) > 0) {
      output$status <- renderText(paste(errs, collapse = "\n"))
      return(NULL)
    }
    
    # --- Duplicate check (robust) ---
    db_now <- fix_ad_cols(.DB())
    
    # always coerce the outgoing date to a comparable character
    out_date_chr <- as.character(out$date %||% "")
    
    dup_exists <- FALSE
    if (is.data.frame(db_now) && nrow(db_now) > 0) {
      # use .data pronoun so we definitely hit columns, not functions
      dup_exists <- nrow(
        dplyr::filter(
          db_now,
          tolower(trimws(.data$athlete)) == tolower(trimws(out$athlete)),
          as.character(.data$date) == out_date_chr
        )
      ) > 0
    }
    
    if (dup_exists) {
      pending_row(out)
      showModal(modalDialog(
        title = "Duplicate found",
        size = "m",
        easyClose = FALSE,
        footer = tagList(
          actionButton("confirm_replace", "Overwrite existing record", class = "btn-danger"),
          modalButton("Cancel")
        ),
        div(
          p("An entry already exists for:"),
          tags$ul(
            tags$li(glue("Athlete: {out$athlete}")),
            tags$li(glue("Collection Date: {out$date}"))
          ),
          p("Do you want to overwrite the existing record with your current data?")
        )
      ))
      return(invisible(NULL))
    }
    
    res <- append_rows(list(out))
    if (isTRUE(res$ok)) {
      # (unchanged) update .DB, .IDX, status text
      new_row <- tibble::as_tibble(jsonlite::fromJSON(jsonlite::toJSON(out), flatten = TRUE))
      new_row <- normalize_cols(new_row)
      db_now <- .DB()
      .DB(bind_rows(new_row, db_now))
      idx_now <- .IDX()
      new_idx <- tibble::tibble(athlete = as.character(out$athlete), date = as.character(out$date))
      .IDX(dplyr::bind_rows(new_idx, safe_tibble(idx_now)) %>% dplyr::distinct(athlete, date) %>% dplyr::arrange(dplyr::desc(as.Date(as.character(date)))))
      output$status <- renderText("Saved to database")
    } else {
      # show message from server/transport
      msg <- paste("Save failed:", res$message %||% "<no message>")
      output$status <- renderText(msg)
      # also log full raw body to R console for debugging
      cat("\n[append_rows] RAW response:\n", res$raw %||% "<no raw>", "\n")
    }
  })
  
  observeEvent(input$confirm_replace, {
    req(!is.null(pending_row()))
    out <- pending_row()
    out_date_chr <- as.character(out$date %||% "")
    removeModal()
    
    res <- replace_rows_for_key(out$athlete, out$date, list(out))
    if (isTRUE(res$ok)) {
      db_now <- .DB()
      db_now2 <- db_now %>%
        dplyr::filter(!(tolower(trimws(athlete)) == tolower(trimws(out$athlete)) &
                          as.character(.data$date) == out_date_chr))
      new_row <- tibble::as_tibble(jsonlite::fromJSON(jsonlite::toJSON(out), flatten = TRUE))
      new_row <- normalize_cols(new_row)
      .DB(bind_rows(new_row, db_now2))
      idx_now <- .IDX()
      new_idx <- tibble::tibble(
        athlete = as.character(out$athlete),
        date    = as.character(out$date)
      )
      .IDX(
        dplyr::bind_rows(new_idx, safe_tibble(idx_now)) %>%
          dplyr::distinct(athlete, date) %>%
          dplyr::arrange(dplyr::desc(as.Date(as.character(date))))
      )
      output$status <- renderText("Existing record overwritten and saved to database")
      pending_row(NULL)
    } else {
      output$status <- renderText(paste("Overwrite failed:", res$message %||% "<no message>"))
      cat("\n[replace_rows_for_key] RAW response:\n", res$raw %||% "<no raw>", "\n")
    }
  })
  
  # ---------- Batch upload (.xlsx) ----------
  output$upload_status <- renderText("")
  observeEvent(input$upload_btn, {
    req(input$batch_file)
    path <- input$batch_file$datapath
    withProgress(message = "Uploading batch…", value = 0, {
      incProgress(0.2, detail = "Reading Excel")
      df <- try(readxl::read_xlsx(path), silent = TRUE)
      if (inherits(df, "try-error")) {
        output$upload_status <- renderText("Error reading Excel file.")
        return(invisible(NULL))
      }
      incProgress(0.5, detail = "Normalizing")
      if (!"timestamp" %in% names(df)) df$timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
      rows <- jsonlite::fromJSON(jsonlite::toJSON(df, na = "null"), simplifyVector = FALSE)
      incProgress(0.8, detail = "Saving to Database")
      ok <- append_rows(rows)
      if (ok) {
        # Simple approach: re-hydrate to reflect batch upload
        hydrate_full_db(limit = 5000)
        output$upload_status <- renderText("Batch upload complete.")
      } else {
        output$upload_status <- renderText("Batch upload failed.")
      }
      incProgress(1, detail = "Done")
    })
  })
  
  # ================== REPORT TAB ==================
  # Use cache for report listing (most recent first)
  report_df <- reactive({
    idx <- .IDX()
    if (!is.data.frame(idx) || nrow(idx) == 0 || !all(c("date","athlete") %in% names(idx))) {
      return(tibble(date = character(), athlete = character(), timestamp = character()))
    }
    has_ts <- "timestamp" %in% names(idx)
    idx %>%
      transmute(
        date = as.character(date),
        athlete = as.character(athlete),
        timestamp = if (has_ts) as.character(timestamp) else as.character(date)
      ) %>%
      distinct() %>%
      arrange(desc(timestamp))
  })
  
  # Reload button: re-hydrate once
  observeEvent(input$report_reload, {
    hydrate_full_db(limit = 5000)
    showNotification("Database reloaded.", type = "message")
  })
  
  # Enable/disable Download buttons based on selection
  observe({
    ids <- input$report_selected
    if (is.null(ids) || length(ids) == 0) {
      shinyjs::disable('report_csv')
      shinyjs::disable('report_print_zip')
    } else {
      shinyjs::enable('report_csv')
      shinyjs::enable('report_print_zip')
    }
  })
  
  output$report_tbl <- renderDT({
    df <- report_df()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Select = character(), `Collection Date` = character(), Athlete = character()),
                       escape = FALSE, rownames = FALSE, options = list(dom = 't', paging = FALSE)))
    }
    df2 <- df %>% select(date, athlete) %>% rename(`Collection Date` = date, Athlete = athlete)
    n <- nrow(df2)
    checks <- vapply(seq_len(n), function(i) as.character(checkboxInput(paste0("sel_", i), label = NULL, value = FALSE)), character(1))
    out <- cbind(Select = checks, df2)
    datatable(out, escape = FALSE, rownames = FALSE, selection = "none",
              options = list(pageLength = 25, dom = 'tip', ordering = FALSE, autoWidth = FALSE))
  }, server = FALSE)
  
  observeEvent(input$report_select_all, {
    runjs("$('input[type=checkbox][id^=sel_]').prop('checked', true).trigger('change');var ids=$('input[type=checkbox][id^=sel_]:checked').map(function(){return this.id;}).get();Shiny.setInputValue('report_selected', ids, {priority:'event'});")
  })
  
  observeEvent(input$report_select_none, {
    runjs("$('input[type=checkbox][id^=sel_]').prop('checked', false).trigger('change');Shiny.setInputValue('report_selected', [], {priority:'event'});")
  })
  
  # ---- Local history helper (from cache) ----
  fetch_athlete_history <- function(ath) {
    dates <- athlete_dates_from_index(ath)
    if (length(dates) == 0) return(tibble())
    
    rows <- lapply(dates, function(d) {
      get_session_row(ath, d)  # already returns tibble() on failure
    })
    
    out <- safe_bind_rows(rows)
    if (nrow(out) > 0) {
      out <- tibble::as_tibble(
        jsonlite::fromJSON(jsonlite::toJSON(out, auto_unbox = TRUE, null = "null"), flatten = TRUE),
        .name_repair = "universal"
      )
    }
    out
  }

  # ---- Local history helper (from cache ONLY; no API calls) ----
  fetch_athlete_history_local <- function(ath) {
    db <- .DB()
    if (!is.data.frame(db) || nrow(db) == 0) return(tibble::tibble())
    target <- tolower(trimws(ath))
    out <- db %>%
      dplyr::filter(tolower(trimws(athlete)) == target) %>%
      dplyr::arrange(dplyr::desc(as.Date(as.character(date)))) %>%
      tibble::as_tibble()
    out
  }
  

  # cache key already sanitized elsewhere
  get_session_row <- function(ath, d) {
    key <- cache_key(ath, d)
    hit <- .session_cache$get(key)
    if (!is.null(hit)) return(hit)
    
    got <- try(api_get(list(action="get_anthro", athlete=ath, date=d, cb=as.integer(Sys.time()))),
               silent = TRUE)
    
    out <- tibble() # <- IMPORTANT: default to empty tibble, not NULL
    if (!inherits(got, "try-error") && isTRUE(got$ok) && length(got$data) > 0) {
      dat <- try(tibble::as_tibble(jsonlite::fromJSON(jsonlite::toJSON(got$data), flatten = TRUE)),
                 silent = TRUE)
      if (!inherits(dat, "try-error") && nrow(dat) > 0) out <- dat[1,]
    }
    .session_cache$set(key, out)
    out
  }
  
  output$report_print_zip <- downloadHandler(
    filename = function() paste0(format(Sys.Date(), "%Y-%m-%d"), "_anthro_report.zip"),
    contentType = "application/zip",
    content = function(file) {
      # --- selection sanity ---
      ids <- isolate(input$report_selected)
      if (is.null(ids) || length(ids) == 0) {
        showNotification("Please select at least one row to print.", type = "warning", duration = 4)
        stop("no-selection")
      }
      df <- report_df()
      n <- nrow(df)
      sel_idx <- suppressWarnings(as.integer(gsub("^sel_", "", ids)))
      sel_idx <- sel_idx[!is.na(sel_idx) & sel_idx >= 1 & sel_idx <= n]
      if (length(sel_idx) == 0) {
        showNotification("Please select at least one row to print.", type = "warning", duration = 4)
        stop("no-valid-index")
      }
      
      # --- template check ---
      if (!file.exists("anthro_report_PDF.Rmd")) {
        showNotification("Template 'anthro_report_PDF.Rmd' not found in app directory.", type = "error", duration = 8)
        stop("missing-template")
      }
      
      # --- helpers available inside server() ---
      choose_output_format <- function() {
        if (requireNamespace("tinytex", quietly = TRUE)) {
          ok <- FALSE
          try({ ok <- tinytex::is_tinytex() || tinytex::is_tlmgr_ready() }, silent = TRUE)
          if (isTRUE(ok)) return("pdf_document")
        }
        "html_document"
      }
      make_title_aliases <- function(d) {
        nm <- names(d)
        for (nm0 in nm) {
          alias <- paste0(toupper(substr(nm0, 1, 1)), substring(nm0, 2))
          if (!identical(alias, nm0) && !(alias %in% nm)) d[[alias]] <- d[[nm0]]
        }
        d
      }
      
      tmpdir <- tempfile("anthro_pdf_"); dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
      out_files <- character(0)
      
      withProgress(message = "Rendering reports…", value = 0, {
        total <- length(sel_idx)
        for (k in seq_along(sel_idx)) {
          i <- sel_idx[k]
          a <- as.character(df$athlete[i])
          d <- as.character(df$date[i])
          incProgress((k - 1) / max(1, total), detail = sprintf("%s — %s", d, a))
          
          # ---- 1) cache-first history ----
          hist <- fetch_history_cache_only(a)
          # If still nothing useful, skip
          if (!is.data.frame(hist) || nrow(hist) == 0 || !has_measure_cols(hist)) {
            showNotification(sprintf("No usable data in cache for %s; skipping.", a),
                             type = "warning", duration = 5)
            next
          }
          
          # ---- 2) normalize for Rmd ----
          names(hist) <- tolower(names(hist))
          hist <- make_title_aliases(hist)
          if (!("athletename" %in% names(hist))) hist$athletename <- hist$athlete %||% NA_character_
          
          renv <- new.env(parent = globalenv())
          renv$AthleteData <- hist %>%
            dplyr::rename(AthleteName = athletename) %>%
            dplyr::mutate(Date = suppressWarnings(as.Date(date)))
          
          # Show chunk errors instead of silent aborts
          renv$.__set_knitr_opts <- function() knitr::opts_chunk$set(error = TRUE)
          renv$.__set_knitr_opts()
          
          fmt <- choose_output_format()
          out_ext <- if (fmt == "pdf_document") ".pdf" else ".html"
          base <- paste0(safe_file(a), "_", format(Sys.Date(), "%Y-%m-%d"))
          outname <- paste0(base, out_ext)
          outpath <- file.path(tmpdir, outname)
          
          ok <- tryCatch({
            rmarkdown::render(
              input         = "anthro_report_PDF.Rmd",
              output_format = fmt,
              output_file   = outname,
              output_dir    = tmpdir,
              envir         = renv,
              quiet         = TRUE,
              clean         = TRUE
            )
            TRUE
          }, error = function(e) {
            showNotification(paste("Render failed for", a, ":", e$message), type = "error", duration = 10)
            FALSE
          })
          
          # ---- 3) ensure PDF even when TeX is absent (HTML->PDF via pagedown) ----
          if (ok && file.exists(outpath)) {
            if (identical(fmt, "pdf_document")) {
              if (tolower(tools::file_ext(outpath)) == "pdf") out_files <- c(out_files, outpath)
            } else {
              # Try to convert HTML to PDF
              pdf_path <- file.path(tmpdir, paste0(base, ".pdf"))
              conv_ok <- FALSE
              if (requireNamespace("pagedown", quietly = TRUE)) {
                conv_ok <- isTRUE(try({
                  pagedown::chrome_print(input = outpath, output = pdf_path); file.exists(pdf_path)
                }, silent = TRUE))
              }
              if (conv_ok) {
                out_files <- c(out_files, pdf_path)
                # Optionally remove the html if you don’t want it in the zip
                try(unlink(outpath), silent = TRUE)
              } else {
                showNotification(sprintf("Converted HTML report for %s was not produced; keeping HTML.", a),
                                 type = "warning", duration = 6)
                # If you strictly want PDFs only, do not add the HTML.
                # If you prefer to include HTML when PDF fails, uncomment below:
                # out_files <- c(out_files, outpath)
              }
            }
          } else {
            showNotification(sprintf("Render did not produce a file for %s; skipping.", a),
                             type = "error", duration = 6)
          }
        } # for each selection
        incProgress(1, detail = "Packaging ZIP…")
      }) # withProgress
      
      if (length(out_files) == 0) {
        showNotification("No reports were generated (cache empty or render failed).", type = "error", duration = 8)
        stop("no-reports")
      }
      
      # --- zip only the PDFs ---
      if (requireNamespace("zip", quietly = TRUE)) {
        zip::zipr(zipfile = file, files = out_files, root = tmpdir)
      } else {
        oldwd <- getwd(); setwd(tmpdir); on.exit(setwd(oldwd), add = TRUE)
        utils::zip(zipfile = file, files = basename(out_files))
      }
    }
  )
  
  
  # ---- Download selected sessions as ONE CSV (cache-first, with progress) ----
  output$report_csv <- downloadHandler(
    filename = function() paste0(format(Sys.Date(), "%Y-%m-%d"), "_anthro.csv"),
    contentType = "text/csv; charset=utf-8",
    content = function(file) {
      # 1) Validate selection
      ids <- isolate(input$report_selected)
      if (is.null(ids) || length(ids) == 0) {
        showNotification("Please select at least one row to download.", type = "warning", duration = 4)
        stop("no-selection")
      }
      
      # 2) Resolve indices against the visible table
      df <- report_df()
      n  <- nrow(df)
      sel_idx <- suppressWarnings(as.integer(gsub("^sel_", "", ids)))
      sel_idx <- sel_idx[!is.na(sel_idx) & sel_idx >= 1 & sel_idx <= n]
      if (length(sel_idx) == 0) {
        showNotification("Please select at least one row to download.", type = "warning", duration = 4)
        stop("no-valid-index")
      }
      
      # Helper: try to pull one row (athlete + date) from the in-memory DB;
      # fallback to API if cache is missing or empty.
      pull_one_row <- function(a, d) {
        fetch_full_session_row(a, d)  # always returns a tibble (maybe 0-row)
      }
      
      # 3) Build the CSV rows
      rows <- list()
      withProgress(message = "Building CSV…", value = 0, {
        total <- length(sel_idx)
        for (k in seq_along(sel_idx)) {
          i <- sel_idx[k]
          a <- as.character(df$athlete[i])
          d <- as.character(df$date[i])
          
          incProgress((k - 1) / max(1, total),
                      detail = sprintf("%s — %s", d, a))
          
          got_row <- pull_one_row(a, d)
          if (is.data.frame(got_row) && nrow(got_row) > 0) {
            rows[[length(rows) + 1]] <- got_row
          } else {
            showNotification(sprintf("No data found for %s (%s); skipping.", a, d),
                             type = "warning", duration = 4)
          }
        }
        incProgress(1, detail = "Finalizing…")
      })
      
      # 4) Bind and flatten
      if (length(rows) == 0) {
        showNotification("No data found for the selected entries.", type = "error", duration = 5)
        return(invisible(NULL))
      }
      
      out <- safe_bind_rows(rows)
      if (nrow(out) > 0) {
        out <- tibble::as_tibble(
          jsonlite::fromJSON(jsonlite::toJSON(out, auto_unbox=TRUE, null="null"), flatten=TRUE),
          .name_repair = "universal"
        )
        names(out) <- tolower(names(out))
        # Common Apps Script / sheet variants → standardize
        if (!"athlete" %in% names(out)) {
          if ("athletename" %in% names(out)) out <- dplyr::rename(out, athlete = athletename)
          if ("name" %in% names(out))        out <- dplyr::rename(out, athlete = name)
        }
        if (!"date" %in% names(out)) {
          if ("collection_date" %in% names(out)) out <- dplyr::rename(out, date = collection_date)
          if ("session_date" %in% names(out))    out <- dplyr::rename(out, date = session_date)
        }
      }

      if (!is.data.frame(out) || nrow(out) == 0) {
        showNotification("No data found for the selected entries.", type = "error", duration = 5)
        stop("no-rows")
      }
      
      # Flatten nested lists (if any) by JSON round-trip
      out <- tibble::as_tibble(
        jsonlite::fromJSON(jsonlite::toJSON(out, auto_unbox = TRUE, null = "null"),
                           flatten = TRUE),
        .name_repair = "universal"
      )
      
      # 5) Write CSV (Excel-friendly, UTF-8 BOM)
      readr::write_excel_csv(out, file, na = "")
    }
  )
}

shinyApp(ui, server)
