library(shiny)
library(shinyWidgets)
library(shinyTime)
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

# ================== Backend ==================
API_URL <- "https://script.google.com/macros/s/AKfycbxPfNlemEphi3kKsT_A9tDw3PLapodl73vfCGLp05bnvLwmGsPCJkARZhvGQSdt4MkB/exec"
API_KEY <- "cCkbo3tpI9CQLTBTY8bkeWeHt-6oHlCsZ8O7YuQ4fClB64LG1z_nq-oUdyZ4KsAf"

`%||%` <- function(a, b) if (!is.null(a)) a else b
safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", x)
cap_first <- function(s) {
  if (is.null(s) || length(s) == 0) return(s)
  s <- trimws(s)
  ifelse(is.na(s) | nchar(s) == 0, s, paste0(toupper(substr(s,1,1)), substring(s,2)))
}
norm_name <- function(x) tolower(trimws(x))

api_get <- function(params = list()) {
  params$key <- API_KEY
  resp <- httr::GET(API_URL, query = params, httr::timeout(30))
  httr::stop_for_status(resp, task = paste("GET", params$action %||% ""))
  txt <- httr::content(resp, "text", encoding = "UTF-8")
  fromJSON(txt, simplifyVector = FALSE)
}

api_post <- function(body) {
  body$key <- API_KEY
  payload <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
  resp <- httr::POST(API_URL, body = payload, httr::content_type("text/plain; charset=UTF-8"), httr::timeout(30))
  httr::stop_for_status(resp, task = "POST Apps Script")
  txt <- httr::content(resp, "text", encoding = "UTF-8")
  fromJSON(txt, simplifyVector = FALSE)
}

# dropdown helpers
load_practitioners <- function() {
  res <- try(api_get(list(action = "practitioners")), silent = TRUE)
  if (!inherits(res, "try-error") && isTRUE(res$ok) && length(res$data) > 0) {
    return(unlist(res$data))
  }
  character()
}

load_athletes <- function() {
  res <- try(api_get(list(action = "athletes")), silent = TRUE)
  if (!inherits(res, "try-error") && isTRUE(res$ok) && length(res$data) > 0) {
    return(unlist(res$data))
  }
  character()
}

# ================== Choices & Measures ==================
day_of_cycle_choices <- c("Not Tracking", as.character(1:35))
caliper_choices <- c(
  "VIC_SIR508","VIC_SIR619","VIC_VDH7033","VIC_ZAB634",
  "VAN_P1697","VAN_YDH7487","VAN_SHD097","WHI_SIR483"
)
scale_choices <- c("CSI Lab","Personal","Athlete's Training Center","Force Plate")
yes_no <- c("Yes","No")

measures_list <- list(
  "Height and Weight" = c("Height (cm)","Sitting Height (cm)","Armspan (cm)","Weight (kg)"),
  "Skinfolds" = c("Triceps (mm)","Subscap (mm)","Biceps (mm)","Illiac (mm)","Supraspinale (mm)","Abdomen (mm)","R Thigh (mm)","R Calf (mm)","L Thigh (mm)","L Calf (mm)"),
  "Girth" = c("R Relaxed Bicep (cm)","R Flexed Bicep (cm)","L Relaxed Bicep (cm)","L Flexed Bicep (cm)","R Forearm (cm)","L Forearm (cm)","Waist (cm)","Hips (cm)","R Mid Thigh (cm)","R Medial Calf (cm)","L Mid Thigh (cm)","L Medial Calf (cm)")
)

# map measure label -> base key used in wide columns
measure_key <- list(
  # Height & Weight
  "Height (cm)" = "height_cm",
  "Sitting Height (cm)" = "sitting_height_cm",
  "Armspan (cm)" = "armspan_cm",
  "Weight (kg)" = "weight_kg",
  # Skinfolds
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
  # Girth
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

# thresholds & bounds
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

# Recent with timestamps via get_anthro (wide row)
fetch_recent_with_ts <- function(limit = 10, progress = FALSE) {
  res <- try(api_get(list(action = "recent_anthro", limit = limit, cb = as.integer(Sys.time()))), silent = TRUE)
  if (inherits(res, "try-error") || !isTRUE(res$ok) || length(res$data) == 0) {
    return(tibble(date = character(), athlete = character(), timestamp = character()))
  }
  df <- try(tibble::as_tibble(jsonlite::fromJSON(jsonlite::toJSON(res$data))), silent = TRUE)
  if (inherits(df, "try-error") || nrow(df) == 0) {
    return(tibble(date = character(), athlete = character(), timestamp = character()))
  }

  fetch_one <- function(a, d) {
    got <- try(api_get(list(action = "get_anthro", athlete = a, date = d, cb = as.integer(Sys.time()))), silent = TRUE)
    ts1 <- ""
    if (!inherits(got, "try-error") && isTRUE(got$ok) && length(got$data) > 0) {
      d1 <- try(tibble::as_tibble(jsonlite::fromJSON(jsonlite::toJSON(got$data))), silent = TRUE)
      if (!inherits(d1, "try-error") && nrow(d1) > 0 && "timestamp" %in% names(d1)) {
        ts1 <- as.character(d1$timestamp[1])
      }
    }
    tibble(date = d, athlete = a, timestamp = ts1)
  }

  rows <- vector("list", nrow(df))
  if (isTRUE(progress)) {
    withProgress(message = 'Loading recent entries…', value = 0, {
      n <- nrow(df)
      for (i in seq_len(n)) {
        incProgress(1/n, detail = sprintf("%s — %s", as.character(df$date[i]), as.character(df$athlete[i])))
        rows[[i]] <- fetch_one(as.character(df$athlete[i]), as.character(df$date[i]))
      }
    })
  } else {
    for (i in seq_len(nrow(df))) rows[[i]] <- fetch_one(as.character(df$athlete[i]), as.character(df$date[i]))
  }

  out <- dplyr::bind_rows(rows) %>%
    mutate(ts_chr = as.character(timestamp)) %>%
    arrange(desc(ts_chr)) %>%
    select(date, athlete, timestamp)
  out
}

# Append/Replace rows (wide objects)
append_rows <- function(rows) {
  body <- list(action = "append", rows = rows)
  res <- try(api_post(body), silent = TRUE)
  isTRUE(!inherits(res, "try-error") && isTRUE(res$ok))
}
replace_rows_for_key <- function(athlete, date, rows) {
  body <- list(action = "replace", key = list(athlete = athlete, date = date), rows = rows)
  res <- try(api_post(body), silent = TRUE)
  isTRUE(!inherits(res, "try-error") && isTRUE(res$ok))
}

# ================== UI ==================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { overflow-y: auto; }
      .card { border: 1px solid #e5e7eb; border-radius: 12px; padding: 16px; margin-bottom: 18px; box-shadow: 0 1px 3px rgba(0,0,0,0.04); overflow: visible; }
      .row-head { font-weight: 600; color: #374151; padding: 4px 0 8px 0; }
      .row-line { border-bottom: 1px dashed #e5e7eb; margin-bottom: 6px; padding-bottom: 6px; }
      .value-box { font-weight: 600; }
      .muted { color: #6b7280; }
      .warn { color: #b91c1c; font-size: 0.92em; }
      input[type=number]::-webkit-outer-spin-button,
      input[type=number]::-webkit-inner-spin-button { -webkit-appearance: none; margin: 0; }
      input[type=number] { -moz-appearance: textfield; }
      .mini-num input.form-control { max-width: 100%; overflow: hidden; }
      .measure-row { display: flex; align-items: flex-start; gap: 8px; }
      .seg-name { flex: 0 0 25%; }
      .seg-in { flex: 0 0 12.5%; }
      .seg-sugg { flex: 0 0 25%; }
      .seg-val { flex: 0 0 12.5%; }
      @media (max-width: 992px) {
        .measure-row { flex-wrap: wrap; }
        .seg-name { flex-basis: 100%; }
        .seg-in { flex-basis: 30%; }
        .seg-sugg { flex-basis: 70%; }
        .seg-val { flex-basis: 30%; }
      }
    "))
  ),
  fluidRow(
    column(6, tags$img(src = "CSIP.jpg", height = "80px", style = "padding:10px;")),
    column(6, div(style = "text-align:right;", tags$img(src = "CCBC.jpg", height = "80px", style = "padding:10px;")))
  ),
  titlePanel("CSI Pacific Anthropometry Data Entry"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectizeInput("practitioner", "Practitioner",
                     choices = NULL,
                     options = list(placeholder = "Type or pick", create = TRUE)),
      selectizeInput("athlete", "Athlete name",
                     choices = NULL,
                     options = list(placeholder = "Type or pick", create = TRUE)),
      dateInput("date", "Collection Date", value = Sys.Date()),
      shinyTime::timeInput("time", "Collection Time", value = strptime(format(Sys.time(), "%I:%M %p"), "%I:%M %p"),
                           seconds = FALSE),
      pickerInput("scale", "Scale", choices = scale_choices, multiple = FALSE,
                  options = pickerOptions(style = "btn-outline-primary")),
      hr(),
      actionButton("prefill_btn", "Prefill from Athlete's most recent data", class = "btn-outline-secondary", width = "100%"),
      br(), br(),
      actionButton("submit", "Submit & Save", class = "btn-primary", width = "100%"),
      br(),
      h4("Batch upload"),
      fileInput("batch_file", "Excel (.xlsx) file only", accept = c(".xlsx"), buttonLabel = "Browse..."),
      actionButton("upload_btn", "Upload to Google Sheet", class = "btn-warning", width = "100%"),
      br(),
      textOutput("upload_status"),
      br(),
      verbatimTextOutput("status"),
      h4("Last 10 entries"),
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

# ================== Server ==================
server <- function(input, output, session) {
  clear_form <- function() {
    # Clear Section 1
    updateSelectizeInput(session, "practitioner", selected = "")
    updateSelectizeInput(session, "athlete", selected = "")
    updateDateInput(session, "date", value = Sys.Date())
    try(shinyTime::updateTimeInput(session, "time", value = strptime(format(Sys.time(), "%I:%M %p"), "%I:%M %p")), silent = TRUE)
    updatePickerInput(session, "scale", selected = character(0))
    updatePrettyRadioButtons(session, "fasted", selected = character(0))
    updatePickerInput(session, "doc", selected = "Not Tracking")
    updatePrettyRadioButtons(session, "bc", selected = character(0))
    updatePrettyRadioButtons(session, "creatine", selected = character(0))
    updateNumericInput(session, "usg", value = NA)
    updatePickerInput(session, "caliper", selected = character(0))
    updateTextAreaInput(session, "comments", value = "")

    # Clear Section 2
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


  pending_row <- reactiveVal(NULL)

  # Populate dropdowns
  observe({
    updateSelectizeInput(session, "practitioner", choices = load_practitioners(), server = TRUE)
    updateSelectizeInput(session, "athlete", choices = load_athletes(), server = TRUE)
  })

  # USG warning under the USG box
  output$usg_warn <- renderUI({
    v <- input$usg
    if (is.null(v) || is.na(v)) return(span(class="muted",""))
    if (v < 0.950 || v > 1.080) {
      span(class="warn","Warning: USG out of range (expected 0.950–1.080).")
    } else {
      span(class="muted","")
    }
  })

  # Last 10 entries (timestamp-sorted)
  recent_data <- reactiveVal(fetch_recent_with_ts(10, progress = TRUE))
  output$recent_tbl <- renderDT({
    df <- recent_data()
    if (is.null(df) || nrow(df) == 0) {
      datatable(data.frame(date = character(), athlete = character()), options = list(dom = 't', paging = FALSE), rownames = FALSE)
    } else {
      df2 <- df %>% select(date, athlete)  # already sorted by timestamp desc
      datatable(df2, options = list(dom = 't', paging = FALSE), rownames = FALSE)
    }
  })

  # Range guards for numeric inputs (reject out-of-range by resetting to NA)
  register_guard <- function(id, lo, hi, label = "Value") {
    observeEvent(input[[id]], {
      v <- input[[id]]
      if (!is.null(v) && !is.na(v) && (v < lo || v > hi)) {
        showNotification(glue("{label} must be between {lo} and {hi}."), type = "error", duration = 3)
        updateNumericInput(session, id, value = NA)
      }
    }, ignoreInit = TRUE)
  }

  # Section 2 rows
  build_row <- function(category, item, idx) {
    base <- paste0(safe_id(category), "_", safe_id(item), "_", idx)
    id1 <- paste0("m1_", base)
    id2 <- paste0("m2_", base)
    id3 <- paste0("m3_", base)
    idsugg <- paste0("sugg_", base)
    idval <- paste0("val_", base)

    bnds <- bounds_for(category, item); lo <- bnds[1]; hi <- bnds[2]

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

    register_guard(id1, lo, hi, "Measure 1")
    register_guard(id2, lo, hi, "Measure 2")
    register_guard(id3, lo, hi, "Measure 3")

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

  # Prefill using most recent date for athlete -> read wide row
  output$status <- renderText("")
  observeEvent(input$prefill_btn, {
    withProgress(message = 'Prefilling…', value = 0, {
      if (is.null(input$athlete) || !nzchar(trimws(input$athlete))) {
        showModal(modalDialog("Please select Athlete first.", easyClose = TRUE))
        return(invisible(NULL))
      }

      incProgress(0.15, detail = 'Contacting API')
      rec <- try(api_get(list(action = "recent_anthro", limit = 1000, cb = as.integer(Sys.time()))), silent = TRUE)
      if (inherits(rec, "try-error") || !isTRUE(rec$ok) || length(rec$data) == 0) {
        showNotification("No recent entries returned from API.", type = "error"); return(invisible(NULL))
      }
      rdf <- try(tibble::as_tibble(jsonlite::fromJSON(jsonlite::toJSON(rec$data))), silent = TRUE)
      if (inherits(rdf, "try-error") || nrow(rdf) == 0) {
        showNotification("Recent entries parsing failed.", type = "error"); return(invisible(NULL))
      }
      if (!all(c("date","athlete") %in% names(rdf))) {
        showNotification("Recent entries missing 'date'/'athlete' columns.", type = "error"); return(invisible(NULL))
      }

      target <- norm_name(input$athlete)
      rdf2 <- rdf %>% mutate(resp_lower = tolower(trimws(athlete))) %>% filter(resp_lower == target)
      if (nrow(rdf2) == 0) {
        showNotification("This athlete not found in recent list.", type = "warning"); return(invisible(NULL))
      }

      latest_date <- as.character(rdf2$date[1])
      incProgress(0.5, detail = 'Fetching session data')
      got <- try(api_get(list(action = "get_anthro", athlete = input$athlete, date = latest_date, cb = as.integer(Sys.time()))), silent = TRUE)
      if (inherits(got, "try-error") || !isTRUE(got$ok) || length(got$data) == 0) {
        showNotification("No session row from get_anthro.", type = "warning"); return(invisible(NULL))
      }
      incProgress(0.7, detail = 'Parsing data')
      dat <- try(tibble::as_tibble(jsonlite::fromJSON(jsonlite::toJSON(got$data))), silent = TRUE)
      if (inherits(dat, "try-error") || nrow(dat) == 0) {
        showNotification("Could not parse get_anthro result.", type = "error"); return(invisible(NULL))
      }

      row <- dat[1,]

      # ---- Prefill Section 1 ----
      if ("practitioner" %in% names(row)) updateSelectizeInput(session, "practitioner", selected = row$practitioner)
      updateSelectizeInput(session, "athlete", selected = input$athlete)

      if ("date" %in% names(row)) {
        d_try <- suppressWarnings(lubridate::ymd(row$date))
        if (is.na(d_try)) d_try <- suppressWarnings(lubridate::ymd_hms(row$date))
        if (!is.na(d_try)) updateDateInput(session, "date", value = as.Date(d_try))
      }
      if ("time" %in% names(row)) {
        t_try <- suppressWarnings(lubridate::ymd_hms(row$time))
        if (!is.na(t_try)) shinyTime::updateTimeInput(session, "time", value = strptime(format(t_try, "%I:%M %p"), "%I:%M %p"))
      }
      if ("scale" %in% names(row))    updatePickerInput(session, "scale", selected = row$scale)
      if ("fasted" %in% names(row))   updatePrettyRadioButtons(session, "fasted", selected = row$fasted)
      if ("day_of_cycle" %in% names(row)) updatePickerInput(session, "doc", selected = as.character(row$day_of_cycle))
      if ("birth_control" %in% names(row)) updatePrettyRadioButtons(session, "bc", selected = row$birth_control)
      if ("creatine" %in% names(row)) updatePrettyRadioButtons(session, "creatine", selected = row$creatine)
      if ("usg" %in% names(row))      updateNumericInput(session, "usg", value = suppressWarnings(as.numeric(row$usg)))
      if ("caliper" %in% names(row))  updatePickerInput(session, "caliper", selected = row$caliper)
      if ("comments" %in% names(row)) updateTextAreaInput(session, "comments", value = row$comments %||% "")

      # ---- Prefill Measures from wide columns ----
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

          bnds <- bounds_for(cat, meas); lo <- bnds[1]; hi <- bnds[2]
          if (!is.na(v1) && (v1 < lo || v1 > hi)) v1 <- NA_real_
          if (!is.na(v2) && (v2 < lo || v2 > hi)) v2 <- NA_real_
          if (!is.na(v3) && (v3 < lo || v3 > hi)) v3 <- NA_real_

          if (!is.null(input[[id1]])) updateNumericInput(session, id1, value = v1)
          if (!is.null(input[[id2]])) updateNumericInput(session, id2, value = v2)
          if (!is.null(input[[id3]])) updateNumericInput(session, id3, value = v3)
        }
      }

      incProgress(0.95, detail = 'Done')
    })
    showNotification(glue("Prefilled data for {input$athlete}."), type="message", duration=5)
  })

  # ---- Batch upload: Excel (wide) -> Google Sheet (wide). Add timestamp if missing) ----
  output$upload_status <- renderText("")
  observeEvent(input$upload_btn, {
    req(input$batch_file)
    f <- input$batch_file$datapath

    df <- try(readxl::read_excel(f), silent = TRUE)
    if (inherits(df, "try-error")) {
      output$upload_status <- renderText("Could not read Excel file."); return(invisible(NULL))
    }
    if (nrow(df) == 0) {
      output$upload_status <- renderText("No rows found in the Excel file."); return(invisible(NULL))
    }

    df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
    df[is.na(df)] <- ""

    ts_now <- as.character(Sys.time())
    rows <- lapply(seq_len(nrow(df)), function(i) {
      row <- as.list(df[i, , drop = FALSE])
      if (is.null(row$timestamp) || identical(row$timestamp, "") || is.na(row$timestamp)) {
        row$timestamp <- ts_now
      }
      row
    })

    # chunked append for stability
    ok_all <- TRUE
    withProgress(message = 'Uploading...', value = 0, {
      n <- length(rows)
      for (i in seq_len(n)) {
        incProgress(1/n, detail = sprintf('Row %d of %d', i, n))
        ok <- append_rows(list(rows[[i]]))
        ok_all <- ok_all && isTRUE(ok)
        if (!ok) break
      }
    })
    if (ok_all) {
      output$upload_status <- renderText(sprintf("Uploaded %d rows to Google Sheet.", length(rows)))
      recent_data(fetch_recent_with_ts(10, progress = TRUE))
    } else {
      output$upload_status <- renderText("Upload failed on one or more rows.")
    }
  })

  # Submit & Save with duplicate confirmation (wide: one row per session)
  observeEvent(input$submit, {
    output$status <- renderText("")
    errs <- c()
    if (is.null(input$practitioner) || !nzchar(trimws(input$practitioner))) errs <- c(errs, "Practitioner is required.")
    if (is.null(input$athlete) || !nzchar(trimws(input$athlete))) errs <- c(errs, "Athlete is required.")
    if (length(errs) > 0) { output$status <- renderText(paste(errs, collapse = "\n")); return() }

    ts_now <- as.character(Sys.time())
    time_str <- tryCatch(format(input$time, "%H:%M:%S"), error = function(e) NA_character_)

    # Build a single wide row
    row <- list(
      timestamp     = ts_now,
      practitioner  = input$practitioner %||% "",
      athlete       = input$athlete %||% "",
      date          = as.character(input$date %||% ""),
      time          = time_str %||% "",
      scale         = input$scale %||% "",
      fasted        = input$fasted %||% "",
      day_of_cycle  = if (!is.null(input$doc)) ifelse(input$doc=="Not Tracking","Not Tracking", suppressWarnings(as.numeric(input$doc))) else NA,
      birth_control = input$bc %||% "",
      creatine      = input$creatine %||% "",
      usg           = if (!is.null(input$usg)) round(input$usg, 3) else NA_real_,
      caliper       = input$caliper %||% "",
      comments      = input$comments %||% ""
    )

    # Add measure fields
    for (cat in names(measures_list)) {
      items <- measures_list[[cat]]
      for (i in seq_along(items)) {
        meas <- items[[i]]
        key <- measure_key[[meas]]
        if (is.null(key)) next
        base <- paste0(safe_id(cat), "_", safe_id(meas), "_", i)
        id1 <- paste0("m1_", base)
        id2 <- paste0("m2_", base)
        id3 <- paste0("m3_", base)

        v1 <- suppressWarnings(as.numeric(input[[id1]]))
        v2 <- suppressWarnings(as.numeric(input[[id2]]))
        v3 <- suppressWarnings(as.numeric(input[[id3]]))

        bnds <- bounds_for(cat, meas); lo <- bnds[1]; hi <- bnds[2]
        if (!is.na(v1) && (v1 < lo || v1 > hi)) { showNotification(glue("{meas}: 1 out of range"), type="error"); return() }
        if (!is.na(v2) && (v2 < lo || v2 > hi)) { showNotification(glue("{meas}: 2 out of range"), type="error"); return() }
        if (!is.na(v3) && (v3 < lo || v3 > hi)) { showNotification(glue("{meas}: 3 out of range"), type="error"); return() }

        th <- threshold_for(cat)
        s_msg <- suggest_msg(v1, v2, th)
        val <- calc_value(v1, v2, v3, s_msg)

        row[[paste0(key, "_m1")]] <- ifelse(is.na(v1), "", v1)
        row[[paste0(key, "_m2")]] <- ifelse(is.na(v2), "", v2)
        row[[paste0(key, "_m3")]] <- ifelse(is.na(v3), "", v3)
        row[[paste0(key, "_suggest")]] <- s_msg %||% ""
        row[[paste0(key, "_value")]] <- ifelse(is.na(val), "", round(val, 2))
      }
    }

    # Duplicate check AFTER building row
    if (!is.null(input$athlete) && nzchar(trimws(input$athlete)) && !is.null(input$date)) {
      dup_chk <- try(api_get(list(action = "get_anthro", athlete = input$athlete, date = as.character(input$date), cb = as.integer(Sys.time()))), silent = TRUE)
      if (!inherits(dup_chk, "try-error") && isTRUE(dup_chk$ok) && length(dup_chk$data) > 0) {
        pending_row(row)
        showModal(modalDialog(
          title = "Duplicate detected",
          easyClose = FALSE,
          footer = tagList(
            actionButton("confirm_overwrite", "Overwrite existing record", class = "btn-danger"),
            modalButton("Cancel")
          ),
          "An entry already exists for this Athlete and Date. Do you want to overwrite the existing record with your current entries?"
        ))
        return(invisible(NULL))
      }
    }

    ok <- append_rows(list(row))
    if (ok) {
      output$status <- renderText("Saved to database")
      recent_data(fetch_recent_with_ts(10, progress = TRUE))
      clear_form()
    } else {
      output$status <- renderText("Error: could not save to Google Sheet.")
    }
  })

  observeEvent(input$confirm_overwrite, {
    req(!is.null(pending_row()))
    removeModal()
    row <- pending_row()
    athlete_key <- input$athlete
    date_key <- as.character(input$date)
    ok <- replace_rows_for_key(athlete_key, date_key, list(row))
    if (ok) {
      output$status <- renderText("Existing record overwritten and saved to database")
      pending_row(NULL)
      recent_data(fetch_recent_with_ts(10, progress = TRUE))
      clear_form()
    } else {
      output$status <- renderText("Error: could not overwrite existing record.")
    }
  })
}

shinyApp(ui, server)
