
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

# ================== API CONFIG ==================
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

# ================== API HELPERS ==================
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
  resp <- httr::POST(API_URL, body = payload,
                     httr::content_type("text/plain; charset=UTF-8"),
                     httr::timeout(30))
  httr::stop_for_status(resp, task = "POST Apps Script")
  txt <- httr::content(resp, "text", encoding = "UTF-8")
  fromJSON(txt, simplifyVector = FALSE)
}

load_practitioners <- function() {
  res <- try(api_get(list(action = "practitioners", cb = as.integer(Sys.time()))), silent = TRUE)
  if (!inherits(res, "try-error") && isTRUE(res$ok) && length(res$data) > 0) unlist(res$data) else character()
}

load_athletes <- function() {
  res <- try(api_get(list(action = "athletes", cb = as.integer(Sys.time()))), silent = TRUE)
  if (!inherits(res, "try-error") && isTRUE(res$ok) && length(res$data) > 0) unlist(res$data) else character()
}

fetch_recent <- function(limit = 10) {
  res <- try(api_get(list(action = "recent_anthro", limit = limit, cb = as.integer(Sys.time()))), silent = TRUE)
  if (inherits(res, "try-error") || !isTRUE(res$ok) || length(res$data) == 0) {
    return(tibble(date = character(), athlete = character()))
  }
  as_tibble(jsonlite::fromJSON(jsonlite::toJSON(res$data)))
}

fetch_recent_with_ts <- function(limit = 10, progress = FALSE) {
  df <- fetch_recent(limit)
  if (nrow(df) == 0) return(tibble(date = character(), athlete = character(), timestamp = character()))
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
    for (i in seq_len(nrow(df))) {
      rows[[i]] <- fetch_one(as.character(df$athlete[i]), as.character(df$date[i]))
    }
  }
  bind_rows(rows) %>%
    mutate(ts_chr = as.character(timestamp)) %>%
    arrange(desc(ts_chr)) %>%
    select(date, athlete, timestamp)
}

append_rows <- function(rows) {
  res <- try(api_post(list(action = "append", rows = rows)), silent = TRUE)
  isTRUE(!inherits(res, "try-error") && isTRUE(res$ok))
}

replace_rows_for_key <- function(athlete, date, rows) {
  res <- try(api_post(list(action = "replace",
                           key = list(athlete = athlete, date = date),
                           rows = rows)), silent = TRUE)
  isTRUE(!inherits(res, "try-error") && isTRUE(res$ok))
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
  "Height and Weight" = c("Height (cm)","Sitting Height (cm)","Armspan (cm)","Weight (kg)"),
  "Skinfolds" = c("Triceps (mm)","Subscap (mm)","Biceps (mm)","Illiac (mm)","Supraspinale (mm)","Abdomen (mm)","R Thigh (mm)","R Calf (mm)","L Thigh (mm)","L Calf (mm)"),
  "Girth" = c("R Relaxed Bicep (cm)","R Flexed Bicep (cm)","L Relaxed Bicep (cm)","L Flexed Bicep (cm)","R Forearm (cm)","L Forearm (cm)","Waist (cm)","Hips (cm)","R Mid Thigh (cm)","R Medial Calf (cm)","L Mid Thigh (cm)","L Medial Calf (cm)")
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

# ================== BRAND HEADER (black bg, white text, bigger tabs, one logo) ==================
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
  tags$head(
    tags$style(HTML("
      .navbar-nav > li > a { font-size: 18px; font-weight: 700; }
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
      #recent_tbl table {font-size: 0.85em;} #recent_tbl table th, #recent_tbl table td {font-size: 0.85em;}
      #report_tbl table { font-size: 0.85em; table-layout: fixed; width: 100%; }
      #report_tbl table th, #report_tbl table td { font-size: 0.85em; padding: 6px 8px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
      #report_tbl input[type=checkbox] { transform: scale(1.1); }
    "))
  ),
  brand_header(),
  navbarPage(
    title = NULL,
    id = "main_nav",
    tabPanel(
      title = "Data Entry",
      fluidPage(
        # ======== DO NOT CHANGE THIS TAB (kept as provided) ========
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
          #recent_tbl table {font-size: 0.85em;} #recent_tbl table th, #recent_tbl table td {font-size: 0.85em;}
        "))
        ),
        
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
            shinyTime::timeInput("time", "Collection Time",
                                 value = strptime(format(Sys.time(), "%I:%M %p"), "%I:%M %p"),
                                 seconds = FALSE),
            pickerInput("scale", "Scale", choices = scale_choices, multiple = FALSE,
                        options = pickerOptions(style = "btn-outline-primary")),
            hr(),
            actionButton("prefill_btn", "Prefill from Athlete's most recent data",
                         class = "btn-outline-secondary", width = "100%"),
            br(), br(),
            actionButton("submit", "Submit & Save", class = "btn-primary", width = "100%"),
            br(),
            h4("Batch upload"),
            fileInput("batch_file", "Excel (.xlsx) file only", accept = c(".xlsx"),
                      buttonLabel = "Browse..."),
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
    ),
    tabPanel(
      title = "Report",
      fluidPage(
        tags$head(
          tags$style(HTML("
          #report_tbl table { font-size: 0.85em; table-layout: fixed; width: 100%; }
          #report_tbl table th, #report_tbl table td { font-size: 0.85em; padding: 6px 8px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
          #report_tbl input[type=checkbox] { transform: scale(1.1); }
        "))
        ),
        fluidRow(
          column(4, actionButton("report_reload", "Reload list", class = "btn-outline-secondary", width = "100%")),
          column(4, actionButton("report_select_all", "Select all", class = "btn-outline-secondary", width = "100%")),
          column(4, actionButton("report_select_none", "Select none", class = "btn-outline-secondary", width = "100%"))
        ),
        br(),
        DTOutput("report_tbl"),
        tags$script(HTML("
          $(document).on('change', 'input[type=checkbox][id^=sel_]', function(){
            var ids = $('input[type=checkbox][id^=sel_]:checked').map(function(){return this.id;}).get();
            Shiny.setInputValue('report_selected', ids, {priority:'event'});
          });
        ")),
        br(),
        fluidRow(
          column(4),
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
  # Startup progress
  session$onFlushed(function() {
    withProgress(message = 'Loading dashboard…', value = 0, {
      incProgress(0.3, detail = 'Fetching recent entries')
      recent_data(fetch_recent_with_ts(10, progress = TRUE))
      incProgress(0.7, detail = 'Loading dropdowns')
      updateSelectizeInput(session, "practitioner", choices = load_practitioners(), server = TRUE)
      updateSelectizeInput(session, "athlete", choices = load_athletes(), server = TRUE)
      incProgress(1, detail = 'Ready')
    })
  }, once = TRUE)

  # USG inline warning
  output$usg_warn <- renderUI({
    v <- input$usg
    if (is.null(v) || is.na(v)) return(span(class="muted",""))
    if (v < 0.950 || v > 1.080) span(class="warn","Warning: USG out of range (expected 0.950–1.080).") else span(class="muted","")
  })

  # Recent list on sidebar
  recent_data <- reactiveVal(tibble(date = character(), athlete = character(), timestamp = character()))
  output$recent_tbl <- renderDT({
    df <- recent_data()
    if (is.null(df) || nrow(df) == 0) {
      datatable(data.frame(Date = character(), Athlete = character()),
                options = list(dom = 't', paging = FALSE), rownames = FALSE)
    } else {
      df2 <- df %>% select(date, athlete) %>% rename(Date = date, Athlete = athlete)
      datatable(df2,
                options = list(dom = 't', paging = FALSE,
                               columnDefs = list(list(width = "60%", targets = 0),
                                                 list(width = "40%", targets = 1))),
                rownames = FALSE)
    }
  })

  # ---------- Measures UI (unchanged) ----------
  register_guard <- function(id, lo, hi, label = "Value") {
    observeEvent(input[[id]], {
      v <- input[[id]]
      if (!is.null(v) && !is.na(v) && (v < lo || v > hi)) {
        showNotification(glue("{label} must be between {lo} and {hi}."), type = "error", duration = 3)
        updateNumericInput(session, id, value = NA)
      }
    }, ignoreInit = TRUE)
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

  # ---------- Prefill (latest for athlete) ----------
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
      incProgress(0.15, detail = 'Contacting API')
      rec <- try(api_get(list(action = "recent_anthro", limit = 1000, cb = as.integer(Sys.time()))), silent = TRUE)
      validate(need(!inherits(rec, "try-error") && isTRUE(rec$ok) && length(rec$data) > 0, "No recent entries."))
      rdf <- as_tibble(jsonlite::fromJSON(jsonlite::toJSON(rec$data)))
      target <- norm_name(input$athlete)
      rdf2 <- rdf %>% mutate(`_ath` = tolower(trimws(athlete))) %>% filter(`_ath` == target)
      validate(need(nrow(rdf2) > 0, "No recent entries for this athlete."))
      latest_date <- as.character(rdf2$date[1])
      incProgress(0.5, detail = 'Fetching session row')
      got <- try(api_get(list(action = "get_anthro", athlete = input$athlete, date = latest_date, cb = as.integer(Sys.time()))), silent = TRUE)
      validate(need(!inherits(got, "try-error") && isTRUE(got$ok) && length(got$data) > 0, "No session row."))
      dat <- as_tibble(jsonlite::fromJSON(jsonlite::toJSON(got$data)))
      row <- dat[1,]

      if ("practitioner" %in% names(row)) updateSelectizeInput(session, "practitioner", selected = row$practitioner)
      updateSelectizeInput(session, "athlete", selected = input$athlete)
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
      if ("scale" %in% names(row))    updatePickerInput(session, "scale", selected = row$scale)
      if ("fasted" %in% names(row))   updatePrettyRadioButtons(session, "fasted", selected = row$fasted)
      if ("day_of_cycle" %in% names(row)) updatePickerInput(session, "doc", selected = as.character(row$day_of_cycle))
      if ("birth_control" %in% names(row)) updatePrettyRadioButtons(session, "bc", selected = row$birth_control)
      if ("creatine" %in% names(row)) updatePrettyRadioButtons(session, "creatine", selected = row$creatine)
      if ("usg" %in% names(row))      updateNumericInput(session, "usg", value = suppressWarnings(as.numeric(row$usg)))
      if ("caliper" %in% names(row))  updatePickerInput(session, "caliper", selected = row$caliper)
      if ("comments" %in% names(row)) updateTextAreaInput(session, "comments", value = row$comments %||% "")

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
    rec <- fetch_recent(1000)
    dup_exists <- nrow(rec %>%
                         filter(tolower(trimws(athlete)) == tolower(trimws(out$athlete)),
                                as.character(date) == as.character(out$date))) > 0
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
    ok <- append_rows(list(out))
    if (ok) {
      output$status <- renderText("Saved to database")
      recent_data(fetch_recent_with_ts(10, progress = TRUE))
      clear_form()
    } else {
      output$status <- renderText("Error: could not save to database")
    }
  })

  observeEvent(input$confirm_replace, {
    req(!is.null(pending_row()))
    out <- pending_row()
    removeModal()
    ok <- replace_rows_for_key(out$athlete, out$date, list(out))
    if (ok) {
      output$status <- renderText("Existing record overwritten and saved to database")
      pending_row(NULL)
      recent_data(fetch_recent_with_ts(10, progress = TRUE))
      clear_form()
    } else {
      output$status <- renderText("Error: could not overwrite existing record")
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
      incProgress(0.8, detail = "Saving to Google Sheet")
      ok <- append_rows(rows)
      if (ok) {
        output$upload_status <- renderText("Batch upload complete.")
        recent_data(fetch_recent_with_ts(10, progress = FALSE))
      } else {
        output$upload_status <- renderText("Batch upload failed.")
      }
      incProgress(1, detail = "Done")
    })
  })

  # ================== REPORT TAB ==================
  report_df <- reactiveVal({
    tryCatch(fetch_recent_with_ts(200, progress = FALSE), error = function(e) {
      tibble(date = character(), athlete = character(), timestamp = character())
    })
  })

  observeEvent(input$report_reload, {
    report_df( tryCatch(fetch_recent_with_ts(200, progress = TRUE), error = function(e) {
      tibble(date = character(), athlete = character(), timestamp = character())
    }))
  })

  # Enable/disable Download CSV based on selection
  observe({
    ids <- input$report_selected
    if (is.null(ids) || length(ids) == 0) shinyjs::disable('report_csv') else shinyjs::enable('report_csv')
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

  # Download selected sessions as ONE CSV (progress + robust data pull)
  output$report_csv <- downloadHandler(
    filename = function() paste0(format(Sys.Date(), "%Y-%m-%d"), "_anthro.csv"),
    contentType = "text/csv; charset=utf-8",
    content = function(file) {
      ids <- isolate(input$report_selected)
      if (is.null(ids) || length(ids) == 0) {
        showNotification("Please select at least one row to download.", type = "warning", duration = 4)
        stop("no-selection")
      }
      df <- report_df()
      n <- nrow(df)
      sel_idx <- suppressWarnings(as.integer(gsub("^sel_", "", ids)))
      sel_idx <- sel_idx[!is.na(sel_idx) & sel_idx >= 1 & sel_idx <= n]
      if (length(sel_idx) == 0) {
        showNotification("Please select at least one row to download.", type = "warning", duration = 4)
        stop("no-valid-index")
      }

      rows <- list()
      withProgress(message = "Building CSV…", value = 0, {
        total <- length(sel_idx)
        for (k in seq_along(sel_idx)) {
          i <- sel_idx[k]
          incProgress((k - 1) / max(1, total), detail = sprintf("%s — %s", as.character(df$date[i]), as.character(df$athlete[i])))
          a <- as.character(df$athlete[i])
          d <- as.character(df$date[i])
          got <- try(api_get(list(action = "get_anthro", athlete = a, date = d, cb = as.integer(Sys.time()))), silent = TRUE)
          if (!inherits(got, "try-error") && isTRUE(got$ok) && length(got$data) > 0) {
            dat <- try(tibble::as_tibble(jsonlite::fromJSON(jsonlite::toJSON(got$data))), silent = TRUE)
            if (!inherits(dat, "try-error") && nrow(dat) > 0) {
              rows[[length(rows) + 1]] <- dat[1, ]
            }
          }
        }
        incProgress(1, detail = "Finalizing…")
      })

      out <- dplyr::bind_rows(rows)
      # Flatten any list-columns into atomic vectors (robust)
      out <- tibble::as_tibble(
        jsonlite::fromJSON(jsonlite::toJSON(out, auto_unbox = TRUE, null = "null"),
                           flatten = TRUE),
        .name_repair = "universal"
      )
      
      if (nrow(out) == 0) {
        showNotification("No data found for the selected entries.", type = "error", duration = 5)
        return(invisible(NULL))
      }
      
      readr::write_excel_csv(out, file, na = "")
    }
  )
}

shinyApp(ui, server)
