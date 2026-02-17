# ================================================================================
# HOR Multi-Purpose Interactive Map 
# ================================================================================

# 1. Load required packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(
  magrittr, dplyr, tidyr, stringr, shiny, sf, 
  leaflet, bslib, shinyjs, viridis, htmltools,
  shinyscreenshot, shinyWidgets, janitor
)

# 2. Global Data Loading & Processing
# ==================================
tryCatch({
  lakes     <- st_read(file.path(getwd(), "data_wgs84/RW_lakes.gpkg"), quiet = TRUE)
  np        <- st_read(file.path(getwd(), "data_wgs84/RW_national_parks.gpkg"), quiet = TRUE)
  country   <- st_read(file.path(getwd(), "data_wgs84/RW_country.gpkg"), quiet = TRUE)
  districts <- st_read(file.path(getwd(), "data_wgs84/RW_districts.gpkg"), quiet = TRUE)
  sectors   <- st_read(file.path(getwd(), "data_wgs84/RW_sectors.gpkg"), quiet = TRUE)
  
  lakes     <- janitor::clean_names(lakes)
  np        <- janitor::clean_names(np)
  country   <- janitor::clean_names(country)
  districts <- janitor::clean_names(districts)
  sectors   <- janitor::clean_names(sectors)
  
  lakes     %<>% st_make_valid()
  np        %<>% st_make_valid()
  country   %<>% st_make_valid()
  districts %<>% st_make_valid()
  sectors   %<>% st_make_valid()
  
  # districts$name holds district names — rename to 'district' for clarity.
  # sectors already has 'name' (sector name) and 'district' (district name).
  districts %<>% rename(district = name)
  
}, error = function(e) {
  message("Error loading data. Please ensure the GPKG files exist in 'data_wgs84'.")
})

# 3. User Interface
# =====================================================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      .match-badge      { display:inline-block; padding:2px 7px; border-radius:10px;
                          font-size:0.75rem; font-weight:600; }
      .badge-matched    { background:#d4edda; color:#155724; }
      .badge-unmatched  { background:#f8d7da; color:#721c24; }
      .preview-row      { font-size:0.85rem; padding:4px 0;
                          border-bottom:1px solid #f0f0f0; }
      .preview-row:last-child { border-bottom:none; }
      .preview-district { color:#6c757d; font-size:0.78rem; }
    "))
  ),
  
  fluidRow(
    column(12,
           div(class = "title-panel p-3 mb-3 bg-primary text-white",
               h2("HOR Interactive Map")
           )
    )
  ),
  
  fluidRow(
    # ── Column 1: Settings + Input Controls ───────────────────────────────────
    column(3,
           wellPanel(
             style = "height: 85vh; overflow-y: auto;",
             
             # 1. Settings ────────────────────────────────────────────────────
             h4("1. Settings"),
             
             shinyWidgets::radioGroupButtons(
               inputId   = "data_type",
               label     = "Data Type:",
               choices   = c("Nominal" = "nominal", "Quantitative" = "quantitative"),
               selected  = "nominal",
               justified = TRUE,
               status    = "secondary",
               checkIcon = list(yes = icon("check"))
             ),
             
             conditionalPanel(
               condition = "input.data_type == 'quantitative'",
               textInput("legend_title_input", "Legend Title:", value = "Value")
             ),
             
             conditionalPanel(
               condition = "input.data_type == 'nominal'",
               shinyWidgets::colorPickr(
                 inputId  = "nominal_color",
                 label    = "Highlight Color:",
                 selected = "#67724D",
                 swatches = c("#67724D","#E74C3C","#3498DB","#2ECC71","#F39C12","#9B59B6"),
                 update   = "save",
                 theme    = "nano"
               )
             ),
             
             # 2. Input Mode ──────────────────────────────────────────────────
             hr(),
             h4("2. Input Mode"),
             
             shinyWidgets::radioGroupButtons(
               inputId   = "input_mode",
               label     = NULL,
               choices   = c("Manual Entry" = "manual", "Upload CSV" = "csv"),
               selected  = "manual",
               justified = TRUE,
               status    = "secondary",
               checkIcon = list(yes = icon("check"))
             ),
             
             # 3a. Manual: location selectors ─────────────────────────────────
             conditionalPanel(
               condition = "input.input_mode == 'manual'",
               hr(),
               h4("3. Select Location"),
               p(class = "text-muted", style = "font-size:0.9em;",
                 "Select a district to populate the data entry list."),
               selectizeInput("district_input",
                              label    = "Choose District:",
                              choices  = sort(unique(sectors$district)),
                              multiple = FALSE),
               uiOutput("sector_selector_ui")
             ),
             
             # 3b. CSV: upload + column mapping ───────────────────────────────
             conditionalPanel(
               condition = "input.input_mode == 'csv'",
               hr(),
               h4("3. Upload CSV"),
               p(class = "text-muted", style = "font-size:0.9em;",
                 "Upload a CSV with district, sector, and statistic columns."),
               
               fileInput("csv_upload", label = NULL,
                         accept      = c("text/csv","text/comma-separated-values",".csv"),
                         buttonLabel = "Browse...",
                         placeholder = "No file selected"),
               
               uiOutput("csv_column_selectors_ui"),
               
               conditionalPanel(
                 condition = "output.csv_columns_ready",
                 conditionalPanel(
                   condition = "input.data_type == 'quantitative'",
                   shinyWidgets::radioGroupButtons(
                     inputId   = "csv_summarise_method",
                     label     = "Summarise duplicates by:",
                     choices   = c("Average" = "mean", "Total" = "sum"),
                     selected  = "mean",
                     justified = TRUE,
                     status    = "secondary",
                     checkIcon = list(yes = icon("check"))
                   )
                 ),
                 actionButton("csv_load_btn", "Load CSV to Map",
                              icon = icon("map"), class = "btn-primary w-100 mt-2")
               )
             ),
             
             # Basemap (always visible) ────────────────────────────────────────
             hr(),
             selectInput(
               inputId  = "basemap",
               label    = "Basemap:",
               choices  = c(
                 "Positron (Light)"  = "CartoDB.Positron",
                 "Dark Matter"       = "CartoDB.DarkMatter",
                 "OpenStreetMap"     = "OpenStreetMap",
                 "Satellite (Esri)"  = "Esri.WorldImagery",
                 "Topo (Esri)"       = "Esri.WorldTopoMap",
                 "Terrain (Stadia)"  = "Stadia.StamenTerrain"
               ),
               selected = "CartoDB.Positron"
             )
           )
    ),
    
    # ── Column 2: Data Entry / CSV Preview ────────────────────────────────────
    column(3,
           wellPanel(
             style = "height: 85vh; overflow-y: auto;",
             
             # Manual entry panel
             conditionalPanel(
               condition = "input.input_mode == 'manual'",
               h4("Enter Data"),
               uiOutput("dynamic_data_inputs"),
               hr(),
               div(class = "d-flex gap-2 mb-2",
                   actionButton("addButton",   "Add/Update", class = "btn-success flex-grow-1"),
                   actionButton("clearButton", "Clear",      class = "btn-danger")
               )
             ),
             
             # CSV preview panel
             conditionalPanel(
               condition = "input.input_mode == 'csv'",
               uiOutput("csv_preview_panel")
             ),
             
             # Shared footer
             hr(),
             actionButton("downloadMap", "Download Image",
                          icon = icon("camera"), class = "btn-secondary w-100"),
             div(class = "mt-3", textOutput("status_message"))
           )
    ),
    
    # ── Column 3: Map ─────────────────────────────────────────────────────────
    column(6,
           card(
             style = "height: 85vh;",
             full_screen = TRUE,
             card_header("Operational Map"),
             card_body(
               class = "p-0",
               div(id = "map_container", style = "height:100%;",
                   leafletOutput("map", height = "100%")
               )
             )
           )
    )
  )
)

# 4. Server Logic
# =====================================================
server <- function(input, output, session) {
  
  # ── A. Reactive Storage ────────────────────────────────────────────────────
  store <- reactiveValues(
    entries = data.frame(
      sector_name = character(),
      user_value  = character(),
      district    = character(),
      stringsAsFactors = FALSE
    )
  )
  
  # Separate store for CSV validation results
  csv_store <- reactiveValues(
    matched   = NULL,
    unmatched = NULL
  )
  
  # Clear everything when data type changes
  observe({
    if (nrow(store$entries) > 0)
      showNotification("Data type changed. Map cleared.", type = "warning")
    store$entries       <- store$entries[0, ]
    csv_store$matched   <- NULL
    csv_store$unmatched <- NULL
  }) %>%
    bindEvent(input$data_type)
  
  # Clear everything when switching input mode
  observe({
    store$entries       <- store$entries[0, ]
    csv_store$matched   <- NULL
    csv_store$unmatched <- NULL
  }) %>%
    bindEvent(input$input_mode, ignoreInit = TRUE)
  
  # ── B. CSV Upload Logic ────────────────────────────────────────────────────
  
  output$csv_columns_ready <- reactive({ !is.null(csv_data()) })
  outputOptions(output, "csv_columns_ready", suspendWhenHidden = FALSE)
  
  csv_data <- reactive({
    req(input$csv_upload)
    tryCatch({
      df <- read.csv(input$csv_upload$datapath,
                     stringsAsFactors = FALSE, check.names = FALSE)
      if (ncol(df) < 2) stop("CSV must have at least 2 columns.")
      df
    }, error = function(e) {
      showNotification(paste("Error reading CSV:", e$message),
                       type = "error", duration = 8)
      NULL
    })
  })
  
  # Reset csv_store on new file upload
  observe({
    csv_store$matched   <- NULL
    csv_store$unmatched <- NULL
  }) %>%
    bindEvent(input$csv_upload)
  
  # Column selectors
  output$csv_column_selectors_ui <- renderUI({
    df   <- csv_data()
    req(df)
    cols <- colnames(df)
    tagList(
      selectInput("csv_district_col", "District column:",
                  choices = cols, selected = cols[1]),
      selectInput("csv_sector_col", "Sector column:",
                  choices = cols,
                  selected = if (length(cols) >= 2) cols[2] else cols[1]),
      selectInput("csv_stat_col", "Statistic column:",
                  choices = cols,
                  selected = if (length(cols) >= 3) cols[3] else cols[1])
    )
  })
  
  # Load CSV: validate → summarise → push to store
  observe({
    df       <- csv_data()
    dist_col <- input$csv_district_col
    sec_col  <- input$csv_sector_col
    stat_col <- input$csv_stat_col
    req(df, dist_col, sec_col, stat_col)
    
    # Sanitize: sentence case + collapse internal whitespace (matches DB convention)
    sanitize <- function(x) str_squish(str_to_sentence(as.character(x)))
    
    working <- df %>%
      select(district    = all_of(dist_col),
             sector_name = all_of(sec_col),
             user_value  = all_of(stat_col)) %>%
      # Fill down blank district/sector cells (common in merged spreadsheets)
      mutate(across(c(district, sector_name),
                    ~na_if(str_trim(as.character(.)), ""))) %>%
      tidyr::fill(district,    .direction = "down") %>%
      tidyr::fill(sector_name, .direction = "down") %>%
      mutate(
        # Preserve originals for the unmatched display
        district_orig    = str_trim(as.character(district)),
        sector_name_orig = str_trim(as.character(sector_name)),
        user_value       = str_trim(as.character(user_value)),
        # Sanitized keys used for matching
        district         = sanitize(district),
        sector_name      = sanitize(sector_name)
      ) %>%
      # Drop rows where essential columns are still NA after fill
      filter(!is.na(district), !is.na(sector_name), !is.na(user_value))
    
    # Filter invalid values depending on data type
    if (input$data_type == "quantitative") {
      working <- working %>%
        mutate(user_value_num = suppressWarnings(as.numeric(user_value))) %>%
        filter(!is.na(user_value_num), user_value_num != 0) %>%
        select(-user_value_num)
    } else {
      working <- working %>%
        filter(user_value != "")
    }
    
    if (nrow(working) == 0) {
      showNotification(
        "No valid rows remain after filtering (all values were NA, empty, or zero).",
        type = "error"
      )
      return()
    }
    
    known_pairs <- sectors %>%
      st_drop_geometry() %>%
      select(district, name) %>%
      mutate(across(everything(), sanitize))
    
    unmatched   <- working %>%
      anti_join(known_pairs,
                by = c("district" = "district", "sector_name" = "name"))
    matched_raw <- working %>%
      semi_join(known_pairs,
                by = c("district" = "district", "sector_name" = "name"))
    
    if (nrow(matched_raw) == 0) {
      showNotification("No rows matched the sector database. Nothing was loaded.",
                       type = "error")
      csv_store$matched   <- data.frame()
      csv_store$unmatched <- unmatched
      return()
    }
    
    # Summarise duplicates
    if (input$data_type == "quantitative") {
      method <- input$csv_summarise_method %||% "mean"
      summarised <- matched_raw %>%
        mutate(user_value = suppressWarnings(as.numeric(user_value))) %>%
        group_by(district, sector_name) %>%
        summarise(
          user_value = if (method == "mean") round(mean(user_value, na.rm = TRUE))
          else                  sum(user_value,  na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(user_value = as.character(user_value))
    } else {
      summarised <- matched_raw %>%
        group_by(district, sector_name) %>%
        slice(1) %>%
        ungroup()
    }
    
    csv_store$matched   <- summarised
    csv_store$unmatched <- unmatched
    
    existing      <- store$entries %>%
      anti_join(summarised,
                by = c("district" = "district", "sector_name" = "sector_name"))
    store$entries <- bind_rows(existing, summarised)
    
    n_un     <- nrow(unmatched)
    n_un_sec <- if (n_un > 0)
      nrow(distinct(unmatched, district, sector_name))
    else 0L
    showNotification(
      paste0(nrow(summarised), " sector(s) loaded from CSV",
             if (n_un_sec > 0)
               paste0(" (", n_un_sec, " unmatched sector(s) — see preview)")
             else "."),
      type = "message"
    )
  }) %>%
    bindEvent(input$csv_load_btn)
  
  # ── C. CSV Preview Panel (Column 2) ───────────────────────────────────────
  output$csv_preview_panel <- renderUI({
    matched   <- csv_store$matched
    unmatched <- csv_store$unmatched
    
    if (is.null(matched) && is.null(unmatched)) {
      return(tagList(
        h4("Data Preview"),
        p(class = "text-muted", style = "font-size:0.9em;",
          "Upload a CSV and click 'Load CSV to Map' to see results here.")
      ))
    }
    
    # Sector-level counts (matched is already 1 row per sector after summarise;
    # unmatched needs dedup since it still has raw rows)
    n_match   <- if (!is.null(matched)   && nrow(matched) > 0)
      nrow(matched)
    else 0
    n_unmatch <- if (!is.null(unmatched) && nrow(unmatched) > 0)
      nrow(distinct(unmatched, district, sector_name))
    else 0
    
    tagList(
      # Header row with badge counts
      div(class = "d-flex justify-content-between align-items-center mb-2",
          h4(class = "mb-0", "Data Preview"),
          div(
            span(class = "match-badge badge-matched me-1",
                 icon("check"), " ", n_match, " matched"),
            if (n_unmatch > 0)
              span(class = "match-badge badge-unmatched",
                   icon("times"), " ", n_unmatch, " unmatched")
          )
      ),
      
      # Clear button
      actionButton("clearButton", "Clear Map",
                   icon = icon("trash"), class = "btn-danger btn-sm w-100 mb-3"),
      
      # Matched rows
      if (n_match > 0) {
        div(
          div(class = "text-muted mb-1",
              style = "font-size:0.75rem; font-weight:700; letter-spacing:0.05em;",
              "MAPPED SECTORS"),
          div(
            style = "max-height:320px; overflow-y:auto;",
            do.call(tagList, lapply(seq_len(nrow(matched)), function(i) {
              div(class = "preview-row d-flex justify-content-between align-items-center",
                  div(
                    div(style = "font-weight:500;", matched$sector_name[i]),
                    div(class = "preview-district",  matched$district[i])
                  ),
                  div(style = "font-weight:600; color:#155724;",
                      matched$user_value[i])
              )
            }))
          )
        )
      },
      
      # Unmatched toggle + aggregated rows
      if (n_unmatch > 0) {
        # Show original names; aggregate to unique district+sector pairs
        unmatched_agg <- unmatched %>%
          mutate(
            display_sector   = if ("sector_name_orig" %in% names(.)) sector_name_orig else sector_name,
            display_district = if ("district_orig"    %in% names(.)) district_orig    else district
          ) %>%
          group_by(display_district, display_sector) %>%
          slice(1) %>%
          ungroup() %>%
          arrange(display_district, display_sector)
        
        n_unmatch_agg <- nrow(unmatched_agg)
        
        tagList(
          hr(),
          shinyWidgets::materialSwitch(
            inputId = "show_unmatched",
            label   = paste0("Show ", n_unmatch_agg, " unmatched sector(s)"),
            value   = FALSE,
            status  = "danger"
          ),
          conditionalPanel(
            condition = "input.show_unmatched",
            div(
              div(class = "text-muted mb-1",
                  style = "font-size:0.75rem; font-weight:700; letter-spacing:0.05em;",
                  "UNMATCHED SECTORS (not mapped)"),
              div(
                style = "max-height:220px; overflow-y:auto;",
                do.call(tagList, lapply(seq_len(nrow(unmatched_agg)), function(i) {
                  div(class = "preview-row d-flex justify-content-between align-items-center",
                      div(
                        div(style = "font-weight:500; color:#721c24;",
                            unmatched_agg$display_sector[i]),
                        div(class = "preview-district",
                            unmatched_agg$display_district[i])
                      ),
                      span(class = "match-badge badge-unmatched", "no match")
                  )
                }))
              )
            )
          )
        )
      }
    )
  })
  
  # ── D. Dynamic UI: Manual Entry ───────────────────────────────────────────
  
  observe({
    req(input$district_input)
    filtered_sectors_list <- sectors %>%
      filter(district == input$district_input) %>%
      pull(name) %>% unique() %>% sort()
    output$sector_selector_ui <- renderUI({
      selectizeInput("sector_input",
                     label    = paste("Choose Sectors in", input$district_input, ":"),
                     choices  = filtered_sectors_list,
                     multiple = TRUE,
                     options  = list(placeholder = "Select sectors..."))
    })
  }) %>%
    bindEvent(input$district_input)
  
  output$dynamic_data_inputs <- renderUI({
    req(input$sector_input)
    input_list <- lapply(input$sector_input, function(sec_name) {
      input_id <- paste0("val_", str_replace_all(sec_name, "[^[:alnum:]]", ""))
      if (input$data_type == "quantitative") {
        input_widget <- numericInput(input_id, label = NULL, value = 0, width = "100%")
      } else {
        input_widget <- textInput(input_id, label = NULL, value = "",
                                  placeholder = "Value...", width = "100%")
      }
      fluidRow(
        class = "mb-1 align-items-center",
        column(7, div(style = "font-weight:500; font-size:0.9rem;", sec_name)),
        column(5, input_widget)
      )
    })
    do.call(tagList, input_list)
  })
  
  # ── E. Button Logic ────────────────────────────────────────────────────────
  
  observe({
    req(input$sector_input)
    new_data <- data.frame(sector_name = character(), user_value = character(),
                           district    = character(), stringsAsFactors = FALSE)
    for (sec in input$sector_input) {
      input_id <- paste0("val_", str_replace_all(sec, "[^[:alnum:]]", ""))
      val      <- input[[input_id]]
      if (is.null(val)) val <- NA
      new_data <- rbind(new_data, data.frame(
        sector_name = sec,
        user_value  = as.character(val),
        district    = input$district_input
      ))
    }
    clean_data    <- store$entries %>%
      filter(!(sector_name %in% new_data$sector_name & district == input$district_input))
    store$entries <- rbind(clean_data, new_data)
    updateSelectizeInput(session, "sector_input", selected = character(0))
    showNotification("Data added to map!", type = "message")
  }) %>%
    bindEvent(input$addButton)
  
  observe({
    store$entries       <- store$entries[0, ]
    csv_store$matched   <- NULL
    csv_store$unmatched <- NULL
    showNotification("Map cleared.", type = "warning")
  }) %>%
    bindEvent(input$clearButton)
  
  observe({
    screenshot(
      selector = "#map_container",
      filename = paste0("HOR_map_", format(Sys.time(), "%Y%m%d_%H%M%S")),
      scale    = 2
    )
  }) %>%
    bindEvent(input$downloadMap)
  
  output$status_message <- renderText({
    n <- nrow(store$entries)
    if (n == 0) "Map is empty." else paste(n, "sectors currently mapped.")
  })
  
  # ── F. Map Rendering & Updates ─────────────────────────────────────────────
  
  observe({
    leafletProxy("map") %>%
      clearTiles() %>%
      addProviderTiles(input$basemap)
  }) %>%
    bindEvent(input$basemap)
  
  output$map <- renderLeaflet({
    bbox <- st_bbox(country)
    leaflet() %>%
      addProviderTiles(isolate(input$basemap) %||% "CartoDB.Positron") %>%
      addPolygons(
        data = np,
        fillColor = "#085e27", color = "#085e27", weight = 1, fillOpacity = 0.6,
        popup = ~paste("<b>National Park:</b>", name), group = "National Parks"
      ) %>%
      addPolygons(
        data = lakes,
        fillColor = "#2CA2E6", color = "#2CA2E6", weight = 1, fillOpacity = 0.6,
        popup = ~paste("<b>Lake:</b>", name), group = "Lakes"
      ) %>%
      addPolygons(
        data = districts,
        fillColor = "#808080", fillOpacity = 0.1, color = "#444444", weight = 1,
        popup = ~paste("<b>District:</b>", district), group = "Districts"
      ) %>%
      addPolygons(
        data = country,
        fill = FALSE, color = "black", weight = 3, group = "Country Border"
      ) %>%
      fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
                lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
  })
  
  observe({
    data_df <- store$entries
    leafletProxy("map") %>%
      clearGroup("selected_sectors") %>%
      clearGroup("sector_labels") %>%
      clearControls()
    
    if (nrow(data_df) > 0) {
      processed_sf <- sectors %>%
        inner_join(data_df, by = c("name" = "sector_name", "district" = "district"))
      
      if (input$data_type == "quantitative") {
        vals_numeric <- as.numeric(processed_sf$user_value)
        pal          <- colorBin(palette = "viridis", domain = vals_numeric, bins = 5)
        fill_col     <- pal(vals_numeric)
        leafletProxy("map") %>%
          addLegend(pal = pal, values = vals_numeric,
                    title    = input$legend_title_input,
                    position = "bottomright", opacity = 0.7)
      } else {
        fill_col <- input$nominal_color %||% "#67724D"
      }
      
      leafletProxy("map", data = processed_sf) %>%
        addPolygons(
          group        = "selected_sectors",
          fillColor    = fill_col, fillOpacity = 0.7,
          color        = "white", weight = 1,
          popup        = ~paste0("<b>", name, " (", district, ")</b>: ", user_value),
          label        = ~lapply(
            paste0("<b>", name, " (", district, ")</b>: ", user_value),
            htmltools::HTML),
          labelOptions = labelOptions(
            style    = list("font-weight" = "normal", "padding" = "4px 8px"),
            textsize = "13px", direction = "auto"
          )
        )
      
      suppressWarnings({ centroids <- st_centroid(processed_sf) })
      leafletProxy("map") %>%
        addLabelOnlyMarkers(
          data  = centroids, group = "sector_labels",
          label = ~as.character(user_value),
          labelOptions = labelOptions(
            noHide    = TRUE, direction = "center", textOnly = TRUE,
            style     = list("font-weight" = "bold",
                             "font-size"   = "12px",
                             "color"       = "black")
          )
        )
      
      bbox <- st_bbox(processed_sf)
      leafletProxy("map") %>%
        flyToBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
                    lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
    }
  })
}

# 5. Run App
shinyApp(ui, server)