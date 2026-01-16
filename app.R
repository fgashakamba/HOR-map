# ================================================================================
# LIV Multi-Purpose Interactive Map (bindEvent Syntax + Composite Key Fix)
# ================================================================================

# 1. Load required packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(
  magrittr, dplyr, stringr, shiny, sf, 
  leaflet, bslib, shinyjs, viridis, htmltools,
  shinyscreenshot, shinyWidgets, janitor
)

# 2. Global Data Loading & Processing
# ==================================
tryCatch({
  # Adjust paths as necessary for your folder structure
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
  
}, error = function(e) {
  message("Error loading data. Please ensure the GPKG files exist in 'data_wgs84'.")
})

# 3. User Interface
# =====================================================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  useShinyjs(), 
  
  fluidRow(
    column(12,
           div(class = "title-panel p-3 mb-3 bg-primary text-white",
               h2("LIV Interactive Map")
           )
    )
  ),
  
  fluidRow(
    # --- Column 1: Settings & Selection ---
    column(3,
           wellPanel(
             style = "height: 85vh; overflow-y: auto;", 
             
             h4("1. Settings"),
             
             shinyWidgets::radioGroupButtons(
               inputId = "data_type",
               label = "Choose Data Type:",
               choices = c("Nominal" = "nominal", 
                           "Quantitative" = "quantitative"),
               selected = "nominal",
               justified = TRUE,       
               status = "secondary",     
               checkIcon = list(
                 yes = icon("check")
               )
             ),
             
             conditionalPanel(
               condition = "input.data_type == 'quantitative'",
               textInput("legend_title_input", "Legend Title:", value = "Value")
             ),
             
             hr(),
             h4("2. Select Location"),
             p(class="text-muted", style="font-size:0.9em;", 
               "Select a district to populate the data entry list."),
             
             selectizeInput("district_input", 
                            label = "Choose District:", 
                            choices = sort(unique(sectors$district)), 
                            multiple = FALSE),
             
             uiOutput("sector_selector_ui")
           )
    ),
    
    # --- Column 2: Data Entry & Actions ---
    column(3,
           wellPanel(
             style = "height: 85vh; overflow-y: auto;",
             
             h4("3. Enter Data"),
             uiOutput("dynamic_data_inputs"),
             
             hr(),
             div(class = "d-flex gap-2 mb-2",
                 actionButton("addButton", "Add/Update", class = "btn-success flex-grow-1"),
                 actionButton("clearButton", "Clear", class = "btn-danger")
             ),
             actionButton("downloadMap", "Download Image", icon = icon("camera"), class = "btn-secondary w-100"),
             
             div(class = "mt-3",
                 textOutput("status_message")
             )
           )
    ),
    
    # --- Column 3: Map Panel ---
    column(6,
           card(
             style = "height: 85vh;", 
             full_screen = TRUE,
             card_header("Operational Map"),
             card_body(
               class = "p-0",
               div(id = "map_container", style="height:100%;",
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
  
  # --- A. Reactive Storage ---
  store <- reactiveValues(
    entries = data.frame(
      sector_name = character(), 
      user_value = character(),   
      district = character(),
      stringsAsFactors = FALSE
    )
  )
  
  # Clear data if user switches between Nominal/Quantitative to prevent type conflicts
  observe({
    if(nrow(store$entries) > 0) {
      showNotification("Data type changed. Map cleared.", type = "warning")
      store$entries <- store$entries[0, ]
    }
  }) %>% 
    bindEvent(input$data_type)
  
  # --- B. Dynamic UI Generation ---
  
  # Update Sector list based on District
  observe({
    req(input$district_input)
    
    filtered_sectors_list <- sectors %>% 
      filter(district == input$district_input) %>% 
      pull(name) %>% 
      unique() %>% 
      sort()
    
    output$sector_selector_ui <- renderUI({
      selectizeInput("sector_input", 
                     label = paste("Choose Sectors in", input$district_input, ":"), 
                     choices = filtered_sectors_list, 
                     multiple = TRUE,
                     options = list(placeholder = 'Select sectors...'))
    })
  }) %>% 
    bindEvent(input$district_input)
  
  # Generate Input Fields
  output$dynamic_data_inputs <- renderUI({
    req(input$sector_input)
    
    # Create input fields list
    input_list <- lapply(input$sector_input, function(sec_name) {
      # Create unique ID using sector name
      input_id <- paste0("val_", str_replace_all(sec_name, "[^[:alnum:]]", ""))
      
      if (input$data_type == "quantitative") {
        input_widget <- numericInput(input_id, label = NULL, value = 0, width = "100%")
      } else {
        input_widget <- textInput(input_id, label = NULL, value = "", placeholder = "Value...", width = "100%")
      }
      
      fluidRow(
        class = "mb-1 align-items-center",
        column(7, div(style = "font-weight: 500; font-size: 0.9rem;", sec_name)),
        column(5, input_widget)
      )
    })
    
    do.call(tagList, input_list)
  })
  
  # --- C. Button Logic ---
  observe({
    req(input$sector_input)
    
    # 1. Collect new data from UI
    new_data <- data.frame(
      sector_name = character(), 
      user_value = character(),
      district = character(),
      stringsAsFactors = FALSE
    )
    
    for(sec in input$sector_input) {
      input_id <- paste0("val_", str_replace_all(sec, "[^[:alnum:]]", ""))
      val <- input[[input_id]]
      if(is.null(val)) val <- NA
      
      new_data <- rbind(new_data, data.frame(
        sector_name = sec, 
        user_value = as.character(val),
        district = input$district_input 
      ))
    }
    
    current_data <- store$entries
    
    # 2. Filter input sectors
    clean_data <- current_data %>% 
      filter(
        !(sector_name %in% new_data$sector_name & district == input$district_input)
      )
    
    store$entries <- rbind(clean_data, new_data)
    
    updateSelectizeInput(session, "sector_input", selected = character(0))
    showNotification("Data added to map!", type = "message")
  }) %>% 
    bindEvent(input$addButton)
  
  observe({
    store$entries <- store$entries[0, ]
    showNotification("Map cleared.", type = "error")
  }) %>% 
    bindEvent(input$clearButton)
  
  observe({
    screenshot(
      selector = "#map_container",
      filename = paste0("liv_map_", format(Sys.time(), "%Y%m%d_%H%M%S")),
      scale = 2
    )
  }) %>% 
    bindEvent(input$downloadMap)
  
  output$status_message <- renderText({
    n <- nrow(store$entries)
    if(n == 0) "Map is empty." else paste(n, "sectors currently mapped.")
  })
  
  # --- D. Map Rendering & Updates ---
  output$map <- renderLeaflet({
    bbox <- st_bbox(country)
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
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
        popup = ~paste("<b>District:</b>", name), group = "Districts"
      )%>% 
      addPolygons(
        data = country,
        fill = FALSE, color = "black", weight = 3, group = "Country Border"
      ) %>%
      fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]], lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
  })
  
  observe({
    data_df <- store$entries
    
    leafletProxy("map") %>%
      clearGroup("selected_sectors") %>%
      clearGroup("sector_labels") %>%
      clearControls()
    
    if (nrow(data_df) > 0) {
      
      # Join on name and district to handle duplicate sector names
      processed_sf <- sectors %>% 
        inner_join(data_df, by = c("name" = "sector_name", "district" = "district"))
      
      # --- COLOR LOGIC ---
      if(input$data_type == "quantitative") {
        vals_numeric <- as.numeric(processed_sf$user_value)
        pal <- colorBin(palette = "viridis", domain = vals_numeric, bins = 5)
        fill_col <- pal(vals_numeric)
        
        leafletProxy("map") %>%
          addLegend(
            pal = pal, values = vals_numeric, 
            title = input$legend_title_input, position = "bottomright", opacity = 0.7
          )
      } else {
        pal <- colorFactor(palette = "Set2", domain = processed_sf$user_value) 
        fill_col <- pal(processed_sf$user_value)
      }
      
      # --- DRAW POLYGONS ---
      leafletProxy("map", data = processed_sf) %>%
        addPolygons(
          group = "selected_sectors",
          fillColor = fill_col, fillOpacity = 0.7, color = "white", weight = 1,
          popup = ~paste0("<b>", name, " (", district, ")</b>: ", user_value)
        )
      
      # --- DRAW LABELS ---
      suppressWarnings({ centroids <- st_centroid(processed_sf) })
      
      leafletProxy("map") %>%
        addLabelOnlyMarkers(
          data = centroids,
          group = "sector_labels",
          label = ~as.character(user_value),
          labelOptions = labelOptions(
            noHide = TRUE, direction = 'center', textOnly = TRUE,
            style = list("font-weight" = "bold", "font-size" = "12px", "color" = "black")
          )
        )
      
      # --- AUTO ZOOM ---
      bbox <- st_bbox(processed_sf)
      leafletProxy("map") %>%
        flyToBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]], lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
    }
  })
}

# 5. Run App
shinyApp(ui, server)