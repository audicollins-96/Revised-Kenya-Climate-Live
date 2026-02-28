# --- 1. THE TOOLS (Libraries) ---
# These are the R packages required to run the dashboard.
library(shiny)           # The core framework for building the web application.
library(tidyverse)       # Used for data manipulation (filtering, grouping, etc.).
library(leaflet)         # The engine used to draw the interactive Kenya map.
library(leaflet.extras)  # Adds the 'pulsing' marker effects to the map.
library(sf)              # Handles 'Spatial' data (the borders of Kenyan counties).
library(DT)              # Draws the professional, searchable data table.
library(bslib)           # Applies the visual theme.
library(lubridate)       # Handles time zones and date calculations (like 'Last 7 Days').
library(jsonlite)        # Allows R to read the live forecast data from the internet.

# This is the URL for the data stored on GitHub.
# format(Sys.time...) ensures the URL has no spaces, preventing "Connection Errors."
DATA_URL <- paste0("https://raw.githubusercontent.com/audicollins-96/Revised-Kenya-Climate-Live/main/weather_history.csv?t=", 
                   format(Sys.time(), "%Y%m%d%H%M%S"))

# --- 2. THE USER INTERFACE (UI) ---
ui <- fluidPage(
  # FIXED: This 'title' ensures the link preview on other platforms is clean text.
  title = "Kenya Weather & Climate Live", 
  
  # Setting a Dark theme with an Orange primary color to match the map colours.
  theme = bs_theme(bootswatch = "darkly", primary = "#ff8c00", success = "#ff4500"),
  
  # Custom CSS: This controls the "Orange Fusion" visual style.
  tags$head(tags$style(HTML("
    /* Makes the main title Orange, Bold, and adds a shadow for depth */
    .title-style { color: #ff8c00; font-weight: bold; text-shadow: 2px 2px 4px #000; }
    
    /* BALANCE FIX: Sets Sidebar height to exactly 80% of the screen (80vh) */
    .well { 
      background: rgba(20, 10, 0, 0.9) !important; 
      border: 2px solid #ff8c00 !important; 
      height: 80vh !important; 
      overflow-y: auto; 
      box-shadow: 0px 0px 15px rgba(255, 140, 0, 0.3);
      margin-bottom: 0px;
    }
    
    /* TAB STYLING: Makes the navigation tabs Dark Orange to match the theme */
    .nav-tabs .nav-link.active { 
      background-color: #ff8c00 !important; 
      color: black !important; 
      border-color: #ff8c00 !important; 
      font-weight: bold;
    }
    .nav-tabs .nav-link { color: #ff8c00 !important; }

    /* Styles the data boxes with an Orange left border for clarity */
    .info-box { 
      padding: 10px; 
      margin-bottom: 8px; 
      border-left: 4px solid #ff8c00; 
      background: #1a1a1a; 
      border-radius: 4px; 
    }
    
    /* Styles the forecast boxes with a deep Orange-Red border */
    .forecast-box { 
      font-size: 0.85em; 
      background: #000; 
      padding: 8px; 
      border-radius: 4px; 
      margin-top: 5px; 
      border: 1px solid #ff4500; 
    }
    
    /* Custom Scrollbar for the sidebar: Modern and Orange */
    .well::-webkit-scrollbar { width: 8px; }
    .well::-webkit-scrollbar-track { background: #1a1a1a; }
    .well::-webkit-scrollbar-thumb { background: #ff8c00; border-radius: 10px; }

    /* Styles for the map popups to match the dashboard theme */
    .leaflet-popup-content-wrapper { background: #1a0a00 !important; color: #fff !important; border: 2px solid #ff8c00; }
    .leaflet-popup-tip { background: #ff8c00 !important; }
    
    /* Footer text for credits */
    .footer-text { font-size: 0.8em; color: #aaa; margin-top: 20px; line-height: 1.5; }
  "))),
  
  # Displays the main header on the page with the orange span style.
  titlePanel(span("Kenya Weather & Climate Live", class = "title-style")),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filters and Insights", style = "color: #ff8c00;"), 
      
      # Toggle between Live Snapshot (Last Hour) and Historical Range (User dates).
      radioButtons("map_mode", "Map Mode:",
                   choices = c("Live (Now)" = "live", "Range (Avg)" = "range"),
                   selected = "live", inline = TRUE),
      
      # Choose between Temperature, Humidity, or Rainfall.
      selectInput("var_select", "Metric:", 
                  choices = c("Temperature (°C)" = "avg_temp", "Humidity (%)" = "avg_hum", "Rainfall (mm)" = "total_rain"),
                  selected = "avg_temp"),
      
      # Dropdown to focus the map on a specific county.
      selectInput("county_filter", "County Focus:", choices = NULL),
      
      # Safety Filter: Prevents the app from crashing by limiting data processing.
      dateRangeInput("date_range", "Historical Range:",
                     start = Sys.Date() - 7, end = Sys.Date(),
                     min = "2026-03-01", max = Sys.Date() + 1),
      
      hr(style = "border-top: 1px solid #ff8c00;"),
      
      # These placeholders are filled by the Server logic below.
      uiOutput("dynamic_header"), 
      uiOutput("stats_sidebar"),  
      hr(style = "border-top: 1px solid #ff4500;"),
      uiOutput("forecast_sidebar"),
      
      hr(style = "border-top: 1px solid #ff8c00;"),
      
      # The main action button to pull new data from GitHub manually.
      actionButton("refresh", "Sync Live Data", icon = icon("fire"), 
                   style = "width: 100%; background-color: #ff8c00; color: white; border: none; font-weight: bold;"),
      
      div(class = "footer-text",
          hr(),
          p("Visualization: Collins Audi Owuor"),
          p("Data Source: Open-Meteo NWP Models") 
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Heatmap", 
                 div(style = "border: 2px solid #ff8c00; margin-top: 10px; box-shadow: 0px 0px 15px rgba(255, 140, 0, 0.2);",
                     # BALANCE FIX: Map height matches sidebar height perfectly.
                     leafletOutput("map", height = "calc(80vh - 45px)"))),
        tabPanel("Monthly Aggregates", 
                 div(style = "margin-top: 10px; height: calc(80vh - 45px); overflow-y: auto;", 
                     h4("Monthly Aggregate Records", style="color:#ff8c00;"),
                     DTOutput("ranking_table")))
      )
    )
  )
)

# --- 3. THE SERVER ---
server <- function(input, output, session) {
  
  # Function to clean names (e.g., 'Nairobi' to 'NAIROBI') for joining datasets.
  clean_name <- function(x) toupper(gsub("[^[:alnum:]]", "", as.character(x)))
  
  # Loads the county boundary shapefiles from the folder.
  kenya_shapes <- reactive({
    # Reads the map file.
    shapes <- st_read("ken_admin1.shp", quiet = TRUE)
    # Finds the column containing county names.
    name_col <- grep("adm1_name", names(shapes), ignore.case = TRUE, value = TRUE)[1]
    shapes %>%
      mutate(county_label = .[[name_col]], 
             join_key = clean_name(county_label),
             # Fixes names to ensure the map file matches the CSV data.
             join_key = recode(join_key, "KEIYOMARAKWET" = "ELGEYOMARAKWET", "THARAKA" = "THARAKANITHI"))
  })
  
  # The Master Data Harvester: Fetches the CSV from GitHub.
  weather_raw <- reactive({
    # Triggers refresh when button is clicked.
    input$refresh
    # Sets auto-refresh timer for 10 minutes.
    invalidateLater(600000) 
    # Downloads the CSV data from your GitHub link.
    read_csv(DATA_URL, show_col_types = FALSE) %>%
      mutate(
        # Converts text dates into real R dates.
        timestamp = ymd_hms(timestamp, quiet = TRUE),
        # Adjusts time to Kenya local time.
        timestamp = with_tz(timestamp, tzone = "Africa/Nairobi"),
        # Prepares county names for joining.
        join_key = clean_name(county)
      ) %>%
      # Removes any rows that have broken dates.
      filter(!is.na(timestamp))
  })
  
  # MAP LOGIC: Switches between Live Observation and Historical Average.
  map_data <- reactive({
    req(weather_raw())
    if(input$map_mode == "live") {
      # LIVE: Grabs the very last hour recorded for each county.
      weather_raw() %>%
        group_by(county) %>%
        filter(timestamp == max(timestamp)) %>%
        summarise(avg_temp = last(temp_c), avg_hum = last(humidity_pct), 
                  total_rain = last(precip_mm), lat = last(lat), lon = last(lon),
                  join_key = last(join_key)) %>% ungroup()
    } else {
      # RANGE: Calculates the mathematical MEAN and SUM across the selected dates.
      weather_raw() %>%
        filter(as.Date(timestamp) >= input$date_range[1] & as.Date(timestamp) <= input$date_range[2]) %>%
        group_by(join_key, county) %>%
        summarise(avg_temp = mean(temp_c, na.rm=T), avg_hum = mean(humidity_pct, na.rm=T),
                  total_rain = sum(precip_mm, na.rm=T), lat = last(lat), lon = last(lon)) %>% ungroup()
    }
  })
  
  # Automatically fills the dropdown menu with county names from your data.
  observe({
    req(weather_raw()) 
    counties <- sort(unique(weather_raw()$county))
    updateSelectInput(session, "county_filter", choices = c("National (All Counties)" = "ALL", counties))
  })
  
  # --- THE 7-DAY CALCULATION UPDATE ---
  sidebar_stats_data <- reactive({
    req(weather_raw())
    # Filters data to only include the last 7 calendar days.
    df <- weather_raw() %>% filter(timestamp >= (now() - days(7)))
    # If a specific county is selected, filter to that county only.
    if(input$county_filter != "ALL") { df <- df %>% filter(county == input$county_filter) }
    
    # CALCULATES: Mean for Temp/Humidity, but SUM for Rainfall (True cumulative total).
    df %>% summarise(t = round(mean(temp_c, na.rm=T), 1), 
                     h = round(mean(humidity_pct, na.rm=T), 1),
                     r = round(sum(precip_mm, na.rm=T), 1))
  })
  
  # Updates the title above the sidebar boxes.
  output$dynamic_header <- renderUI({
    title <- if(input$county_filter == "ALL") "National Aggregates (Past 7 Days)" else paste(input$county_filter, "7-Day Context")
    h4(title, style = "color: #ff8c00; font-size: 1em;")
  })
  
  # Builds the stats boxes for the sidebar.
  output$stats_sidebar <- renderUI({
    req(sidebar_stats_data())
    s <- sidebar_stats_data()
    tagList(
      div(class="info-box", p("Temperature (Avg): ", span(s$t, "°C", style="color:#ff8c00; font-weight:bold;"))),
      div(class="info-box", p("Humidity (Avg): ", span(s$h, "%", style="color:#ff8c00; font-weight:bold;"))),
      div(class="info-box", p("Rainfall (7D Total): ", span(s$r, "mm", style="color:#ff8c00; font-weight:bold;")))
    )
  })
  
  # Future 3-Day Forecast: Fetches projected weather from Open-Meteo API.
  output$forecast_sidebar <- renderUI({
    req(input$county_filter != "ALL", weather_raw())
    # Grabs coordinates for the selected county.
    loc <- weather_raw() %>% filter(county == input$county_filter) %>% slice(1)
    # ACCURACY CHECK: Connects to the Open-Meteo Daily Forecast Engine.
    api_url <- paste0("https://api.open-meteo.com/v1/forecast?latitude=", loc$lat, 
                      "&longitude=", loc$lon, "&daily=temperature_2m_max,precipitation_sum&timezone=auto")
    # Tries to read the JSON forecast data.
    forecast <- tryCatch({ fromJSON(api_url)$daily }, error = function(e) return(NULL))
    req(forecast)
    tagList(
      h4("Projected 3-Day Outlook", style = "color: #ff4500; font-size: 1em;"),
      lapply(1:3, function(i) {
        # Loops through the next 3 days of forecast data.
        div(class="forecast-box", 
            p(strong(forecast$time[i]), " | Max: ", forecast$temperature_2m_max[i], "°C | Rain: ", forecast$precipitation_sum[i], "mm"))
      })
    )
  })
  
  # Draws the basic dark-mode map of Kenya.
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>% 
      setView(lng = 37.9, lat = 0.5, zoom = 6)           
  })
  
  # THE MAP: Redraws the county shapes and colors based on your filters.
  observe({
    req(map_data(), kenya_shapes(), input$var_select)
    
    # Links the boundary shapes with the weather numbers.
    map_ready <- kenya_shapes() %>% left_join(map_data(), by = "join_key")
    
    # If a county is selected, only that county is drawn on the map.
    if(input$county_filter != "ALL") {
      map_ready <- map_ready %>% filter(county == input$county_filter)
    }
    
    # Sets the color intensity for the heatmap.
    full_range <- if(input$var_select == "avg_temp") c(10, 45) else if(input$var_select == "avg_hum") c(0, 100) else c(0, 50)
    pal_type <- if(input$var_select == "total_rain") "Blues" else if(input$var_select == "avg_hum") "Greens" else "YlOrRd"
    temp_pal <- colorNumeric(palette = pal_type, domain = full_range, na.color = "transparent")
    
    # Updates the existing map without reloading the whole page.
    proxy <- leafletProxy("map", data = map_ready) %>% clearShapes() %>% clearMarkers()
    
    # Zooms camera to the focused area.
    if(input$county_filter != "ALL") {
      req(nrow(map_ready) > 0)
      proxy %>% setView(lng = map_ready$lon, lat = map_ready$lat, zoom = 8)
    } else {
      proxy %>% setView(lng = 37.9, lat = 0.5, zoom = 6)
    }
    
    label_prefix <- if(input$map_mode == "live") "Live Observation: " else "Historical Mean: "
    
    proxy %>%
      addPolygons(
        fillColor = ~temp_pal(get(input$var_select)),
        weight = 1, opacity = 1, color = "white", fillOpacity = 0.7,
        # Populates the popup when a county is clicked.
        popup = ~paste0(
          "<div style='border-bottom: 2px solid #ff8c00; padding-bottom: 5px; margin-bottom: 5px;'>",
          "<b style='color:#ff8c00; font-size:16px;'>", county_label, "</b></div>",
          "<i style='color:#ffa500;'>", label_prefix, "</i><br>",
          "🌡️ Temp: ", round(avg_temp, 1), "°C<br>",
          "💧 Humid: ", round(avg_hum, 1), "%<br>",
          "🌧️ Rain: ", round(total_rain, 1), "mm"
        ),
        highlightOptions = highlightOptions(weight = 4, color = "#ff8c00", bringToFront = TRUE)
      ) %>%
      # Adds the animated orange heartbeat markers.
      addPulseMarkers(
        lng = ~lon, lat = ~lat,
        label = ~paste0(county, ": ", round(get(input$var_select), 1)),
        icon = makePulseIcon(color = "#ff8c00", heartbeat = 0.8, iconSize = 6)
      )
  })
  
  # HISTORICAL TABLE: Groups data by month to show long-term trends.
  output$ranking_table <- renderDT({
    req(weather_raw())
    monthly_summary <- weather_raw() %>%
      # 1. 'floor_date' groups all data from Feb into one 'bucket' (Feb 1st).
      # 2. 'as.Date' prevents the UTC/Nairobi midnight timezone error.
      mutate(Month_Date = floor_date(as.Date(timestamp), "month")) %>%
      group_by(Month_Date, county) %>%
      # Calculates the mathematical summaries for each month.
      summarise(Avg_Temp = round(mean(temp_c, na.rm=T), 1),
                Avg_Humidity = round(mean(humidity_pct, na.rm=T), 1),
                Total_Rainfall = round(sum(precip_mm, na.rm=T), 1), .groups = 'drop') %>%
      # This line makes the row say "February 2026" or "March 2026" instead of a timestamp.
      mutate(Month = format(Month_Date, "%B %Y")) %>%
      # We sort by the hidden 'Month_Date' (descending) to keep history in order.
      arrange(desc(Month_Date), county) %>%
      select(Month, county, Avg_Temp, Avg_Humidity, Total_Rainfall)
    
    # Renders the interactive searchable table.
    datatable(monthly_summary, options = list(pageLength = 10, scrollX = TRUE), 
              style = 'bootstrap4', class = 'table-dark table-hover')
  })
}

# --- 4. EXECUTION ---
# Runs the application.
shinyApp(ui, server)