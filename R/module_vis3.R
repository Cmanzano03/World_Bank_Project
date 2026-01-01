# ---------------------------------------------------------
# VISUALIZATION 3: Spatial Distribution & Trends (Module)
# ---------------------------------------------------------

vis3_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Q3: Global Comparison of Digital and Educational Access"),
    p("Highlight regional disparities by comparing Internet access and Literacy rates side-by-side."),
    hr(),
    
    # User controls for year selection and regional zooming
    fluidRow(
      box(title = "Map Controls", status = "warning", solidHeader = TRUE, width = 12,
          column(4,
                 sliderInput(ns("year_select"), "Select Year:",
                             min = 2000, max = 2022, value = 2018,
                             step = 1, sep = "", animate = animationOptions(interval = 1500))
          ),
          column(4,
                 selectInput(ns("region_focus"), "Region Focus (Zoom):",
                             choices = c("World" = "world",
                                         "Africa" = "africa",
                                         "North America" = "north america",
                                         "South America" = "south america",
                                         "Asia" = "asia",
                                         "Europe" = "europe"))
          ),
          column(4,
                 helpText("Missing data is filled with the last known value. See tooltip for details.")
          )
      )
    ),
    
    # Side-by-side maps for spatial comparison
    fluidRow(
      box(title = "Internet Access (%)", status = "primary", solidHeader = TRUE, width = 6,
          plotlyOutput(ns("map_internet"), height = "400px")),
      box(title = "Literacy Rate (%)", status = "primary", solidHeader = TRUE, width = 6,
          plotlyOutput(ns("map_literacy"), height = "400px"))
    ),
    
    # Combined line chart for temporal analysis of a selected country
    fluidRow(
      box(title = "Evolution of Internet Access & Literacy", status = "info", solidHeader = TRUE, width = 12,
          plotlyOutput(ns("mini_time_series"), height = "300px"))
    )
  )
}

vis3_server <- function(id, data_internet, data_literacy) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactively store the clicked country code
    selection <- reactiveValues(code = NULL)
    
    # Custom tooltip generator for better user information
    get_tooltip <- function(df) {
      paste0("<b>", df$`Country Name`, "</b><br>",
             "Internet: ", if_else(is.na(df$Internet_val), "No data", paste0(round(df$Internet_val, 1), "% (", df$Int_Year, ")")), "<br>",
             "Literacy: ", if_else(is.na(df$Literacy_val), "No data", paste0(round(df$Literacy_val, 1), "% (", df$Lit_Year, ")")))
    }
    
    `%||%` <- function(x, y) if (is.null(x)) y else x
    
    # reactiveValues to store and maintain manual zoom/pan states across updates
    view_state <- reactiveValues(
      center = NULL,
      scale  = NULL,
      lonrng = NULL,
      latrng = NULL
    )
    
    # Helper to re-apply stored view settings to the maps
    apply_view_to_geo <- function(geo_base) {
      geo_out <- geo_base
      if (!is.null(view_state$center)) geo_out$center <- view_state$center
      if (!is.null(view_state$scale)) geo_out$projection <- modifyList(geo_out$projection %||% list(), list(scale = view_state$scale))
      if (!is.null(view_state$lonrng)) geo_out$lonaxis <- modifyList(geo_out$lonaxis %||% list(), list(range = view_state$lonrng))
      if (!is.null(view_state$latrng)) geo_out$lataxis <- modifyList(geo_out$lataxis %||% list(), list(range = view_state$latrng))
      geo_out
    }
    
    cb_ticks_vals <- c(0, 25, 50, 75, 100)
    cb_ticks_text <- paste0(cb_ticks_vals, "%")
    
    # Data Processing: Pivot to long format and apply LOCF (Last Observation Carried Forward)
    full_data_all <- reactive({
      req(data_internet, data_literacy)
      int_long <- data_internet %>%
        pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Internet_val") %>%
        filter(as.numeric(Year) <= 2022)
      
      lit_long <- data_literacy %>%
        pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Literacy_val") %>%
        filter(as.numeric(Year) <= 2022)
      
      full_join(int_long, lit_long, by = c("Country Code", "Country Name", "Year")) %>%
        arrange(`Country Code`, Year) %>% 
        group_by(`Country Code`) %>%
        mutate(
          Int_Year = if_else(!is.na(Internet_val), Year, NA_character_),
          Lit_Year = if_else(!is.na(Literacy_val), Year, NA_character_)
        ) %>%
        fill(Internet_val, Int_Year, Literacy_val, Lit_Year, .direction = "down") %>%
        ungroup()
    })
    
    current_year_data <- reactive({
      full_data_all() %>%
        filter(Year == as.character(input$year_select)) %>%
        arrange(`Country Code`)
    })
    
    # Reset zoom state when the region of interest changes
    observeEvent(input$region_focus, {
      view_state$center <- NULL
      view_state$scale  <- NULL
      view_state$lonrng <- NULL
      view_state$latrng <- NULL
    }, ignoreInit = TRUE)
    
    # Render Internet Access Map
    output$map_internet <- renderPlotly({
      df <- current_year_data()
      geo_cfg <- list(projection = list(type = 'robinson'), scope = input$region_focus, showframe = FALSE)
      geo_cfg <- apply_view_to_geo(geo_cfg)
      
      plot_geo(df, source = ns("map_internet")) %>%
        add_trace(
          z = ~Internet_val, locations = ~`Country Code`, key = ~`Country Code`,
          colors = "Blues", zmin = 0, zmax = 100,
          text = get_tooltip(df), hoverinfo = "text",
          marker = list(line = list(color = '#2980b9', width = 0.8)),
          colorbar = list(title = "Internet (%)", tickmode = "array", tickvals = cb_ticks_vals, ticktext = cb_ticks_text, len = 0.9)
        ) %>%
        layout(geo = geo_cfg, uirevision = input$region_focus, margin = list(l = 0, r = 0, b = 0, t = 0))
    })
    
    # Render Literacy Rate Map
    output$map_literacy <- renderPlotly({
      df <- current_year_data()
      geo_cfg <- list(projection = list(type = 'robinson'), scope = input$region_focus, showframe = FALSE)
      geo_cfg <- apply_view_to_geo(geo_cfg)
      
      plot_geo(df, source = ns("map_literacy")) %>%
        add_trace(
          z = ~Literacy_val, locations = ~`Country Code`, key = ~`Country Code`,
          colors = "Reds", zmin = 0, zmax = 100,
          text = get_tooltip(df), hoverinfo = "text",
          marker = list(line = list(color = '#c0392b', width = 0.8)),
          colorbar = list(title = "Literacy (%)", tickmode = "array", tickvals = cb_ticks_vals, ticktext = cb_ticks_text, len = 0.9)
        ) %>%
        layout(geo = geo_cfg, uirevision = input$region_focus, margin = list(l = 0, r = 0, b = 0, t = 0) )
    })
    
    # Synchronize zoom/pan between both maps (Linked Views)
    observe({
      rel_int <- event_data("plotly_relayout", source = ns("map_internet"))
      if (!is.null(rel_int)) plotlyProxy("map_literacy", session) %>% plotlyProxyInvoke("relayout", rel_int)
    })
    observe({
      rel_lit <- event_data("plotly_relayout", source = ns("map_literacy"))
      if (!is.null(rel_lit)) plotlyProxy("map_internet", session) %>% plotlyProxyInvoke("relayout", rel_lit)
    })
    
    # Capture and update manual zoom/pan coordinates
    observeEvent(event_data("plotly_relayout", source = ns("map_internet")), {
      rel <- event_data("plotly_relayout", source = ns("map_internet"))
      if (is.null(rel)) return()
      if (!is.null(rel[["geo.center.lon"]]) && !is.null(rel[["geo.center.lat"]])) {
        view_state$center <- list(lon = rel[["geo.center.lon"]], lat = rel[["geo.center.lat"]])
      }
      if (!is.null(rel[["geo.projection.scale"]])) view_state$scale <- rel[["geo.projection.scale"]]
      if (!is.null(rel[["geo.lonaxis.range[0]"]]) && !is.null(rel[["geo.lonaxis.range[1]"]])) {
        view_state$lonrng <- c(rel[["geo.lonaxis.range[0]"]], rel[["geo.lonaxis.range[1]"]])
      }
      if (!is.null(rel[["geo.lataxis.range[0]"]]) && !is.null(rel[["geo.lataxis.range[1]"]])) {
        view_state$latrng <- c(rel[["geo.lataxis.range[0]"]], rel[["geo.lataxis.range[1]"]])
      }
    }, ignoreInit = TRUE)
    
    observeEvent(event_data("plotly_relayout", source = ns("map_literacy")), {
      rel <- event_data("plotly_relayout", source = ns("map_literacy"))
      if (is.null(rel)) return()
      if (!is.null(rel[["geo.center.lon"]]) && !is.null(rel[["geo.center.lat"]])) {
        view_state$center <- list(lon = rel[["geo.center.lon"]], lat = rel[["geo.center.lat"]])
      }
      if (!is.null(rel[["geo.projection.scale"]])) view_state$scale <- rel[["geo.projection.scale"]]
      if (!is.null(rel[["geo.lonaxis.range[0]"]]) && !is.null(rel[["geo.lonaxis.range[1]"]])) {
        view_state$lonrng <- c(rel[["geo.lonaxis.range[0]"]], rel[["geo.lonaxis.range[1]"]])
      }
      if (!is.null(rel[["geo.lataxis.range[0]"]]) && !is.null(rel[["geo.lataxis.range[1]"]])) {
        view_state$latrng <- c(rel[["geo.lataxis.range[0]"]], rel[["geo.lataxis.range[1]"]])
      }
    }, ignoreInit = TRUE)
    
    # Capture country selection via map clicks
    observeEvent(event_data("plotly_click", source = ns("map_internet")), {
      click <- event_data("plotly_click", source = ns("map_internet"))
      if(!is.null(click)) selection$code <- if(!is.null(click$location)) click$location else click$key
    })
    observeEvent(event_data("plotly_click", source = ns("map_literacy")), {
      click <- event_data("plotly_click", source = ns("map_literacy"))
      if(!is.null(click)) selection$code <- if(!is.null(click$location)) click$location else click$key
    })
    
    # Combined Time Series Plot for both indicators
    output$mini_time_series <- renderPlotly({
      req(selection$code)
      hist_int <- data_internet %>% filter(`Country Code` == selection$code) %>%
        pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Internet")
      hist_lit <- data_literacy %>% filter(`Country Code` == selection$code) %>%
        pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Literacy")
      
      combined_hist <- full_join(hist_int, hist_lit, by = c("Country Code", "Country Name", "Year")) %>%
        filter(as.numeric(Year) <= 2022) %>%
        filter(!is.na(Internet) | !is.na(Literacy)) %>%
        arrange(Year)
      
      if (nrow(combined_hist) == 0) return(plot_ly() %>% layout(annotations = list(text = "No history available", showarrow = FALSE)))
      
      sel_name <- combined_hist$`Country Name`[1]
      
      plot_ly(combined_hist, x = ~Year) %>%
        add_trace(y = ~Internet, name = "Internet Access", type = 'scatter', mode = 'lines+markers',
                  line = list(color = '#2980b9', width = 3), marker = list(color = '#2980b9', size = 8), connectgaps = TRUE) %>%
        add_trace(y = ~Literacy, name = "Literacy Rate", type = 'scatter', mode = 'lines+markers',
                  line = list(color = '#c0392b', width = 3), marker = list(color = '#c0392b', size = 8), connectgaps = TRUE) %>%
        layout(
          title = paste0("Internet Access & Literacy Rate Over Time: ", sel_name),
          hovermode = "x unified",
          yaxis = list(range = c(0, 105), title = "Percentage (%)"),
          xaxis = list(title = list(text = "Year", standoff = 25), tickformat = "d"),
          legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.32),
          margin = list(b = 95)
        )
    })
  })
}