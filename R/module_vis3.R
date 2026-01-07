# ----------------------------------------------
# VISUALIZATION 3: Spatial Distribution (Q3)
# ----------------------------------------------

names(countrycode::codelist)


vis3_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Q3: Global Comparison of Digital and Educational Access"),
    p("Highlight regional disparities by comparing Internet access and Literacy rates side-by-side."),
    hr(),
    
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
    
    fluidRow(
      box(title = "Internet Access (%)", status = "primary", solidHeader = TRUE, width = 6,
          plotlyOutput(ns("map_internet"), height = "400px")),
      box(title = "Literacy Rate (%)", status = "primary", solidHeader = TRUE, width = 6,
          plotlyOutput(ns("map_literacy"), height = "400px"))
    ),
    
    fluidRow(
      box(title = "Hover indicator", status = "warning", solidHeader = TRUE, width = 12,
          plotlyOutput(ns("hover_bars"), height = "140px"))
    ),
    
    fluidRow(
      box(title = "Evolution Over Time", status = "info", solidHeader = TRUE, width = 6,
          checkboxGroupInput(
            ns("avg_overlay"),
            "Comparison lines:",
            choices = c("World average" = "world", "Region average" = "region"),
            selected = c("world"),
            inline = TRUE
          ),
          plotlyOutput(ns("mini_time_series"), height = "350px")
      ),
      # Le titre ici est statique dans l'UI mais nous allons gérer le titre du graphe dynamiquement
      box(title = "Correlation Analysis", status = "success", solidHeader = TRUE, width = 6,
          plotlyOutput(ns("correlation_scatter"), height = "350px"))
    )
    
    
  )
}

vis3_server <- function(id, data_internet, data_literacy) {
  moduleServer(id, function(input, output, session) {
    # --- Référentiel pays (ISO3 -> continent/subregion) pour filtrer exactement comme la carte
    world_ref <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
      sf::st_drop_geometry() %>%
      dplyr::transmute(
        iso3 = iso_a3,
        continent = tolower(continent),
        subregion = tolower(subregion)
      ) %>%
      dplyr::filter(!is.na(iso3), nchar(iso3) == 3)
    
    # --- Helpers: region of a selected country (based on world_ref)
    get_country_region_iso3 <- function(iso3) {
      row <- world_ref %>% dplyr::filter(iso3 == !!iso3)
      if (nrow(row) == 0) return(world_ref$iso3)
      
      cont <- row$continent[1]
      subr <- row$subregion[1]
      
      # Continents (Africa/Europe/Asia) -> continent group
      if (cont %in% c("africa", "europe", "asia")) {
        return(world_ref %>% dplyr::filter(continent == cont) %>% dplyr::pull(iso3))
      }
      
      # Americas: split more finely
      if (subr == "south america") {
        return(world_ref %>% dplyr::filter(subregion == "south america") %>% dplyr::pull(iso3))
      }
      
      # North America (broad): Northern + Central + Caribbean
      if (subr %in% c("northern america", "central america", "caribbean")) {
        return(world_ref %>%
                 dplyr::filter(subregion %in% c("northern america", "central america", "caribbean")) %>%
                 dplyr::pull(iso3))
      }
      
      # Fallback
      world_ref$iso3
    }
    
    # --- Average time series builder (Option A compatible: uses filled values)
    avg_series <- function(iso3_vec = NULL) {
      df <- full_data_all() %>%
        dplyr::filter(nchar(`Country Code`) == 3) %>%
        dplyr::filter(!is.na(Internet_val) | !is.na(Literacy_val))
      
      if (!is.null(iso3_vec)) {
        df <- df %>% dplyr::filter(`Country Code` %in% iso3_vec)
      }
      
      df %>%
        dplyr::group_by(Year) %>%
        dplyr::summarise(
          Internet_avg = mean(Internet_val, na.rm = TRUE),
          Literacy_avg = mean(Literacy_val, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::arrange(as.numeric(Year))
    }
    
    
    ns <- session$ns
    
    selection <- reactiveValues(code = NULL)
    
    hover_state <- reactiveValues(code = NULL)
    
    # Helper: Tooltip intelligent
    get_tooltip <- function(df) {
      paste0("<b>", df$`Country Name`, "</b><br>",
             "Internet: ", if_else(is.na(df$Internet_val), "No data", paste0(round(df$Internet_val, 1), "% (", df$Int_Year, ")")), "<br>",
             "Literacy: ", if_else(is.na(df$Literacy_val), "No data", paste0(round(df$Literacy_val, 1), "% (", df$Lit_Year, ")")))
    }
    
    # Helper: %||% (fallback if NULL)
    `%||%` <- function(x, y) if (is.null(x)) y else x
    
    # store last zoom/pan state
    view_state <- reactiveValues(center = NULL, scale = NULL, lonrng = NULL, latrng = NULL)
    
    apply_view_to_geo <- function(geo_base) {
      geo_out <- geo_base
      if (!is.null(view_state$center)) geo_out$center <- view_state$center
      if (!is.null(view_state$scale)) geo_out$projection <- modifyList(geo_out$projection %||% list(), list(scale = view_state$scale))
      if (!is.null(view_state$lonrng)) geo_out$lonaxis <- modifyList(geo_out$lonaxis %||% list(), list(range = view_state$lonrng))
      if (!is.null(view_state$latrng)) geo_out$lataxis <- modifyList(geo_out$lataxis %||% list(), list(range = view_state$latrng))
      geo_out
    }
    
    syncing <- reactiveVal(FALSE)
    cb_ticks_vals <- c(0, 25, 50, 75, 100)
    cb_ticks_text <- paste0(cb_ticks_vals, "%")
    
    # 1. Préparation des données
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
    
    observeEvent(input$region_focus, {
      view_state$center <- NULL; view_state$scale <- NULL; view_state$lonrng <- NULL; view_state$latrng <- NULL
    }, ignoreInit = TRUE)
    
    # 2. Rendu des cartes
    output$map_internet <- renderPlotly({
      df <- current_year_data()
      geo_cfg <- apply_view_to_geo(list(projection = list(type = 'robinson'), scope = input$region_focus, showframe = FALSE))
      p <- plot_geo(df, source = ns("map_internet")) %>%
        add_trace(z = ~Internet_val, locations = ~`Country Code`, key = ~`Country Code`,
                  colors = "Blues", zmin = 0, zmax = 100, text = get_tooltip(df), hoverinfo = "text",
                  marker = list(line = list(color = '#2980b9', width = 0.8)),
                  colorbar = list(title = "Internet (%)", tickvals = cb_ticks_vals, ticktext = cb_ticks_text, len = 0.9)) %>%
        layout(geo = geo_cfg, uirevision = input$region_focus, margin = list(l = 0, r = 0, b = 0, t = 0))
      p %>%
        event_register("plotly_relayout") %>%
        event_register("plotly_click") %>%
        event_register("plotly_doubleclick") %>%
        event_register("plotly_hover") %>%
        event_register("plotly_unhover")
      
    })
    
    output$map_literacy <- renderPlotly({
      df <- current_year_data()
      geo_cfg <- apply_view_to_geo(list(projection = list(type = 'robinson'), scope = input$region_focus, showframe = FALSE))
      p <- plot_geo(df, source = ns("map_literacy")) %>%
        add_trace(z = ~Literacy_val, locations = ~`Country Code`, key = ~`Country Code`,
                  colors = "Reds", zmin = 0, zmax = 100, text = get_tooltip(df), hoverinfo = "text",
                  marker = list(line = list(color = '#c0392b', width = 0.8)),
                  colorbar = list(title = "Literacy (%)", tickvals = cb_ticks_vals, ticktext = cb_ticks_text, len = 0.9)) %>%
        layout(geo = geo_cfg, uirevision = input$region_focus, margin = list(l = 0, r = 0, b = 0, t = 0))
      p %>%
        event_register("plotly_relayout") %>%
        event_register("plotly_click") %>%
        event_register("plotly_doubleclick") %>%
        event_register("plotly_hover") %>%
        event_register("plotly_unhover")
      
    })
    
    # Sync Zoom
    filter_geo_relayout <- function(rel) {
      if (is.null(rel)) return(NULL)
      keep_keys <- c("geo.center.lon", "geo.center.lat", "geo.projection.scale", "geo.lonaxis.range[0]", "geo.lonaxis.range[1]", "geo.lataxis.range[0]", "geo.lataxis.range[1]")
      out <- rel[names(rel) %in% keep_keys]
      if (length(out) == 0) return(NULL)
      out
    }
    
    handle_relayout <- function(source_id, target_id) {
      rel_raw <- event_data("plotly_relayout", source = source_id)
      rel <- filter_geo_relayout(rel_raw)
      if (is.null(rel)) return()
      if (!is.null(rel[["geo.center.lon"]])) view_state$center <- list(lon = rel[["geo.center.lon"]], lat = rel[["geo.center.lat"]])
      if (!is.null(rel[["geo.projection.scale"]])) view_state$scale <- rel[["geo.projection.scale"]]
      if (!is.null(rel[["geo.lonaxis.range[0]"]])) view_state$lonrng <- c(rel[["geo.lonaxis.range[0]"]], rel[["geo.lonaxis.range[1]"]])
      if (!is.null(rel[["geo.lataxis.range[0]"]])) view_state$latrng <- c(rel[["geo.lataxis.range[0]"]], rel[["geo.lataxis.range[1]"]])
      if (isTRUE(syncing())) return()
      syncing(TRUE); on.exit(syncing(FALSE), add = TRUE)
      plotlyProxy(target_id, session) %>% plotlyProxyInvoke("relayout", rel)
    }
    
    observeEvent(event_data("plotly_relayout", source = ns("map_internet")), { handle_relayout(ns("map_internet"), "map_literacy") }, ignoreInit = TRUE)
    observeEvent(event_data("plotly_relayout", source = ns("map_literacy")), { handle_relayout(ns("map_literacy"), "map_internet") }, ignoreInit = TRUE)
    
    observeEvent(event_data("plotly_hover", source = ns("map_internet")), {
      h <- event_data("plotly_hover", source = ns("map_internet"))
      if (is.null(h)) return()
      hover_state$code <- h$location %||% h$key
    }, ignoreInit = TRUE)
    
    observeEvent(event_data("plotly_hover", source = ns("map_literacy")), {
      h <- event_data("plotly_hover", source = ns("map_literacy"))
      if (is.null(h)) return()
      hover_state$code <- h$location %||% h$key
    }, ignoreInit = TRUE)
    
    
    # Clics
    observeEvent(event_data("plotly_click", source = ns("map_internet")), {
      click <- event_data("plotly_click", source = ns("map_internet"))
      if(!is.null(click)) selection$code <- if(!is.null(click$location)) click$location else click$key
    })
    observeEvent(event_data("plotly_click", source = ns("map_literacy")), {
      click <- event_data("plotly_click", source = ns("map_literacy"))
      if(!is.null(click)) selection$code <- if(!is.null(click$location)) click$location else click$key
    })
    
    output$hover_bars <- renderPlotly({
      df <- current_year_data()
      
      # Aucun pays survolé
      if (is.null(hover_state$code)) {
        return(
          plot_ly() %>%
            layout(
              annotations = list(list(
                text = "Hover a country on the maps to see its values.",
                x = 0.5, y = 0.5,
                xref = "paper", yref = "paper",
                showarrow = FALSE,
                font = list(size = 13)
              )),
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              margin = list(l = 10, r = 10, b = 10, t = 10)
            )
        )
      }
      
      row <- df %>% filter(`Country Code` == hover_state$code)
      if (nrow(row) == 0) return(plot_ly())
      
      country  <- row$`Country Name`[1]
      internet <- row$Internet_val[1]
      literacy <- row$Literacy_val[1]
      
      # Construction des barres à afficher
      bars <- tibble::tibble(
        metric = c("Internet access", "Literacy rate"),
        value  = c(internet, literacy)
      ) %>% dplyr::filter(!is.na(value))
      
      # Aucun indicateur disponible
      if (nrow(bars) == 0) {
        return(
          plot_ly() %>%
            layout(
              annotations = list(list(
                text = paste0(country, ": no data available"),
                x = 0.5, y = 0.5,
                xref = "paper", yref = "paper",
                showarrow = FALSE
              )),
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE)
            )
        )
      }
      
      # Barres jaunes
      plot_ly(
        bars,
        x = ~value,
        y = ~metric,
        type = "bar",
        orientation = "h",
        marker = list(color = "#F1C40F")
      ) %>%
        layout(
          title = paste0("Current values — ", country),
          xaxis = list(range = c(0, 100), title = "%"),
          yaxis = list(title = ""),
          margin = list(l = 110, r = 30, t = 45, b = 30)
        )
    })
    
    
    # 6. Graphique COMBINÉ
    output$mini_time_series <- renderPlotly({
      if (is.null(selection$code)) {
        return(
          plot_ly() %>%
            layout(
              annotations = list(
                text = paste(
                  "Please select a country on the map",
                  "to display the time series.",
                  sep = "<br>"
                ),
                showarrow = FALSE,
                x = 0.5, y = 0.5,
                xref = "paper", yref = "paper",
                align = "center",
                font = list(size = 12),
                width = 300
              ),
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              margin = list(l = 10, r = 10, b = 10, t = 10)
            )
        )
      }
      
      # --- Country history (raw)
      hist_int <- data_internet %>%
        filter(`Country Code` == selection$code) %>%
        pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Internet")
      
      hist_lit <- data_literacy %>%
        filter(`Country Code` == selection$code) %>%
        pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Literacy")
      
      combined_hist <- full_join(hist_int, hist_lit, by = c("Country Code", "Country Name", "Year")) %>%
        filter(as.numeric(Year) <= 2022) %>%
        filter(!is.na(Internet) | !is.na(Literacy)) %>%
        arrange(as.numeric(Year)) %>%
        mutate(Year_num = as.numeric(Year))
      
      if (nrow(combined_hist) == 0) {
        return(plot_ly() %>% layout(annotations = list(text = "No history available", showarrow = FALSE)))
      }
      
      # --- Averages (Option A, via filled full_data_all)
      years_keep <- combined_hist$Year_num
      
      show_world  <- "world"  %in% (input$avg_overlay %||% character(0))
      show_region <- "region" %in% (input$avg_overlay %||% character(0))
      
      if (show_world) {
        world_avg <- avg_series(NULL) %>%
          mutate(Year_num = as.numeric(Year)) %>%
          filter(Year_num %in% years_keep)
      }
      
      if (show_region) {
        region_iso3 <- get_country_region_iso3(selection$code)
        region_avg <- avg_series(region_iso3) %>%
          mutate(Year_num = as.numeric(Year)) %>%
          filter(Year_num %in% years_keep)
      }
      
      # --- Vertical line on selected year
      sel_year <- as.numeric(input$year_select)
      
      p <- plot_ly() %>%
        # Country
        add_trace(
          data = combined_hist, x = ~Year_num, y = ~Internet,
          name = "Internet Access (Country)",
          type = "scatter", mode = "lines+markers",
          line = list(color = "#2980b9", width = 3),
          marker = list(color = "#2980b9", size = 8),
          connectgaps = TRUE
        ) %>%
        add_trace(
          data = combined_hist, x = ~Year_num, y = ~Literacy,
          name = "Literacy Rate (Country)",
          type = "scatter", mode = "lines+markers",
          line = list(color = "#c0392b", width = 3),
          marker = list(color = "#c0392b", size = 8),
          connectgaps = TRUE
        )
      
      # World average (grey dotted)
      if (show_world) {
        p <- p %>%
          add_trace(
            data = world_avg, x = ~Year_num, y = ~Internet_avg,
            name = "Internet (World Avg)",
            type = "scatter", mode = "lines",
            line = list(color = "gray50", width = 2, dash = "dot"),
            hoverinfo = "World avg Internet: %{y:.1f}%<extra></extra>"
          ) %>%
          add_trace(
            data = world_avg, x = ~Year_num, y = ~Literacy_avg,
            name = "Literacy (World Avg)",
            type = "scatter", mode = "lines",
            line = list(color = "gray50", width = 2, dash = "dot"),
            hoverinfo = "World avg Literacy: %{y:.1f}%<extra></extra>"
          )
      }
      
      # Region average (grey dashed)
      if (show_region) {
        p <- p %>%
          add_trace(
            data = region_avg, x = ~Year_num, y = ~Internet_avg,
            name = "Internet (Region Avg)",
            type = "scatter", mode = "lines",
            line = list(color = "gray60", width = 2, dash = "dash"),
            hoverinfo = "World avg Literacy: %{y:.1f}%<extra></extra>"
          ) %>%
          add_trace(
            data = region_avg, x = ~Year_num, y = ~Literacy_avg,
            name = "Literacy (Region Avg)",
            type = "scatter", mode = "lines",
            line = list(color = "gray60", width = 2, dash = "dash"),
            hoverinfo = "Region avg Literacy: %{y:.1f}%<extra></extra>"
          )
      }
      
      p %>%
        layout(
          title = paste0("Trends: ", combined_hist$`Country Name`[1]),
          hovermode = "x unified",
          yaxis = list(range = c(0, 105), title = "%"),
          xaxis = list(title = "Year", tickformat = "d"),
          shapes = list(list(
            type = "line",
            x0 = sel_year, x1 = sel_year,
            y0 = 0, y1 = 1,
            yref = "paper",
            line = list(color = "gold", width = 2)
          )),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.35),
          margin = list(b = 110)
        )
    })
    
    
    
    # 7. Scatter Plot de CORRÉLATION dynamique avec FILTRE RÉGIONAL ROBUSTE
    output$correlation_scatter <- renderPlotly({
      
      # 1) Définition DES PAYS réellement affichés selon le scope de la carte
      scope_iso3 <- reactive({
        if (input$region_focus == "world") {
          world_ref$iso3
          
        } else if (input$region_focus %in% c("africa", "europe", "asia")) {
          world_ref %>%
            filter(continent == input$region_focus) %>%
            pull(iso3)
          
        } else if (input$region_focus == "north america") {
          world_ref %>%
            dplyr::filter(subregion %in% c("northern america", "central america", "caribbean")) %>%
            dplyr::pull(iso3)
          
        } else if (input$region_focus == "south america") {
          world_ref %>%
            filter(subregion == "south america") %>%
            pull(iso3)
          
        } else {
          world_ref$iso3
        }
      })
      
      # 2) Données corrélables, strictement alignées avec la carte
      df_plot <- current_year_data() %>%
        filter(nchar(`Country Code`) == 3) %>%                 # enlève WLD, LCN, etc.
        filter(`Country Code` %in% scope_iso3()) %>%           # ⬅️ clé : mêmes pays que la carte
        filter(!is.na(Internet_val) & !is.na(Literacy_val))    # points corrélables
      
      # Cas extrême : aucune donnée
      if (nrow(df_plot) == 0) {
        return(
          plot_ly() %>%
            layout(
              annotations = list(
                text = "No data available for this region/year",
                showarrow = FALSE,
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper"
              )
            )
        )
      }
      
      # 3) Mise en évidence du pays sélectionné
      df_plot$is_selected <- if (!is.null(selection$code)) {
        df_plot$`Country Code` == selection$code
      } else {
        FALSE
      }
      
      # 4) Titre dynamique
      display_region <- switch(
        input$region_focus,
        "world" = "Global",
        "africa" = "Africa",
        "north america" = "North America",
        "south america" = "South America",
        "asia" = "Asia",
        "europe" = "Europe"
      )
      
      # 5) Scatter Plot
      plot_ly(
        df_plot,
        x = ~Literacy_val,
        y = ~Internet_val,
        type = "scatter",
        mode = "markers",
        key = ~`Country Code`,
        marker = list(
          size = ifelse(df_plot$is_selected, 15, 8),
          color = ~Internet_val,
          colorscale = "Viridis",
          showscale = FALSE,
          line = list(
            color = ifelse(df_plot$is_selected, "black", "white"),
            width = ifelse(df_plot$is_selected, 3, 1)
          )
        ),
        text = ~paste0(
          "Country: ", `Country Name`,
          "<br>Literacy: ", round(Literacy_val, 1), "%",
          "<br>Internet: ", round(Internet_val, 1), "%"
        ),
        hoverinfo = "text"
      ) %>%
        layout(
          title = paste0(display_region, " Correlation (", input$year_select, ")"),
          xaxis = list(title = "Literacy Rate (%)", range = c(0, 105)),
          yaxis = list(title = "Internet Access (%)", range = c(0, 105)),
          margin = list(b = 50)
        )
    })
  })
}