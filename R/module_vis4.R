# R/module_vis4.R
# (Make sure app.R loads: shiny, shinydashboard, plotly, dplyr, tidyr, scales, stringr, readr)

# ----------------------------------------------
# VISUALIZATION 4: Clustering (Q4)
# ----------------------------------------------

# --- Module UI ---
vis4_ui <- function(id) {
  ns <- NS(id) # Namespace
  tagList(
    h2("Q4 — 2D Clustering (GDP+Internet) per Year + Coordinated Views"),
    hr(),
    fluidRow(
      
      # --- Control Box (former sidebar) ---
      box(
        title = "Controls", status = "warning", solidHeader = TRUE, width = 4, # 4 columns
        
        sliderInput(ns("year"), "Year:", min = 2000, max = 2020, value = 2018, step = 1, sep = ""),
        sliderInput(ns("q_out"), "Outlier percentile (distance to centroid):",
                    min = 90, max = 99, value = 95, step = 1),
        radioButtons(ns("xscale"), "GDP Scale (X-axis):", inline = TRUE,
                     choices = c("log" = "log", "linear" = "linear"), selected = "log"),
        checkboxInput(ns("show_labels"), "Show outlier labels", value = TRUE),
        helpText("Triangles = outliers by distance in the standardized space (internet, log(GDP))."),
        
        # --- Coordinated Bar Chart ---
        br(),
        h4("Country Profile (hover):"),
        plotlyOutput(ns("barchart_profile"), height = "220px"),
        
        # --- Summary ---
        br(),
        h4("Cluster Summary:"),
        verbatimTextOutput(ns("summary"))
      ),
      
      # --- Main Plot Box ---
      box(
        title = "K=3 Clustering Scatter Plot", status = "primary", solidHeader = TRUE, width = 8, # 8 columns
        # 'source' is key for the hover to work
        plotlyOutput(ns("scatter"), height = "640px") 
      )
    )
  )
}


# --- Module Server ---
# (We receive the 4 data frames from the main app.R)
vis4_server <- function(id, data_internet, data_gdp, data_literacy, data_metadata) {
  
  moduleServer(id, function(input, output, session) {
    
    # 1) FILTER AND JOIN BY YEAR
    data_year <- reactive({
      
      # Clean the metadata we receive
      data_metadata_clean <- data_metadata %>%
        select(
          country_code = `Country Code`,
          region = Region,
          income = `IncomeGroup`
        ) %>%
        filter(region != "" & !is.na(region), !is.na(income))
      
      selected_year_str <- as.character(input$year)
      
      if (!all(c(selected_year_str) %in% names(data_internet),
               c(selected_year_str) %in% names(data_gdp))) {
        
        output$summary <- renderPrint({ cat(paste("Error: Year", selected_year_str, "not found in CSV files.")) })
        return(NULL)
      }
      
      df_internet <- data_internet %>%
        select(country_code = `Country Code`,
               country = `Country Name`,
               internet = !!sym(selected_year_str))
      
      df_gdp <- data_gdp %>%
        select(country_code = `Country Code`,
               gdp = !!sym(selected_year_str))
      
      df_literacy <- data_literacy %>%
        select(country_code = `Country Code`,
               literacy = !!sym(selected_year_str))
      
      df_joined <- data_metadata_clean %>%
        inner_join(df_internet, by = "country_code") %>%
        inner_join(df_gdp, by = "country_code") %>%
        drop_na(internet, gdp) %>%
        left_join(df_literacy, by = "country_code") %>%
        mutate(
          gdp_log = log10(pmax(gdp, 1))
        )
      
      return(df_joined)
    })
    
    # 2) K-means (k=3) on 2D variables (with Z-Scores)
    clustered <- reactive({
      df <- data_year()
      req(df) 
      
      if (nrow(df) < 5) {
        return(
          df %>% mutate(cluster = NA, dist_center = NA, cluster_name = "N/A",
                        outlier = FALSE, internet_z = NA, gdp_log_z = NA)
        )
      }
      
      X <- scale(df[, c("internet", "gdp_log")])
      set.seed(123) # reproducible
      km <- kmeans(X, centers = 3, nstart = 50)
      
      # Centroid distance (in standardized space)
      centers <- km$centers[km$cluster, , drop = FALSE]
      d <- sqrt(rowSums((X - centers)^2))
      
      df$cluster <- km$cluster
      
      df$internet_z <- X[, "internet"]
      df$gdp_log_z  <- X[, "gdp_log"]
      
      # 3) Name clusters by "development" (mean of the 2 standardized vars)
      cl_means <- aggregate(X, by = list(cluster = km$cluster), FUN = mean)
      cl_means$dev_score <- rowMeans(cl_means[, c("internet", "gdp_log")])
      
      # Order: 1=high, 2=medium, 3=low
      ord <- cl_means %>% arrange(desc(dev_score)) %>% mutate(rank = row_number())
      # Map cluster->name
      name_map <- setNames(c("High development", "Medium development", "Low development"), ord$cluster)
      
      df$cluster_name <- name_map[as.character(df$cluster)]
      df$dist_center  <- as.numeric(d)
      
      # Outlier threshold by percentile
      thr <- quantile(df$dist_center, probs = input$q_out/100, na.rm = TRUE)
      df$outlier <- df$dist_center >= thr
      
      df
    })
    
    # 4) Scatter Plot (with hover)
    output$scatter <- renderPlotly({
      df <- clustered()
      req(df, nrow(df) > 0)
      
      df$literacy_label <- ifelse(
        is.na(df$literacy), 
        "N/A", 
        paste0(round(df$literacy, 1), "%")
      )
      
      df$hover <- paste0(
        "<b>", df$country, "</b>",
        "<br><b>Cluster (K-Means): ", df$cluster_name, "</b>",
        "<br><b>Income Group (WB): ", df$income, "</b>", 
        "<br>Region: ", df$region,
        "<br>---",
        "<br>Internet: ", round(df$internet, 1), "%",
        "<br>GDP pc: ", dollar_format(prefix = "$", largest_with_cents = 1)(df$gdp),
        "<br>Literacy: ", df$literacy_label, 
        "<br>Centroid dist: ", round(df$dist_center, 2)
      )
      
      df$symbol <- ifelse(df$outlier, "triangle-up", "circle")
      df$size   <- ifelse(df$outlier, 12, 9)
      
      p <- plot_ly(
        df,
        source = "scatter_plot", # ID for hover
        x = ~gdp, y = ~internet,
        type = "scatter", mode = "markers",
        text = ~hover, hoverinfo = "text",
        color = ~cluster_name,
        colors = c("High development" = "#1b9e77",
                   "Medium development" = "#7570b3",
                   "Low development" = "#d95f02"),
        symbol = ~symbol,
        symbols = c("circle", "triangle-up"),
        marker = list(line = list(width = 0.5, color = "rgba(0,0,0,0.3)")),
        customdata = ~country
      )
      
      # Optional labels on outliers
      if (isTRUE(input$show_labels)) {
        p <- p %>% add_text(
          data = df %>% filter(outlier),
          x = ~gdp, y = ~internet,
          text = ~country, textposition = "top center",
          showlegend = FALSE,
          hovertext = ~hover,
          hoverinfo = "text"
        )
      }
      
      p %>%
        layout(
          title = paste0("K=3 Clustering (recalculated) — Year ", input$year,
                         " • Outliers: p≥", input$q_out, "%"),
          xaxis = list(title = "GDP per capita (US$)",
                       type = ifelse(input$xscale == "log", "log", "linear"),
                       tickprefix = ifelse(input$xscale == "log", "", "$")),
          yaxis = list(title = "Internet users (%)", range = c(0, 100)),
          legend = list(title = list(text = "<b>Group</b>")),
          margin = list(t = 120)
        )
    })
    
    # 5) Summary
    output$summary <- renderPrint({
      df <- clustered()
      req(df) 
      
      if (!nrow(df)) return(cat("No data for selected year."))
      cat("Countries:", nrow(df), "\n")
      cat("Outliers (", input$q_out, "%): ", sum(df$outlier), "\n\n", sep = "")
      cat("Cluster means (original values):\n")
      
      print(df %>%
              group_by(cluster_name) %>%
              summarise(
                n = n(),
                internet_avg = round(mean(internet, na.rm = TRUE), 1),
                gdp_avg = round(mean(gdp, na.rm = TRUE))
              ) %>% arrange(desc(internet_avg)))
    })
    
    # --- Coordinated Barchart Logic ---
    
    # 6) Store hovered country
    pais_seleccionado_hover <- reactiveVal(NULL)
    
    # ('source' must match the 'source' in plot_ly)
    observeEvent(event_data("plotly_hover", source = "scatter_plot"), {
      pais <- event_data("plotly_hover", source = "scatter_plot")$customdata
      pais_seleccionado_hover(pais)
    })
    
    observeEvent(event_data("plotly_unhover", source = "scatter_plot"), {
      pais_seleccionado_hover(NULL)
    })
    
    # 7) Coordinated bar chart
    output$barchart_profile <- renderPlotly({
      
      df_completo <- clustered()
      pais <- pais_seleccionado_hover()
      
      if (is.null(pais) || is.null(df_completo)) {
        return(NULL)
      }
      
      datos_perfil <- df_completo %>%
        filter(country == pais) %>%
        select(internet_z, gdp_log_z) %>%
        rename(Internet = internet_z, `GDP (log)` = gdp_log_z) %>%
        pivot_longer(everything(), names_to = "indicador", values_to = "z_score")
      
      g <- ggplot(datos_perfil, aes(x = indicador, y = z_score, fill = indicador)) +
        geom_col() +
        coord_flip() +
        labs(
          title = paste("Z-score Profile for", pais),
          x = NULL, y = "Standardized Value (Z-score)"
        ) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
        theme_minimal() +
        theme(legend.position = "none")
      
      ggplotly(g, tooltip = "y")
    })
    
  }) # End moduleServer
}