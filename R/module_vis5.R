# ----------------------------------------------
# VISUALIZATION 5: TREEMAP
# ----------------------------------------------

# --- Module UI ---
vis5_ui <- function(id) {
  ns <- NS(id) # Namespace
  
  # Use tagList to wrap multiple UI elements
  tagList(
    h2("Visualization 5: Treemap of Internet Users"),
    hr(),
    fluidRow(
      # 1. UI INPUTS (WIDGETS)
      # We use ns() to namespace the inputId
      box(
        title = "Controls", status = "warning", solidHeader = TRUE, width = 12,
        sliderInput(
          inputId = ns("year"), # <-- NAMESPACED
          label = "Select the year:",
          min = 1990, 
          max = 2024, 
          value = 2020, 
          sep = "", 
          step = 1 
        )
      ),
      
      # 2. UI OUTPUT (VISUALIZATION)
      # We use ns() to namespace the outputId
      box(
        title = "Treemap chart", status = "primary", solidHeader = TRUE, width = 12,
        plotlyOutput(ns("treemap_plot")) # <-- NAMESPACED
      )
    )
  )
}


# --- Module Server ---
# We pass the loaded data frames into the module
vis5_server <- function(id, data_internet, data_population, data_regions) {
  
  moduleServer(id, function(input, output, session) {
    
    # All your existing render logic goes inside the moduleServer function
    output$treemap_plot <- renderPlotly({
      
      # data preparation
      
      # Note: we use input$year, NOT ns("year"). 
      # Shiny handles this automatically inside moduleServer.
      selected_year <- as.character(input$year) 
      
      # We no longer need to load data_internet(), data_population(), etc.
      # They are now just standard data frames passed as arguments.
      df_internet <- data_internet
      df_regions <- data_regions
      df_population <- data_population
      
      data_internet_prep <- df_internet %>%
        select(`Country Code`,
               `Country Name`,
               v = !!sym(selected_year)) %>%
        filter(!is.na(v),
               v > 0,
               nchar(`Country Code`) == 3)
      
      data_regions_prep <- df_regions %>%
        select(`Country Code`, Region) %>%
        filter(Region != "" & !is.na(Region))
      
      # merges internet data with region data
      data_joined <- inner_join(data_regions_prep, data_internet_prep, by = "Country Code")
      
      data_population_prep <- df_population %>%
        select(`Country Code`,p = !!sym(selected_year)) %>%
        filter(!is.na(p),
               p > 0)
      
      #merges data with population data
      data_rejoined <- inner_join(data_joined, data_population_prep, by = "Country Code")
      
      data_final <- data_rejoined %>%
        rename(
          name = `Country Name`,
          code = `Country Code`
        ) %>%
        filter(!is.na(Region), !is.na(p))
      
      # color section
      
      region_color_map <- c(
        "East Asia & Pacific"       = "#2A5783",  #blue
        "Europe & Central Asia"     = "#9E3A26",  #orange
        "Latin America & Caribbean" = "#24693D",  #green
        "Middle East & North Africa"= "#B71D3E",  #red
        "North America"             = "#7C4D79",  #purple
        "South Asia"                = "#49525E",  #gray
        "Sub-Saharan Africa"        = "#1E1E1E"   #black
      )
      
      #HIERARCHY
      
      # Level 2: Countries (Parents: Regions)
      data_countries <- data_final %>%
        filter(!is.na(v), !is.na(p), v > 0, p > 0) %>%
        mutate(
          labels = paste(name, code),
          parents = Region,
          values = (v * p) / 100,
          density = v / 100,
          base_color = region_color_map[Region],
          color = darken(base_color, amount = 0.5 * density)  
        )
      
      # Level 1: Regions 
      data_regions_hier <- data_final %>%
        distinct(Region) %>%
        mutate(
          labels = Region,
          parents = "",
          values = 0,  # temporary
          color=region_color_map[Region]
        )
      
      #region values = sum of countries values
      region_values <- data_countries %>%
        group_by(parents) %>%
        summarise(values = sum(values))
      
      data_regions_hier <- data_regions_hier %>%
        left_join(region_values, by = c("labels" = "parents")) %>%
        mutate(values = ifelse(is.na(values.y), 0, values.y)) %>%
        select(-values.x, -values.y)
      
      # Combine the levels
      data_hierarchy <- bind_rows(data_regions_hier, data_countries)
      
      #% of world total and total in the country
      tot_value <- sum(data_hierarchy$values, na.rm = TRUE)
      
      data_hierarchy <- data_hierarchy %>%
        mutate(
          percent_total = values / tot_value,
          text_label = paste0(
            labels, "\n", 
            format(round(values, 0), big.mark = ","), "\n",  #total in the country
            round(percent_total*100, 2), "%" #% of world total
          )
        )
      
      #PLOT
      plot_ly(
        data = data_hierarchy,
        type = "treemap",
        labels = ~labels,
        parents = ~parents,
        values = ~values,
        branchvalues = "total",
        texttemplate = ~text_label,
        textposition = "middle",
        hovertemplate = paste( "<b>%{label}</b><br>",
                               "Region: %{parent}<br>",
                               "Internet users: %{value:,.0f}<br>",
                               "% of world total: %{percentEntry:.2%}<br>",
                               "<extra></extra>" ),
        marker = list(
          colors = ~color,
          line = list(width = 1, color = "white")
        )
      ) %>%
        layout(
          title = paste("Internet users per region - Year", input$year),
          uniformtext = list(minsize = 10)
        )
      
    }) # end of renderPlotly
    
  }) # end of moduleServer
}