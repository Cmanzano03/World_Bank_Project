library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(plotly)
library(tidyr)
library(colorspace)

# User interface ----
ui <- dashboardPage(
  dashboardHeader(title = "World Bank Project"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "tab_intro"),
      menuItem("Visualization 1", tabName = "tab_vis1"),
      menuItem("Visualization 2", tabName = "tab_vis2"),
      menuItem("Visualization 3", tabName = "tab_vis3"),
      menuItem("Visualization 4", tabName = "tab_vis4"),
      menuItem("Visualization 5", tabName = "tab_vis5")
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(
        tabName = "tab_intro",
        h2("Introduction"),
        hr()
      ),
      
      tabItem(
        tabName = "tab_vis1",
        h2("Visualization 1"),
        hr()
      ),
      
      tabItem(
        tabName = "tab_vis2",
        h2("Visualization 2"),
        hr()
      ),
      
      tabItem(
        tabName = "tab_vis3",
        h2("Visualization 3"),
        hr()
      ),
      
      tabItem(
        tabName = "tab_vis4",
        h2("Visualization 4"),
        hr()
      ),
      
      tabItem(
        tabName = "tab_vis5",
        h2("Visualization 5"),
        hr(),
        fluidRow(
          
          # 1. UI INPUTS (WIDGETS)
          box(
            title = "Controls", status = "warning", solidHeader = TRUE, width = 12,
            
            # Slider for selecting the year
            sliderInput(
              inputId = "year",
              label = "Select the year:",
              min = 1990, 
              max = 2024, 
              value = 2020, 
              sep = "", 
              step = 1 
            )
          ),
          
          # 2. UI OUTPUT (VISUALIZATION)
          box(
            title = "Treemap chart", status = "primary", solidHeader = TRUE, width = 12,
            
            # Output for the treemap
            plotlyOutput("treemap_plot") #output id
          )
        )
      )
    )
  )
)

# Server logic ----
server <- function(input, output, session) {
  
  # load of internet data
  data_internet <- reactive({
    
    file_path <- "data/internet.csv"
    
    # check if file exists
    if (!file.exists(file_path)) {
      stop("Error: internet.cvs not found")
    }
    
    # skip of the metadata (first 3 lines)
    df_internet <- read_csv(file_path, skip = 4)
    
    # remove the last column (empty)
    df_internet <- df_internet %>% 
      dplyr::select(-last_col())
    
    return(df_internet)
  })
  
  # load of internet data
  data_population <- reactive({
    
    file_path <- "data/population.csv"
    
    # check if file exists
    if (!file.exists(file_path)) {
      stop("Error: population.cvs not found")
    }
    
    # skip of the metadata (first 3 lines)
    df_population <- read_csv(file_path, skip = 4)
    
    # remove the last column (empty)
    df_population <- df_population %>% 
      dplyr::select(-last_col())
    
    return(df_population)
  })
  
  data_regions <- reactive({
    file_path <- "data/metadataCountry.csv"
    if (!file.exists(file_path)) {
      stop("Error: 'metadataCountry.csv' not found.")
    }
    
    df_regions <- read_csv(file_path, show_col_types = FALSE)
    
    # filter only the usefull rows
    df_regions <- df_regions %>%
      select(`Country Code`, Region) %>%
      filter(!is.na(Region) & Region != "")
    
    return(df_regions)
  })
  
  # =========================================================
  # 📊 RENDER LOGIC FOR VISUALIZATIONS (V1 - V5)
  # =========================================================
  
  # --- RENDER LOGIC FOR VISUALIZATION 1 (tab_vis1) ---
  # output$vis1_plot <- renderPlot({
  #   # 1. Access data: df <- data_internet()
  #   # 2. Access inputs: input$country_select, input$year_range
  #   # 3. Create plot (e.g., ggplot(df, ...) )
  # })

  # --- RENDER LOGIC FOR VISUALIZATION 2 (tab_vis2) ---
  # output$vis2_output <- renderPlot({
  #   # Add data processing and render function here
  # })

  # --- RENDER LOGIC FOR VISUALIZATION 3 (tab_vis3) ---
  # output$vis3_output <- renderTable({
  #   # Add data processing and render function here
  # })

  # --- RENDER LOGIC FOR VISIZATION 4 (tab_vis4) ---
  # output$vis4_output <- renderPlotly({
  #   # Add data processing and render function here
  # })

  # --- RENDER LOGIC FOR VISUALIZATION 5 (tab_vis5) ---
  output$treemap_plot <- renderPlotly({
    
    # data preparation
    
    selected_year <- as.character(input$year)
    
    df_internet <- data_internet()
    df_regions <- data_regions()
    df_population <- data_population()
    
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
    
  })
  
}

# Run app ----
shinyApp(ui = ui, server = server)