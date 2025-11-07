# ----------------------------------------------
# VISUALIZATION 2: Dynamic Bubble Scatterplot (Q2)
# ----------------------------------------------

# --- Module UI ---
vis2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Q2: Correlation of Internet Access with Socioeconomic Indicators"),
    hr(),
    fluidRow(
      # --- Controls Box (Width 4 for filters) ---
      # NOTA: solidHeader = FALSE es el valor por defecto, así que lo omitimos
      # para obtener el estilo de la Imagen 1.
      box(title = "Plot Controls", status = "warning", solidHeader = TRUE, width = 4, 
          
          # 1. Year Slider
          sliderInput(
            ns("year_select"),
            "Select Year:",
            min = 2000, max = 2020, value = 2020,
            step = 1, 
            sep = "", # Removes the comma from the year
            animate = animationOptions(interval = 1500, loop = TRUE)
          ),
          
          # 2. Y-Axis Selector
          selectInput(
            ns("y_var"),
            "Select Y-Axis Variable:",
            choices = c(
              "GDP Per Capita (Current US$)" = "GDP_Per_Capita",
              "Female Employment (% of Female Labor Force)" = "Female_Employment_Perc"
            ),
            selected = "GDP_Per_Capita"
          ),
          
          # 3. Toggle for Log/Linear GDP Axis
          conditionalPanel(
            condition = "input.y_var == 'GDP_Per_Capita'",
            ns = ns, 
            checkboxInput(
              ns("log_gdp_toggle"),
              "Use Log10 Scale for GDP",
              value = TRUE 
            )
          ),
          
          # 4. Region Selector
          selectInput(
            ns("region_filter"),
            "Filter by Region(s):",
            choices = c("All Regions" = "All"), 
            selected = "All",
            multiple = TRUE 
          ),
          
          hr(), 
          
          # 5. Legend/Explanation for Bubble Size
          helpText("Bubble size corresponds to the total Population of the country.")
          
      ),
      
      # --- Plot Box (Width 8 for main visualization) ---
      # NOTA: solidHeader = FALSE es el valor por defecto.
      box(title = "Internet Access vs. Selected Indicator", 
          status = "primary", width = 8, solidHeader = TRUE,
          plotlyOutput(ns("bubble_plot"), height = "600px")
      )
    )
  )
}

# --- Module Server ---
# 'data_clean' is the reactive data frame loaded in app.R
vis2_server <- function(id, data_clean) { 
  moduleServer(id, function(input, output, session) {
    
    # 1. Update Region Selector choices based on the loaded data
    observe({
      req(data_clean()) 
      
      regions <- data_clean() %>%
        pull(Region) %>%
        unique() %>%
        sort()
      
      updateSelectInput(session, "region_filter",
                        choices = c("All Regions" = "All", regions),
                        selected = "All")
    })
    
    # 2. Create a reactive variable for the Y-axis
    y_var_reactive <- reactive({
      if (input$y_var == "GDP_Per_Capita" && isTRUE(input$log_gdp_toggle)) {
        return("Log_GDP_Per_Capita")
      } else {
        return(input$y_var)
      }
    })
    
    # 3. Filter the data based on user inputs
    filtered_data <- reactive({
      req(data_clean(), input$year_select, input$region_filter, y_var_reactive())
      
      data_for_year <- data_clean() %>%
        filter(Year == input$year_select)
      
      if (!"All" %in% input$region_filter) {
        data_for_year <- data_for_year %>%
          filter(Region %in% input$region_filter)
      }
      
      selected_y_sym <- sym(y_var_reactive())
      
      data_for_year %>%
        drop_na(Internet_Perc, !!selected_y_sym)
    })
    
    # 4. Render the Interactive Plot
    output$bubble_plot <- renderPlotly({
      df <- filtered_data()
      
      validate(
        need(nrow(df) > 0, "No data available for the selected year, variable, or region filter.")
      )
      
      y_sym <- sym(y_var_reactive())
      
      # Friendly names for Y-axis labels
      y_label <- case_when(
        y_var_reactive() == "Log_GDP_Per_Capita" ~ "GDP Per Capita (Log10 Scale)",
        y_var_reactive() == "GDP_Per_Capita" ~ "GDP Per Capita (Current US$)",
        y_var_reactive() == "Female_Employment_Perc" ~ "Female Employment (% of Labor Force)",
        TRUE ~ y_var_reactive()
      )
      
      # Build the plot
      p <- ggplot(df, aes(
        x = Internet_Perc, 
        y = !!y_sym, 
        size = Population, 
        color = Region,
        
        # Custom hover text in English
        text = paste(
          "<b>Country:</b>", Country.Name,
          "<br><b>Region:</b>", Region,
          "<br><b>Year:</b>", Year,
          "<br><b>Population:</b>", prettyNum(Population, big.mark = ","),
          "<br><b>Internet:</b>", round(Internet_Perc, 2), "%",
          "<br><b>GDP Per Capita:</b> $", prettyNum(round(GDP_Per_Capita, 0), big.mark = ","),
          "<br><b>Female Employment:</b>", round(Female_Employment_Perc, 2), "%"
        )
      )) +
        geom_point(alpha = 0.7) +
        scale_size(range = c(2, 20), guide = "none") + 
        labs(
          x = "Internet Users (% of Population)",
          y = y_label,
          title = paste("Internet Access vs.", y_label, "in", input$year_select),
          color = "Region" 
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      # Convert to plotly
      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = 0.1, y = -0.2))
    })
    
  })
}