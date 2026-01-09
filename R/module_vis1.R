# module_vis1.R

vis1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Q1: Evolution of Global Internet Access (2000–2020)"),
    fluidRow(
      # Left Column: Sidebar with Tabs
      column(width = 4,
             box(
               title = tagList(icon("sliders-h"), "Plot Controls"),
               status = "warning",
               solidHeader = TRUE,
               width = NULL,
               
               tabsetPanel(
                 id = ns("control_tabs"),
                 
                 # Tab 1: Time & Region
                 tabPanel("Geography", 
                          br(),
                          sliderInput(ns("year_range"), "Select Year Range:",
                                      min = 2000, max = 2020,
                                      value = c(2000, 2020),
                                      sep = ""),
                          selectizeInput(ns("region_filter"), "Filter by Region(s):",
                                         choices = NULL, 
                                         multiple = TRUE,
                                         options = list(placeholder = 'All Regions'))
                 ),
                 
                 # Tab 2: Economic Status
                 tabPanel("Economics", 
                          br(),
                          selectizeInput(ns("income_filter"), "Filter by Income Group(s):",
                                         choices = NULL, 
                                         multiple = TRUE,
                                         options = list(placeholder = 'All Income Groups')),
                          helpText("The chart will update to show countries that match both Geographical and Economic filters.")
                 )
               )
             )
      ),
      
      # Right Column: The Plot
      column(width = 8,
             box(
               title = tagList(icon("line-chart"), "Internet Access Over Time"),
               status = "primary",
               solidHeader = TRUE,
               width = NULL,
               plotlyOutput(ns("internetPlot"), height = "600px")
             )
      )
    )
  )
}

vis1_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Load data
    full_data <- reactive({
      internet <- read_csv("data/internet.csv", skip = 4)
      metadata <- read_csv("data/metadataCountry.csv")
      
      internet %>%
        select(`Country Name`, `Country Code`, as.character(2000:2020)) %>%
        pivot_longer(cols = as.character(2000:2020), names_to = "Year", values_to = "Usage") %>%
        mutate(Year = as.numeric(Year)) %>%
        left_join(metadata, by = "Country Code") %>%
        filter(!is.na(Region), !is.na(Usage))
    })
    
    # 2. Update Filter Choices dynamically
    observe({
      df <- full_data()
      updateSelectizeInput(session, "region_filter", choices = sort(unique(df$Region)), server = TRUE)
      updateSelectizeInput(session, "income_filter", choices = sort(unique(df$IncomeGroup)), server = TRUE)
    })
    
    # 3. Reactive Filter Logic
    filtered_data <- reactive({
      df <- full_data() %>%
        filter(Year >= input$year_range[1], Year <= input$year_range[2])
      
      if (!is.null(input$region_filter)) {
        df <- df %>% filter(Region %in% input$region_filter)
      }
      
      if (!is.null(input$income_filter)) {
        df <- df %>% filter(IncomeGroup %in% input$income_filter)
      }
      df
    })
    
    # 4. Render the Plot
    output$internetPlot <- renderPlotly({
      plot_ly(filtered_data(), 
              x = ~Year, 
              y = ~Usage, 
              color = ~Region, 
              colors = "Set2",
              type = 'scatter', 
              mode = 'lines',
              text = ~paste("<b>Country:</b>", `Country Name`, 
                            "<br><b>Region:</b>", Region,
                            "<br><b>Income:</b>", IncomeGroup,
                            "<br><b>Usage:</b>", round(Usage, 1), "%"),
              hoverinfo = "text") %>%
        layout(
          xaxis = list(title = "Year"),
          yaxis = list(title = "Internet Users (% of Population)"),
          legend = list(orientation = 'h', y = -0.2),
          margin = list(b = 100)
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}