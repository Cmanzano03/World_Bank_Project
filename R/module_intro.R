# ----------------------------------------------
# INTRO TAB
# ----------------------------------------------

# --- Module UI ---
intro_ui <- function(id) {
  ns <- NS(id) # Namespace
  
  tagList(
    # 1. Header Section with a "Lead" paragraph
    div(class = "jumbotron", style = "background-color: #ecf0f1; padding: 20px; border-radius: 5px;",
        h2("Global Digital Divide: Wealth vs. Connectivity"),
        p(class = "lead", 
          "An interactive dashboard exploring the relationship between economic development (GDP) 
        and digital adoption (Internet Usage) across the globe from 2000 to 2020.")
    ),
    
    br(),
    
    # 2. Main Content organized in columns
    fluidRow(
      # Left Column: Context & Data
      column(6,
             h3(icon("globe"), " Project Context"),
             p("In the modern era, internet access is often viewed as a fundamental utility. 
          However, the 'Digital Divide' remains a significant barrier to equality. 
          This project investigates:"),
             tags$ul(
               tags$li(strong("The Correlation:"), " How strongly does national wealth predict internet access?"),
               tags$li(strong("The Trends:"), " Which regions are adopting technology the fastest?"),
               tags$li(strong("The Outliers:"), " Which countries represent 'Digital Leapfroggers' (Low GDP, High Access) vs. 'Under-performers'?")
             ),
             
             br(),
             
             h3(icon("database"), " Data Source"),
             p("The data is sourced from the ", 
               a(href = "https://data.worldbank.org/", target = "_blank", "World Bank Open Data repository"), "."),
             p("Key indicators analyzed:"),
             tags$ul(
               tags$li("GDP per capita (current US$)"),
               tags$li("Individuals using the Internet (% of population)"),
               tags$li("Total Population"),
               tags$li("World Bank Income Groups & Regions")
             )
      ),
      
      # Right Column: Navigation Guide
      column(6,
             h3(icon("map-signs"), " How to use this Dashboard"),
             p("Navigate through the tabs on the left to explore different perspectives:"),
             
             # You can customize these descriptions based on your actual tab names
             div(style = "background-color: #f9f9f9; padding: 15px; border-left: 5px solid #2c3e50; margin-bottom: 10px;",
                 h4("1. Global Overview"),
                 p("A spatial view of the data. Use this map to identify regional clusters and geographic disparities.")
             ),
             
             div(style = "background-color: #f9f9f9; padding: 15px; border-left: 5px solid #18bc9c; margin-bottom: 10px;",
                 h4("2. Correlation & Trends"),
                 p("Time-series analysis and scatter plots to visualize the evolution of the GDP-Internet relationship over 20 years.")
             ),
             
             div(style = "background-color: #f9f9f9; padding: 15px; border-left: 5px solid #e74c3c; margin-bottom: 10px;",
                 h4("3. Outlier Detection (Clustering)"),
                 p("An algorithmic analysis using K-Means Clustering. This module identifies statistical anomalies—such as Kosovo or Equatorial Guinea—that defy standard economic models.")
             )
      )
    ),
    
    hr(),
    
    # 3. Footer / Credits
    div(style = "text-align: center; color: #7f8c8d;",
        p(em("Created for the Data Visualization Course • Year 2025/2026"))
        # You can add your names here if you want
    )
  )
}

# --- Module Server ---
intro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server logic needed for a static intro page
  })
}