# ----------------------------------------------
# VISUALIZATION 3: (e.g., Line Chart - Q3)
# ----------------------------------------------

# --- Module UI ---
vis3_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Visualization 3"),
    hr(),
    fluidRow(
      box(title = "Controls", status = "warning", solidHeader = TRUE, width = 12,
          p("Add Vis 3 controls here")
          # e.g., sliderInput(ns("year_range_vis3"), ... )
      ),
      box(title = "Plot", status = "primary", solidHeader = TRUE, width = 12,
          p("Add Vis 3 plot output here")
          # e.g., plotOutput(ns("line_plot_vis3"))
      )
    )
  )
}

# --- Module Server ---
# Add data arguments as needed (e.g., data_internet)
vis3_server <- function(id) { 
  moduleServer(id, function(input, output, session) {
    
    # output$line_plot_vis3 <- renderPlot({
    #   ... R code for plot 3 ...
    # })
    
  })
}