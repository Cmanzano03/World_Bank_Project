# ----------------------------------------------
# VISUALIZATION 1: (e.g., Line Chart - Q1)
# ----------------------------------------------

# --- Module UI ---
vis1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Visualization 1"),
    hr(),
    fluidRow(
      box(title = "Controls", status = "warning", solidHeader = TRUE, width = 12,
          p("Add Vis 1 controls here")
          # e.g., sliderInput(ns("year_range_vis1"), ... )
      ),
      box(title = "Plot", status = "primary", solidHeader = TRUE, width = 12,
          p("Add Vis 1 plot output here")
          # e.g., plotOutput(ns("line_plot_vis1"))
      )
    )
  )
}

# --- Module Server ---
# Add data arguments as needed (e.g., data_internet)
vis1_server <- function(id) { 
  moduleServer(id, function(input, output, session) {
    
    # output$line_plot_vis1 <- renderPlot({
    #   ... R code for plot 1 ...
    # })
    
  })
}