# ----------------------------------------------
# VISUALIZATION 4: (e.g., Line Chart - Q4)
# ----------------------------------------------

# --- Module UI ---
vis4_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Visualization 4"),
    hr(),
    fluidRow(
      box(title = "Controls", status = "warning", width = 12,
          p("Add Vis 4 controls here")
          # e.g., sliderInput(ns("year_range_vis4"), ... )
      ),
      box(title = "Plot", status = "primary", width = 12,
          p("Add Vis 4 plot output here")
          # e.g., plotOutput(ns("line_plot_vis4"))
      )
    )
  )
}

# --- Module Server ---
# Add data arguments as needed (e.g., data_internet)
vis4_server <- function(id) { 
  moduleServer(id, function(input, output, session) {
    
    # output$line_plot_vis4 <- renderPlot({
    #   ... R code for plot 4 ...
    # })
    
  })
}