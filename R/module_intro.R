# ----------------------------------------------
# INTRO TAB
# ----------------------------------------------

# --- Module UI ---
intro_ui <- function(id) {
  ns <- NS(id) # Namespace
  
  tagList(
    h2("Introduction"),
    hr()
    # You can add more text/images here
  )
}

# --- Module Server ---
intro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server logic needed for a static intro page
  })
}