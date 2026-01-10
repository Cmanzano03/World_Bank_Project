# install_dependencies.R
# Run this script once to install all required packages for the World Bank Project

# List of dependencies identified in the development environment
packages <- c(
  "shiny",
  "shinydashboard",
  "tidyverse",          # Meta-package (includes ggplot2, dplyr, tidyr, readr, etc.)
  "ggplot2",
  "dplyr",
  "readr",
  "tidyr",
  "tibble",
  "purrr",
  "stringr",
  "forcats",
  "lubridate",          # Date handling
  "plotly",             # Interactive plotting
  "scales",             # Plot scaling
  "colorspace",         # Color palette manipulation
  "sf",                 # Simple Features for maps
  "rnaturalearth",      # Map data
  "rnaturalearthdata"   # Map data assets
)

# Identify which packages are not yet installed
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

# Install the missing packages
if(length(new_packages)) {
  message("Installing missing dependencies: ", paste(new_packages, collapse = ", "))
  install.packages(new_packages)
} else {
  message("All dependencies are already installed.")
}

print("Environment setup complete.")