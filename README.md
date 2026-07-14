# World Bank Data Visualization Project

Interactive Shiny application to explore global internet access and its relationship with GDP, literacy, and population using World Bank data.

## Repository Structure

```
.
├── app.R                    # Main Shiny app entry point
├── install_dependencies.R   # Installs required R packages
├── README.md
├── WorldBankProject.Rproj
├── R/
│   ├── module_intro.R
│   ├── module_vis1.R
│   ├── module_vis2.R
│   ├── module_vis3.R
│   ├── module_vis4.R
│   └── module_vis5.R
└── data/
    ├── cleaned_world_bank_data_Q2.csv
    ├── GDP.csv
    ├── internet.csv
    ├── literacy.csv
    ├── metadataCountry.csv
    └── population.csv
```

## What The App Includes

- Visualization 1: Internet access trends over time (line chart).
- Visualization 2: Internet vs GDP and other socioeconomic variables (bubble scatterplot).
- Visualization 3: Internet and literacy world comparison (coordinated dual maps + time series).
- Visualization 4: Outlier detection with K-Means and profile views.
- Visualization 5: Hierarchical distribution of connected population (treemap).

## Data Sources

World Bank Open Data indicators used:

- Internet users (% of population): `IT.NET.USER.ZS`
- GDP per capita (current US$): `NY.GDP.PCAP.CD`
- Literacy rate (% people ages 15+): `SE.ADT.LITR.ZS`
- Population, total: `SP.POP.TOTL`
- Country metadata (region/income group)

## Run The Project

Online deployment:

- https://cmanzano.shinyapps.io/World_Bank_Project/

Local run:

1. Open the project in RStudio.
2. Install packages:

```r
source("install_dependencies.R")
```

3. Run the app:

```r
shiny::runApp(".")
```

