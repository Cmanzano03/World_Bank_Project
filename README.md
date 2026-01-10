# 🌍 World Bank Data Visualization Project

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-007BC2?style=for-the-badge&logo=rstudio&logoColor=white)
![Status](https://img.shields.io/badge/Status-Deployed-success?style=for-the-badge)

This project, developed for the **Data Visualization** subject, features an interactive Shiny application designed to explore global Internet usage trends and their relationships with various socioeconomic indicators (GDP, Literacy, Employment) using data from the World Bank.

---

## 📝 Project Summary & Visualizations

### 📊 Q1: How has global Internet access evolved over time?
* **Visualization:** Line Chart
* **Task Abstraction:** *Discover Trends* over *Sequential Time*
* **Interactions:**
    * 📅 **Year range slider:** (2000–2020)
    * 🌍 **Region selector:** Filter by specific regions or income groups.
    * 🖱️ **Hover:** Tooltip showing country details and exact %.
    * 👁️ **Legend Toggle:** Click to hide/show specific groups.
* **Purpose:** Identify global and regional connectivity trends and highlight temporal inequalities.

### 📈 Q2: Correlations between Internet, GDP, and Socioeconomic factors
* **Visualization:** Dynamic Bubble Scatterplot
* **Task Abstraction:** *Explore / Discover Correlation* among *Multiple Attributes*
* **Interactions:**
    * ▶️ **Animation:** Auto-play changes from 2000 to 2020.
    * 🔀 **Variable Selector:** Switch Y-Axis (GDP, Female Employment, etc.).
    * 📐 **Log Scale Toggle:** Switch GDP axis between Linear and Logarithmic.
    * 🔍 **Region Filter:** Focus on specific continents.
* **Purpose:** Allow deeper exploration of relationships beyond simple GDP–Internet correlation, supporting hypothesis generation.

### 🗺️ Q3: Geographic variance of Literacy and Internet access
* **Visualization:** Choropleth Map (or Cartogram)
* **Task Abstraction:** *Compare Spatial Distribution* (Spatio-temporal analysis)
* **Interactions:**
    * 📆 **Year Dropdown:** Select specific time slices (e.g., 2018).
    * 🔄 **Indicator Toggle:** Switch between Internet % and Literacy Rate.
    * 🖱️ **Mini-Time Series:** Click on a country to see its specific history.
* **Purpose:** Highlight regional disparities in digital and educational access using geographic spatial patterns.

### ⚠️ Q4: Outlier Detection and Anomaly Characterization
* **Visualization:** Dynamic K-Means Scatterplot + Coordinated Profile Chart
* **Task Abstraction:** *Locate and Identify Outliers* (Characterize atypical countries).
* **The Algorithm:**
    * Performs unsupervised **K-Means clustering (k=3)** on *Internet %* and *Log(GDP)*.
    * Calculates **Z-Scores** to standardize variables.
    * Identifies outliers based on **Euclidean distance** from the cluster centroid.
* **Interactions:**
    * 🔢 **Cluster Controls:** Recalculates the model dynamically based on the selected Year.
    * 🎚️ **Outlier Threshold:** Adjust the percentile (e.g., p > 95%) to define strictness.
    * 📊 **Coordinated View:** Hovering over a "Triangle" (outlier) reveals a bar chart showing *why* it is an outlier (e.g., "High Internet but Low GDP").
* **Purpose:** Discover countries whose digital profile deviates from their official World Bank economic classification (e.g., finding "over-achievers" in low-income groups).

### 🌳 Q5: Hierarchical distribution of the connected population
* **Visualization:** Treemap Chart
* **Task Abstraction:** *Explore Hierarchy and Distribution*
* **Interactions:**
    * 🔍 **Zoom:** Navigate from Region → Country.
    * 🖱️ **Tooltip:** View total Internet users and % of the world total.
* **Purpose:** Visualize the dominance of specific regions (like Asia) or countries (like China/India) in the total global count of connected users.

---

## 💾 Datasets Used

All data is sourced from the World Bank Open Data repository.

| Indicator | World Bank Code | Used in Charts |
| :--- | :--- | :--- |
| **Internet users (% of population)** | `IT.NET.USER.ZS` | 1, 2, 3, 4, 5 |
| **GDP per capita (current US$)** | `NY.GDP.PCAP.CD` | 2, 4 |
| **Literacy rate (% people ages 15+)** | `SE.ADT.LITR.ZS` | 3, 4 |
| **Population** | `SP.POP.TOTL` | 5 |
| **Region / Income group** | *(Metadata)* | All |

---

## 🚀 Instructions to run the app

To ensure accessibility and ease of evaluation, the application has been deployed to the cloud. However, it can also be executed locally within the RStudio environment.

### Option 1: Online Access (Recommended)
The most immediate way to interact with the tool is via the web deployment on ShinyApps.io. This version requires no local configuration or installation.

* 🔗 **Link:** [Click here to open the World Bank Project App](https://cmanzano.shinyapps.io/World_Bank_Project/)
* **Status:** ✅ Active

### Option 2: Local Installation
If you wish to run the source code locally, please follow these steps.

#### Prerequisites
* **R:** Version 4.0.0 or higher.
* **RStudio:** Recommended for managing the Shiny runtime.

#### Step 1: Install Dependencies
We have provided a helper script `install_dependencies.R` in the root directory. You can install all required libraries by running the following command in the R console:

```r
source("install_dependencies.R")