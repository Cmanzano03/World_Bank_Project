# World_Bank_Project

Project of the subject Data visualization, where wil be developed an interactive Shiny app for exploring different charts related to the use of internet and its relationship to other variables along different countries of the world.

## ==**Refined Summary**==

-   **Q1: How has global Internet access evolved over time?**
    -   **Visualization:** Line Chart
    -   **Task Abstraction:** *Discover Trends* over *Sequential Time*
    -   **Filter/Interaction:**
        -   Year range slider (2000–2020)
        -   Region or income group selector
        -   Hover tooltip (country + % Internet users)
        -   Click legend to toggle regions/countries
    -   **Purpose:** Identify global and regional connectivity trends; highlight temporal inequalities.
-   **Q2: What are the potential correlations and dependencies between Internet access, GDP per capita, and other socioeconomic indicators?**
    -   **Visualization:** Dynamic Bubble Scatterplot (optionally linked with Parallel Coordinates or SPLOM, depending on the number of atributes)
    -   **Task Abstraction:** *Explore / Discover Correlation* among *Multiple Attributes*
    -   **Filter/Interaction:**
        -   Year slider or animation (2000–2020)
        -   Variable selector for X/Y axes (GDP, literacy, female employment)
        -   Continent/region selector
        -   Hover tooltip (country, Internet %, selected indicator, population)
        -   Toggle linear/log GDP axis
        -   Click to highlight a country’s trajectory (optional)
    -   **Purpose:** Allow deeper exploration of relationships beyond simple GDP–Internet correlation, supporting hypothesis generation.
-   **Q3: How do literacy rates and Internet access vary across different geographical regions of the world?**
    -   **Visualization:** Choropleth (or Cartogram)
    -   **Task Abstraction:** *Compare Spatial Distribution* (justified *spatio-temporal* analysis)
    -   **Filter/Interaction:**
        -   Year dropdown (fixed or limited range, e.g. 2018)
        -   Indicator toggle (Internet % ↔ literacy rate)
        -   Hover tooltip (country, literacy %, Internet %)
        -   Click on a country → mini time series of Internet %
    -   **Purpose:** Highlight regional disparities in digital and educational access using geographic spatial patterns.
-   **Q4: Which countries show outlier behavior in Internet access or GDP per capita relative to their regional or income group, and how can these deviations be characterized?**
    -   **Visualization:** Dynamic K-Means Scatterplot + Coordinated Profile Chart (Linked Views)
    -   **Task Abstraction:** *Locate and Identify Outliers* to characterize atypical countries within socioeconomic groups.
    -   **Controls & Interactions:**
        -   `Year Slider (2000–2020):` Recalculates the K-Means clustering model dynamically for the selected year.
        -   `Log/Linear Scale Toggle:` Switches the GDP X-axis to aid visual exploration, demonstrating the necessity of a log scale for this data.
        -   `Outlier Percentile Slider:` Adjusts the percentile threshold (e.g., p\>=95) used to classify outliers based on their distance from their cluster centroid.
        -   `VA Integration (K-Means):` Color and grouping are assigned by a K-Means (k=3) model run on two standardized variables: **`Internet users (%)`** and **`log(GDP per capita)`**.
        -   `Outlier Detection (Shape):` Outliers are marked with a triangle. They are defined as the countries with the largest Euclidean distance from their assigned cluster's centroid in the 2D standardized space (i.e., outside the user-defined percentile).
        -   **`Hover Interaction 1 (Tooltip):`** The main tooltip is designed to reveal the core *insight* of the analysis by comparing:
            -   **`K-Means Cluster:`** (e.g., "Medium development")
            -   **`World Bank Income Group:`** (e.g., "Lower middle income")
            -   It also provides context with Region, Internet %, GDP, and Literacy %.
        -   **`Hover Interaction 2 (Coordinated Chart):`** A bar chart in the sidebar updates instantly on hover. It displays the country's **Z-score profile**, visually explaining *why* it was clustered that way by showing its standardized `Internet` and `log(GDP)` values.
    -   **Purpose:** Identify and explore countries whose digital/economic profile (K-Means) deviates from their official economic classification (World Bank), such as discovering low-income countries with surprisingly high-tech adoption (e.g., Kosovo).

We would perform an unsupervised **k-means clustering** (k=3/4) on **Internet%**, **GDP per capita**, and **Literacy%** to reveal country groupings (e.g., high-GDP/high-Internet; low-GDP/low-Internet; low-GDP/high-Internet). Clusters are used for color encoding in the scatter plot and to highlight countries that deviate from their **World Bank income group**, supporting outlier discovery within the Visual Analytics framework.

-   **Q5: How is the global connected population distributed among regions and countries, reflecting the hierarchical structure?**
    -   **Visualization:** Treemap Chart
    -   **Task Abstraction:** *Explore Hierarchy and Distribution*
    -   **Filter/Interaction:**
        -   Year selector
        -   Hover tooltip (region, country, total Internet users, % of world total)
        -   Zoom in/out (Region → Country → Group)
    -   **Purpose:** Visualize the hierarchical structure of the global Internet population and the dominance of specific regions or countries.

# Datasets needed

| Indicator                                | World Bank Code  | Used in charts |
|------------------------------------------|------------------|----------------|
| Internet users (% of population)         | `IT.NET.USER.ZS` | 1, 2, 3, 4, 5  |
| GDP per capita (current US\$)            | `NY.GDP.PCAP.CD` | 2, 4           |
| Literacy rate, adult (% people ages 15+) | `SE.ADT.LITR.ZS` | 3, 4           |
| Population                               | `SP.POP.TOTL`    | 5              |
| Region / Income group (metadata)         | —                | All            |

==Q4: clustering (k-means) to color code the countries in the scatter plot, when you click on a country, its socioeconomic profile in a bar char.==
