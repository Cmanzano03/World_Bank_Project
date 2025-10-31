# World_Bank_Project
Project of the subject Data visualization, where wil be developed an interactive Shiny app for exploring different charts related to the use of internet and its relationship to other variables along different countries of the world.

## ==**Refined Summary**==

- **Q1: How has global Internet access evolved over time?**
    - **Visualization:** Line Chart
    - **Task Abstraction:** _Discover Trends_ over _Sequential Time_
    - **Filter/Interaction:**
        - Year range slider (2000–2020)
        - Region or income group selector
        - Hover tooltip (country + % Internet users)
        - Click legend to toggle regions/countries
    - **Purpose:** Identify global and regional connectivity trends; highlight temporal inequalities.
        

- **Q2: What are the potential correlations and dependencies between Internet access, GDP per capita, and other socioeconomic indicators?**
    - **Visualization:** Dynamic Bubble Scatterplot (optionally linked with Parallel Coordinates or SPLOM, depending on the number of atributes)
    - **Task Abstraction:** _Explore / Discover Correlation_ among _Multiple Attributes_
    - **Filter/Interaction:**
        - Year slider or animation (2000–2020)
        - Variable selector for X/Y axes (GDP, literacy, female employment)
        - Continent/region selector
        - Hover tooltip (country, Internet %, selected indicator, population)
        - Toggle linear/log GDP axis
        - Click to highlight a country’s trajectory (optional)
    - **Purpose:** Allow deeper exploration of relationships beyond simple GDP–Internet correlation, supporting hypothesis generation.
        

- **Q3: How do literacy rates and Internet access vary across different geographical regions of the world?**
    - **Visualization:** Choropleth (or Cartogram)
    - **Task Abstraction:** _Compare Spatial Distribution_ (justified _spatio-temporal_ analysis)
    - **Filter/Interaction:**
        - Year dropdown (fixed or limited range, e.g. 2018)
        - Indicator toggle (Internet % ↔ literacy rate)
        - Hover tooltip (country, literacy %, Internet %)
        - Click on a country → mini time series of Internet %
    - **Purpose:** Highlight regional disparities in digital and educational access using geographic spatial patterns.
        

- **Q4: Which countries show outlier behavior in Internet access or GDP per capita relative to their regional or income group, and how can these deviations be characterized?**
	- **Visualization:** Dynamic Scatterplot + Coordinated Bar View (linked views)
	- **Task Abstraction:** _Locate and Identify Outliers_ to characterize atypical countries within socioeconomic groups
	- **Filter/Interaction:**
	    - Year slider (2000–2020)
	    - Region or income group selector
	    - Hover tooltip (country, Internet %, GDP, literacy, population)
	    - Click on country → show side panel or mini bar chart with other indicators
	    - Crucial Addition (VA Integration): Color encoding is dynamically assigned based on K-Means Clustering (k=3 or 4) run on key socioeconomic indicators (Internet %, GDP, Literacy, etc.). This analysis provides the initial grouping/model.
	    - Outlier Detection: Countries that show atypical behavior relative to their cluster membership are visually highlighted (popout) using the Shape channel. This supports the Visual Analytics Mantra: "Analyze first, show the important...".
	• Purpose: Identify and explore countries whose profile deviates from patterns established by the automated cluster model, supporting the discovery of unexpected 
	
We would perform an unsupervised **k-means clustering** (k=3/4) on **Internet%**, **GDP per capita**, and **Literacy%** to reveal country groupings (e.g., high-GDP/high-Internet; low-GDP/low-Internet; low-GDP/high-Internet). Clusters are used for color encoding in the scatter plot and to highlight countries that deviate from their **World Bank income group**, supporting outlier discovery within the Visual Analytics framework.


- **Q5: How is the global connected population distributed among regions and countries, reflecting the hierarchical structure?**
    - **Visualization:** Treemap Chart
    - **Task Abstraction:** _Explore Hierarchy and Distribution_
    - **Filter/Interaction:**
        - Year selector
        - Hover tooltip (region, country, total Internet users, % of world total)
        - Zoom in/out (Region → Country → Group)
    - **Purpose:** Visualize the hierarchical structure of the global Internet population and the dominance of specific regions or countries.

# Datasets needed

| Indicator                                              | World Bank Code     | Used in charts |
| ------------------------------------------------------ | ------------------- | -------------- |
| Internet users (% of population)                       | `IT.NET.USER.ZS`    | 1, 2, 3, 5     |
| GDP per capita (current US$)                           | `NY.GDP.PCAP.CD`    | 2              |
| Literacy rate, adult (% people ages 15+)               | `SE.ADT.LITR.ZS`    | 3              |
| Female employment in services (% of female employment) | `SL.SRV.EMPL.FE.ZS` | 4              |
| Total employment in services (% of total employment)   | `SL.SRV.EMPL.ZS`    | 4              |
| Population                                             | `SP.POP.TOTL`       | 5              |
| Region / Income group (metadata)                       | —                   | All            |

==Q4: clustering (k-means) to color code the countries in the scatter plot, when you click on a country, its socioeconomic profile in a bar char.==
