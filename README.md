# Premier League 2023/2024 Analysis

This project analyzes Premier League 2023/2024 data using three CSV files:
1. `matches.csv` - Contains match details for the Premier League 2023/2024 season.
2. `premier-player-23-24.csv` - Provides player-specific data for the season.
3. `stadiums-with-GPS-coordinates.csv` - Includes stadium details with their respective GPS coordinates.

## Features
The project includes a Shiny app that provides a comprehensive data analysis and visualization for the Premier League 2023/2024 season. The app offers:
- Interactive data visualizations.
- Stadium mapping with GPS coordinates.
- Player and match analysis.

## How to Run
1. **Download the requirements**: Ensure you have the required libraries installed (see the **Requirements** section below).

2. **Download datasets**: 
   - Download the CSV files from the `data/` folder in this repository.
   - Place them in the same directory where the `Full_app.R` file is located.

3. **Set the working directory**:
   In RStudio or your R console, set the working directory to the project folder:
   ```R
   setwd("path/to/your/project/folder")

   ## Requirements

To run this project, you need to install the following R libraries. You can install them by running the following code in your R console:

```R
# Install required libraries
install.packages(c(
  "shiny",
  "ggplot2",
  "dplyr",
  "ggrepel",
  "treemapify",
  "RColorBrewer",
  "tidyr",
  "leaflet",
  "htmlwidgets",
  "webshot2",
  "zoo",
  "scales",
  "leaflet.providers"
))

