# Global Consumer Pulse Dashboard

An interactive R Shiny dashboard for analyzing global economic indicators including GDP, inflation, and consumer price index data from the World Bank.

## Features

- Interactive world map with economic data visualization
- Multi-country time series analysis and comparisons
- Top 10 country rankings with trend indicators
- Professional Quarto reports (HTML, PDF, Word)
- Responsive design with modern UI

## Project Structure

```
GlobalConsumerPulse/
├── R/                    # Data processing and visualization functions
│   ├── clean_data.R     # Data cleaning pipeline
│   └── visualization_functions.R  # Chart creation functions
├── app/                  # Shiny dashboard
│   └── app.R            # Main application
├── quarto/               # Report generation
│   ├── report.qmd       # Main report template
│   └── styles.css       # Report styling
├── data/                 # Data files
│   ├── raw/             # Original World Bank CSV files
│   └── cleaned/         # Processed datasets
└── README.md            # This file
```

## Quick Start

### Prerequisites

Install R 4.0+ and required packages:

```r
install.packages(c("shiny", "shinydashboard", "dplyr", "plotly", "DT"))
```

### Running the Dashboard

1. **Install packages**:
   ```r
   source("setup.R")  # Installs all required packages
   ```

2. **Launch dashboard**:
   ```r
   source("debug_and_run.R")  # Starts the dashboard
   ```

3. **Access dashboard**: Open http://localhost:3838 in your browser

## Data Sources

Uses World Bank economic indicators:
- **GDP (Current US$)**: NY.GDP.MKTP.CD
- **Consumer Price Index**: FP.CPI.TOTL  
- **Inflation Rate**: FP.CPI.TOTL.ZG
- **GDP Deflator**: NY.GDP.DEFL.KD.ZG

Data covers 1960-2023 for 180+ countries.

## Key Features

- **Interactive World Map**: Choropleth visualization with hover tooltips
- **Time Series Charts**: Multi-country trend comparisons
- **Rankings Table**: Top 10 countries with growth indicators
- **Responsive Design**: Works on desktop and mobile devices
- **Professional Styling**: Clean, modern interface

## Technical Details

### Main Functions

**Data Processing (`R/clean_data.R`)**:
- `process_all_data()` - Processes World Bank CSV files
- `build_unified_economic_dataset()` - Creates final dataset

**Visualizations (`R/visualization_functions.R`)**:
- `create_world_map()` - Interactive choropleth maps
- `create_time_series_chart()` - Multi-country trend charts
- `create_comparison_chart()` - Country comparison bars

## Generate Reports

Create reports in multiple formats:

```r
# HTML report
quarto::quarto_render("quarto/report.qmd", output_format = "html")

# PDF report  
quarto::quarto_render("quarto/report.qmd", output_format = "pdf")
```

## Requirements

- R 4.0+
- Required packages: shiny, shinydashboard, dplyr, plotly, DT
- World Bank data files (included in `data/` directory)

## License

MIT License

---

*Built with R, Shiny, and Plotly for economic data analysis*