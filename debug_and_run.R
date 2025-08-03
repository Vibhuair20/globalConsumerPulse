# Debug and Run Dashboard Script
# This script checks data availability and runs the dashboard

cat("🔍 Global Consumer Pulse Dashboard - Debug & Run\n")
cat("===============================================\n\n")

# Check current working directory
cat("📁 Current working directory:", getwd(), "\n")

# Check for data files
data_paths <- c(
  "GlobalConsumerPulse/data/cleaned/economic_data.csv",
  "data/cleaned/economic_data.csv",
  "app/../data/cleaned/economic_data.csv"
)

data_found <- FALSE
for (path in data_paths) {
  if (file.exists(path)) {
    cat("✅ Found data file at:", path, "\n")
    
    # Quick data check
    tryCatch({
      data <- read.csv(path, nrows = 5)
      cat("📊 Data preview - Columns:", ncol(data), "| Sample rows:", nrow(data), "\n")
      cat("🌍 Sample countries:", paste(head(unique(data$country_name), 3), collapse = ", "), "\n")
      data_found <- TRUE
      break
    }, error = function(e) {
      cat("❌ Error reading data:", e$message, "\n")
    })
  } else {
    cat("❌ Not found:", path, "\n")
  }
}

if (!data_found) {
  cat("⚠️  No data files found. Dashboard will use sample data.\n")
}

cat("\n🚀 Starting dashboard...\n")
cat("🎯 Check the 'Top 10 Countries' table for fixes\n")
cat("🌐 URL: http://localhost:3838\n")
cat("⏹️  Press Ctrl+C to stop\n\n")

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(dplyr)
  library(plotly)
  library(DT)
})

# Set working directory and run
setwd("GlobalConsumerPulse/app")
shiny::runApp("app.R", port = 3838, launch.browser = TRUE)