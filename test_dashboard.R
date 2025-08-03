# Test Dashboard Launch Script
# Quick test to see the rankings table fixes

cat("🔧 Testing Global Consumer Pulse Dashboard fixes...\n")
cat("📊 Focus: Rankings table visibility and black text\n")
cat("===============================================\n\n")

# Load required libraries quietly
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(dplyr)
  library(plotly)
  library(DT)
})

# Set working directory
setwd("GlobalConsumerPulse/app")

# Check data
if (file.exists("../data/cleaned/economic_data.csv")) {
  cat("✅ Data files found\n")
  
  # Quick data check
  data <- read.csv("../data/cleaned/economic_data.csv")
  cat("📈 Data loaded:", nrow(data), "rows\n")
  cat("🌍 Countries:", length(unique(data$country_name)), "\n")
  cat("📊 Indicators:", paste(unique(data$indicator_short), collapse = ", "), "\n\n")
} else {
  cat("❌ Data files not found\n")
}

# Launch dashboard
cat("🚀 Starting dashboard with fixes...\n")
cat("🎯 Check the 'Top 10 Countries' table on the right\n")
cat("🔍 Look for:\n")
cat("   - Black text in country names\n")
cat("   - Visible country flags and names\n")
cat("   - Proper table formatting\n")
cat("🌐 URL: http://localhost:3838\n")
cat("⏹️  Press Ctrl+C to stop\n\n")

# Run the app
shiny::runApp(
  "app.R",
  host = "127.0.0.1",
  port = 3838,
  launch.browser = TRUE
)