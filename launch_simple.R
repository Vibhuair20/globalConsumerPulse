# Simple Dashboard Launch Script
# This launches the dashboard with minimal output

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

# Launch dashboard
cat("🚀 Starting Global Consumer Pulse Dashboard...\n")
cat("🌐 Open your browser to: http://localhost:3838\n")
cat("⏹️  Press Ctrl+C to stop\n\n")

shiny::runApp("app.R", port = 3838, launch.browser = TRUE)