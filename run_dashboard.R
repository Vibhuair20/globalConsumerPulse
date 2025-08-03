# Global Consumer Pulse Dashboard - Launch Script
# This script launches the Shiny dashboard

cat("🚀 Launching Global Consumer Pulse Dashboard...\n")
cat("===============================================\n\n")

# Set working directory to app folder
setwd("GlobalConsumerPulse/app")

# Check if data files exist
if (file.exists("../data/cleaned/economic_data.csv")) {
  cat("✓ Data files found\n")
} else {
  cat("⚠️  Warning: Data files not found. Dashboard may not work properly.\n")
}

# Launch the dashboard
cat("📊 Starting Shiny application...\n")
cat("🌐 Dashboard will open in your default browser\n")
cat("🔗 URL: http://127.0.0.1:3838\n")
cat("⏹️  Press Ctrl+C to stop the dashboard\n\n")

# Run the app
shiny::runApp(
  "app.R",
  host = "127.0.0.1",
  port = 3838,
  launch.browser = TRUE
)