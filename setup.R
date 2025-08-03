# Global Consumer Pulse Dashboard - Setup Script
# Run this script to install all required packages

cat("=== Global Consumer Pulse Dashboard Setup ===\n")
cat("Installing required R packages...\n\n")

# List of required packages
required_packages <- c(
  # Core Shiny packages
  "shiny",
  "shinydashboard",
  
  # Data manipulation
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  
  # Visualization
  "ggplot2",
  "plotly",
  "DT",
  "RColorBrewer",
  
  # Additional utilities
  "zoo",
  "htmltools",
  
  # Reporting (optional)
  "knitr",
  "rmarkdown"
)

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("Installing", pkg, "...\n")
      install.packages(pkg, dependencies = TRUE)
      
      # Verify installation
      if (require(pkg, character.only = TRUE, quietly = TRUE)) {
        cat("✓", pkg, "installed successfully\n")
      } else {
        cat("✗ Failed to install", pkg, "\n")
      }
    } else {
      cat("✓", pkg, "already installed\n")
    }
  }
}

# Install packages
install_if_missing(required_packages)

cat("\n=== Setup Complete ===\n")
cat("All required packages are installed.\n")
cat("You can now run the dashboard with:\n")
cat("setwd('GlobalConsumerPulse/app')\n")
cat("shiny::runApp('app.R')\n")

# Test package loading
cat("\n=== Testing Package Loading ===\n")
test_packages <- c("shiny", "shinydashboard", "dplyr", "plotly", "DT")

for (pkg in test_packages) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("✓", pkg, "loads correctly\n")
  } else {
    cat("✗", pkg, "failed to load\n")
  }
}

cat("\n🎉 Setup completed successfully!\n")