# Global Consumer Pulse Dashboard
# Professional Shiny Application for Economic Data Visualization

# Load required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(readr)

# Load cleaned economic data with error handling
tryCatch({
  economic_data <- read_csv("../data/cleaned/economic_data.csv", show_col_types = FALSE)
  cat("✅ Successfully loaded economic data:", nrow(economic_data), "rows\n")
}, error = function(e) {
  cat("❌ Error loading economic data:", e$message, "\n")
  cat("📁 Current working directory:", getwd(), "\n")
  cat("🔍 Checking for data file...\n")
  
  # Try alternative paths
  possible_paths <- c(
    "../data/cleaned/economic_data.csv",
    "data/cleaned/economic_data.csv",
    "GlobalConsumerPulse/data/cleaned/economic_data.csv"
  )
  
  for (path in possible_paths) {
    if (file.exists(path)) {
      cat("✅ Found data file at:", path, "\n")
      economic_data <- read_csv(path, show_col_types = FALSE)
      break
    } else {
      cat("❌ Not found:", path, "\n")
    }
  }
  
  # If still no data, create sample data
  if (!exists("economic_data")) {
    cat("⚠️ Creating sample data for demonstration\n")
    economic_data <- data.frame(
      country_name = rep(c("United States", "China", "Germany", "Japan", "United Kingdom"), each = 10),
      country_code = rep(c("USA", "CHN", "DEU", "JPN", "GBR"), each = 10),
      indicator_name = "Sample Data",
      indicator_code = "SAMPLE",
      year = rep(2014:2023, 5),
      value = runif(50, 1e12, 25e12),
      indicator_short = "gdp",
      yoy_change = runif(50, -1e11, 1e11),
      yoy_pct_change = runif(50, -5, 5),
      growth_rate = runif(50, -5, 5),
      trend_indicator = sample(c("increasing", "decreasing", "stable"), 50, replace = TRUE),
      value_3yr_avg = runif(50, 1e12, 25e12),
      data_quality_score = 1,
      last_updated = Sys.Date(),
      stringsAsFactors = FALSE
    )
  }
})

# Get unique countries for selection (filter out regional aggregates)
countries <- economic_data %>%
  filter(!grepl("World|Region|Income|Eastern|Western|Central|Northern|Southern|Arab|Euro|OECD|IBRD|IDA", country_name)) %>%
  distinct(country_name) %>%
  arrange(country_name) %>%
  pull(country_name)

# Get available years
years <- economic_data %>%
  distinct(year) %>%
  arrange(year) %>%
  pull(year)

# Enhanced Custom CSS styling with advanced professional polish
custom_css <- "
  /* CSS Variables for consistent theming */
  :root {
    --primary-blue: #2563eb;
    --primary-dark: #1e40af;
    --primary-light: #3b82f6;
    --success-green: #10b981;
    --warning-orange: #f59e0b;
    --error-red: #ef4444;
    --background-gray: #f8fafc;
    --card-white: #ffffff;
    --border-gray: #e2e8f0;
    --text-dark: #1e293b;
    --text-muted: #64748b;
    --text-light: #94a3b8;
    --shadow-sm: 0 1px 2px rgba(0, 0, 0, 0.05);
    --shadow-md: 0 4px 6px rgba(0, 0, 0, 0.07);
    --shadow-lg: 0 10px 15px rgba(0, 0, 0, 0.1);
    --transition-fast: 0.15s ease-in-out;
    --transition-normal: 0.3s ease-in-out;
    --transition-slow: 0.5s ease-in-out;
  }

  /* Main dashboard styling with smooth transitions */
  .content-wrapper, .right-side {
    background: linear-gradient(135deg, var(--background-gray) 0%, #f1f5f9 100%);
    transition: background var(--transition-normal);
  }
  
  /* Enhanced sidebar styling with hover effects */
  .main-sidebar {
    background-color: var(--card-white);
    border-right: 1px solid var(--border-gray);
    box-shadow: var(--shadow-md);
    transition: box-shadow var(--transition-normal);
  }
  
  .main-sidebar:hover {
    box-shadow: var(--shadow-lg);
  }
  
  .sidebar-menu > li > a {
    color: var(--text-dark);
    border-left: 3px solid transparent;
    padding: 15px 20px;
    transition: all var(--transition-fast);
    position: relative;
    overflow: hidden;
  }
  
  .sidebar-menu > li > a::before {
    content: '';
    position: absolute;
    top: 0;
    left: -100%;
    width: 100%;
    height: 100%;
    background: linear-gradient(90deg, transparent, rgba(37, 99, 235, 0.1), transparent);
    transition: left var(--transition-normal);
  }
  
  .sidebar-menu > li > a:hover {
    background-color: #f1f5f9;
    border-left-color: var(--primary-blue);
    transform: translateX(2px);
  }
  
  .sidebar-menu > li > a:hover::before {
    left: 100%;
  }
  
  /* Enhanced control card styling with advanced effects */
  .control-card {
    background: var(--card-white);
    border: 1px solid var(--border-gray);
    border-radius: 12px;
    padding: 24px;
    margin-bottom: 20px;
    box-shadow: var(--shadow-sm);
    transition: all var(--transition-normal);
    position: relative;
    overflow: hidden;
  }
  
  .control-card::before {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    height: 3px;
    background: linear-gradient(90deg, var(--primary-blue), var(--primary-light));
    transform: scaleX(0);
    transition: transform var(--transition-normal);
  }
  
  .control-card:hover {
    box-shadow: var(--shadow-lg);
    transform: translateY(-2px);
    border-color: var(--primary-light);
  }
  
  .control-card:hover::before {
    transform: scaleX(1);
  }
  
  .control-card h4 {
    color: var(--text-dark);
    font-weight: 600;
    margin-bottom: 16px;
    font-size: 16px;
    display: flex;
    align-items: center;
    gap: 10px;
    position: relative;
  }
  
  .control-card h4::after {
    content: '';
    flex: 1;
    height: 1px;
    background: linear-gradient(90deg, var(--border-gray), transparent);
    margin-left: 12px;
  }
  
  /* Enhanced header styling with gradient and animations */
  .main-header .navbar {
    background: linear-gradient(135deg, var(--primary-blue) 0%, var(--primary-dark) 100%);
    border-bottom: none;
    box-shadow: var(--shadow-md);
    position: relative;
    overflow: hidden;
  }
  
  .main-header .navbar::before {
    content: '';
    position: absolute;
    top: 0;
    left: -100%;
    width: 100%;
    height: 100%;
    background: linear-gradient(90deg, transparent, rgba(255, 255, 255, 0.1), transparent);
    animation: shimmer 3s infinite;
  }
  
  @keyframes shimmer {
    0% { left: -100%; }
    100% { left: 100%; }
  }
  
  .main-header .navbar-brand {
    color: #ffffff !important;
    font-weight: 700;
    font-size: 20px;
    text-shadow: 0 1px 2px rgba(0, 0, 0, 0.1);
    transition: all var(--transition-fast);
  }
  
  .main-header .navbar-brand:hover {
    transform: scale(1.05);
    text-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
  }
  
  /* Enhanced box styling with professional polish */
  .box {
    border-radius: 12px;
    box-shadow: var(--shadow-sm);
    border: 1px solid var(--border-gray);
    transition: all var(--transition-normal);
    overflow: hidden;
    position: relative;
  }
  
  .box::before {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    height: 4px;
    background: linear-gradient(90deg, var(--primary-blue), var(--success-green), var(--warning-orange));
    opacity: 0;
    transition: opacity var(--transition-normal);
  }
  
  .box:hover {
    box-shadow: var(--shadow-lg);
    transform: translateY(-1px);
  }
  
  .box:hover::before {
    opacity: 1;
  }
  
  .box-header {
    background: linear-gradient(135deg, var(--card-white) 0%, #f8fafc 100%);
    border-bottom: 1px solid var(--border-gray);
    padding: 20px 24px;
    position: relative;
  }
  
  .box-title {
    color: var(--text-dark);
    font-weight: 600;
    font-size: 18px;
    display: flex;
    align-items: center;
    gap: 8px;
  }
  
  /* Enhanced tab styling with smooth transitions */
  .nav-tabs {
    border-bottom: 2px solid var(--border-gray);
    margin-bottom: 20px;
  }
  
  .nav-tabs > li > a {
    color: var(--text-muted);
    border: none;
    border-radius: 8px 8px 0 0;
    margin-right: 6px;
    padding: 12px 20px;
    font-weight: 500;
    transition: all var(--transition-fast);
    position: relative;
    overflow: hidden;
  }
  
  .nav-tabs > li > a::before {
    content: '';
    position: absolute;
    bottom: 0;
    left: 0;
    width: 100%;
    height: 3px;
    background: var(--primary-blue);
    transform: scaleX(0);
    transition: transform var(--transition-normal);
  }
  
  .nav-tabs > li.active > a {
    background: linear-gradient(135deg, var(--primary-blue) 0%, var(--primary-light) 100%);
    color: #ffffff;
    border: none;
    box-shadow: var(--shadow-md);
  }
  
  .nav-tabs > li.active > a::before {
    transform: scaleX(1);
  }
  
  .nav-tabs > li > a:hover:not(.active) {
    background: linear-gradient(135deg, #f1f5f9 0%, #e2e8f0 100%);
    color: var(--primary-blue);
    transform: translateY(-1px);
  }
  
  /* Enhanced form controls with focus states */
  .form-control {
    border: 2px solid var(--border-gray);
    border-radius: 8px;
    padding: 10px 14px;
    font-size: 14px;
    transition: all var(--transition-fast);
    background-color: var(--card-white);
  }
  
  .form-control:focus {
    border-color: var(--primary-blue);
    box-shadow: 0 0 0 4px rgba(37, 99, 235, 0.1);
    outline: none;
    transform: scale(1.02);
  }
  
  .form-control:hover:not(:focus) {
    border-color: var(--primary-light);
    box-shadow: var(--shadow-sm);
  }
  
  /* Enhanced radio buttons with custom styling */
  .radio {
    margin-bottom: 12px;
  }
  
  .radio label {
    color: var(--text-dark);
    font-weight: 500;
    padding-left: 30px;
    position: relative;
    cursor: pointer;
    transition: color var(--transition-fast);
  }
  
  .radio label:hover {
    color: var(--primary-blue);
  }
  
  .radio input[type='radio'] {
    position: absolute;
    left: 0;
    top: 2px;
    width: 18px;
    height: 18px;
    accent-color: var(--primary-blue);
  }
  
  /* Enhanced slider styling */
  .irs-bar {
    background: linear-gradient(90deg, var(--primary-blue), var(--primary-light));
    border-radius: 4px;
  }
  
  .irs-from, .irs-to, .irs-single {
    background: var(--primary-blue);
    border-radius: 6px;
    font-weight: 500;
    box-shadow: var(--shadow-sm);
  }
  
  .irs-handle {
    background: var(--card-white);
    border: 3px solid var(--primary-blue);
    box-shadow: var(--shadow-md);
    transition: all var(--transition-fast);
  }
  
  .irs-handle:hover {
    transform: scale(1.1);
    box-shadow: var(--shadow-lg);
  }
  
  /* Enhanced loading states with animations */
  .shiny-output-error {
    color: var(--error-red);
    background: linear-gradient(135deg, #fef2f2 0%, #fee2e2 100%);
    border: 2px solid #fecaca;
    border-radius: 8px;
    padding: 16px;
    margin: 16px 0;
    box-shadow: var(--shadow-sm);
    animation: shake 0.5s ease-in-out;
  }
  
  @keyframes shake {
    0%, 100% { transform: translateX(0); }
    25% { transform: translateX(-5px); }
    75% { transform: translateX(5px); }
  }
  
  .shiny-spinner-output-container {
    position: relative;
  }
  
  .shiny-spinner-output-container::before {
    content: '';
    position: absolute;
    top: 50%;
    left: 50%;
    width: 40px;
    height: 40px;
    margin: -20px 0 0 -20px;
    border: 3px solid var(--border-gray);
    border-top-color: var(--primary-blue);
    border-radius: 50%;
    animation: spin 1s linear infinite;
    z-index: 1000;
  }
  
  @keyframes spin {
    to { transform: rotate(360deg); }
  }
  
  /* Professional badges and indicators */
  .badge {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    padding: 4px 8px;
    border-radius: 12px;
    font-size: 12px;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }
  
  .badge-success {
    background: linear-gradient(135deg, var(--success-green) 0%, #059669 100%);
    color: white;
  }
  
  .badge-warning {
    background: linear-gradient(135deg, var(--warning-orange) 0%, #d97706 100%);
    color: white;
  }
  
  .badge-error {
    background: linear-gradient(135deg, var(--error-red) 0%, #dc2626 100%);
    color: white;
  }
  
  .badge-info {
    background: linear-gradient(135deg, var(--primary-blue) 0%, var(--primary-dark) 100%);
    color: white;
  }
  
  /* Trend indicators with animations */
  .trend-indicator {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    font-weight: 600;
    transition: all var(--transition-fast);
  }
  
  .trend-up {
    color: var(--success-green);
    animation: bounce-up 2s infinite;
  }
  
  .trend-down {
    color: var(--error-red);
    animation: bounce-down 2s infinite;
  }
  
  .trend-stable {
    color: var(--text-muted);
  }
  
  @keyframes bounce-up {
    0%, 100% { transform: translateY(0); }
    50% { transform: translateY(-2px); }
  }
  
  @keyframes bounce-down {
    0%, 100% { transform: translateY(0); }
    50% { transform: translateY(2px); }
  }
  
  /* Enhanced tooltips */
  .tooltip-custom {
    position: relative;
    cursor: help;
  }
  
  /* Fix for DataTable text visibility */
  .dataTables_wrapper {
    color: #000000 !important;
  }
  
  .dataTables_wrapper table {
    color: #000000 !important;
  }
  
  .dataTables_wrapper table tbody tr td {
    color: #000000 !important;
    font-size: 13px !important;
  }
  
  .dataTables_wrapper table thead tr th {
    color: #000000 !important;
    background-color: #f8fafc !important;
    font-weight: 600 !important;
  }
  
  .dataTables_info {
    color: #000000 !important;
  }
  
  .dataTables_filter label {
    color: #000000 !important;
  }
  
  .dataTables_filter input {
    color: #000000 !important;
    background-color: #ffffff !important;
  }
  
  .tooltip-custom::after {
    content: attr(data-tooltip);
    position: absolute;
    bottom: 100%;
    left: 50%;
    transform: translateX(-50%);
    background: var(--text-dark);
    color: white;
    padding: 8px 12px;
    border-radius: 6px;
    font-size: 12px;
    white-space: nowrap;
    opacity: 0;
    pointer-events: none;
    transition: opacity var(--transition-fast);
    z-index: 1000;
  }
  
  .tooltip-custom:hover::after {
    opacity: 1;
  }
  
  /* Professional typography with better hierarchy */
  body {
    font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    color: var(--text-dark);
    line-height: 1.6;
    font-size: 14px;
  }
  
  h1, h2, h3, h4, h5, h6 {
    color: var(--text-dark);
    font-weight: 600;
    line-height: 1.3;
    margin-bottom: 0.5em;
  }
  
  /* Enhanced button styling */
  .btn {
    border-radius: 8px;
    font-weight: 500;
    padding: 10px 20px;
    transition: all var(--transition-fast);
    border: none;
    cursor: pointer;
    display: inline-flex;
    align-items: center;
    gap: 6px;
  }
  
  .btn-primary {
    background: linear-gradient(135deg, var(--primary-blue) 0%, var(--primary-dark) 100%);
    color: white;
    box-shadow: var(--shadow-sm);
  }
  
  .btn-primary:hover {
    transform: translateY(-1px);
    box-shadow: var(--shadow-md);
  }
  
  .btn-outline-secondary {
    background: transparent;
    border: 2px solid var(--border-gray);
    color: var(--text-muted);
  }
  
  .btn-outline-secondary:hover {
    background: var(--border-gray);
    color: var(--text-dark);
    transform: translateY(-1px);
  }
  
  /* Responsive design enhancements */
  @media (max-width: 1200px) {
    .control-card {
      padding: 20px;
    }
    
    .box-title {
      font-size: 16px;
    }
  }
  
  @media (max-width: 768px) {
    .control-card {
      margin-bottom: 15px;
      padding: 16px;
      border-radius: 8px;
    }
    
    .control-card h4 {
      font-size: 14px;
    }
    
    .main-header .navbar-brand {
      font-size: 16px;
    }
    
    .nav-tabs > li > a {
      padding: 8px 12px;
      font-size: 13px;
    }
    
    .form-control {
      padding: 8px 12px;
      font-size: 13px;
    }
    
    .box {
      border-radius: 8px;
    }
    
    .box-header {
      padding: 16px 20px;
    }
    
    .box-title {
      font-size: 15px;
    }
  }
  
  @media (max-width: 480px) {
    .control-card {
      padding: 12px;
      margin-bottom: 12px;
    }
    
    .control-card h4 {
      font-size: 13px;
      gap: 6px;
    }
    
    .nav-tabs > li > a {
      padding: 6px 10px;
      font-size: 12px;
      margin-right: 3px;
    }
    
    .form-control {
      padding: 6px 10px;
      font-size: 12px;
    }
    
    .btn {
      padding: 8px 16px;
      font-size: 13px;
    }
  }
  
  /* Accessibility improvements */
  @media (prefers-reduced-motion: reduce) {
    *, *::before, *::after {
      animation-duration: 0.01ms !important;
      animation-iteration-count: 1 !important;
      transition-duration: 0.01ms !important;
    }
  }
  
  /* Focus indicators for keyboard navigation */
  .form-control:focus,
  .btn:focus,
  .nav-tabs > li > a:focus {
    outline: 2px solid var(--primary-blue);
    outline-offset: 2px;
  }
  
  /* High contrast mode support */
  @media (prefers-contrast: high) {
    :root {
      --border-gray: #000000;
      --text-muted: #000000;
      --background-gray: #ffffff;
    }
  }
  
  /* Dark mode support (future enhancement) */
  @media (prefers-color-scheme: dark) {
    :root {
      --background-gray: #1e293b;
      --card-white: #334155;
      --text-dark: #f1f5f9;
      --text-muted: #cbd5e1;
      --border-gray: #475569;
    }
  }
  
  /* Print styles */
  @media print {
    .control-card,
    .box {
      box-shadow: none !important;
      border: 1px solid var(--border-gray) !important;
    }
    
    .main-header .navbar {
      background: var(--text-dark) !important;
    }
    
    .nav-tabs > li.active > a {
      background: var(--text-dark) !important;
    }
  }
  
  /* Custom scrollbar styling */
  ::-webkit-scrollbar {
    width: 8px;
    height: 8px;
  }
  
  ::-webkit-scrollbar-track {
    background: var(--background-gray);
    border-radius: 4px;
  }
  
  ::-webkit-scrollbar-thumb {
    background: var(--border-gray);
    border-radius: 4px;
    transition: background var(--transition-fast);
  }
  
  ::-webkit-scrollbar-thumb:hover {
    background: var(--text-muted);
  }
  
  /* Smooth page transitions */
  .content-wrapper {
    animation: fadeIn 0.5s ease-in-out;
  }
  
  @keyframes fadeIn {
    from { opacity: 0; transform: translateY(10px); }
    to { opacity: 1; transform: translateY(0); }
  }
"

# Define UI
ui <- dashboardPage(
  # Dashboard Header
  dashboardHeader(
    title = "Global Consumer Pulse",
    titleWidth = 300
  ),
  
  # Dashboard Sidebar
  dashboardSidebar(
    width = 300,
    div(
      style = "padding: 20px;",
      
      # Country Selection Card
      div(
        class = "control-card",
        h4("🌍 Country Selection"),
        selectInput(
          "primary_country",
          "Primary Country:",
          choices = countries,
          selected = "United States",
          width = "100%"
        ),
        checkboxGroupInput(
          "compare_countries",
          "Compare with (up to 5):",
          choices = head(countries[countries != "United States"], 10),
          selected = c("China", "Germany", "Japan"),
          width = "100%"
        )
      ),
      
      # Economic Metric Card with enhanced styling
      div(
        class = "control-card",
        h4("📊 Economic Metric", 
           tags$span(class = "badge badge-info tooltip-custom", 
                    `data-tooltip` = "Select economic indicator to analyze",
                    "INFO")),
        div(
          class = "metric-selection-container",
          radioButtons(
            "metric",
            "",
            choices = list(
              "💰 GDP (Current US$)" = "gdp",
              "📈 Consumer Price Index" = "cpi"
            ),
            selected = "gdp",
            width = "100%"
          )
        ),
        # Add metric description
        div(
          id = "metric-description",
          class = "metric-description",
          style = "margin-top: 12px; padding: 8px 12px; background: #f8fafc; border-radius: 6px; font-size: 12px; color: #64748b;",
          textOutput("metric_description")
        )
      ),
      
      # Time Range Card with enhanced features
      div(
        class = "control-card",
        h4("📅 Time Range", 
           tags$span(class = "badge badge-success tooltip-custom",
                    `data-tooltip` = "Adjust time period for analysis",
                    "RANGE")),
        sliderInput(
          "year_range",
          "",
          min = min(years, na.rm = TRUE),
          max = max(years, na.rm = TRUE),
          value = c(2015, max(years, na.rm = TRUE)),
          step = 1,
          sep = "",
          width = "100%"
        ),
        # Add time range summary
        div(
          class = "time-range-summary",
          style = "margin-top: 12px; display: flex; justify-content: space-between; font-size: 12px; color: #64748b;",
          div(
            class = "range-info",
            textOutput("time_range_info", inline = TRUE)
          ),
          div(
            class = "data-coverage",
            textOutput("data_coverage_info", inline = TRUE)
          )
        )
      )
    )
  ),
  
  # Dashboard Body
  dashboardBody(
    # Include custom CSS and JavaScript
    tags$head(
      tags$style(HTML(custom_css)),
      tags$script(HTML("
        // Enhanced JavaScript for professional interactions
        $(document).ready(function() {
          
          // Smooth loading animations
          $('.box').each(function(index) {
            $(this).css('opacity', '0').delay(index * 100).animate({opacity: 1}, 500);
          });
          
          // Enhanced hover effects for interactive elements
          $('.control-card').hover(
            function() {
              $(this).find('h4').css('color', '#2563eb');
            },
            function() {
              $(this).find('h4').css('color', '#1e293b');
            }
          );
          
          // Loading state management
          $(document).on('shiny:busy', function(event) {
            $('.shiny-output-container').addClass('loading-state');
            $('<div class=\"loading-overlay\"><div class=\"spinner\"></div><span>Loading...</span></div>')
              .appendTo('.shiny-output-container');
          });
          
          $(document).on('shiny:idle', function(event) {
            $('.shiny-output-container').removeClass('loading-state');
            $('.loading-overlay').remove();
          });
          
          // Smooth tab transitions
          $('.nav-tabs a').on('click', function(e) {
            e.preventDefault();
            var target = $(this).attr('href');
            $('.tab-pane').fadeOut(200, function() {
              $(target).fadeIn(300);
            });
          });
          
          // Enhanced form interactions
          $('.form-control').on('focus', function() {
            $(this).parent().addClass('form-group-focused');
          }).on('blur', function() {
            $(this).parent().removeClass('form-group-focused');
          });
          
          // Notification enhancements
          $(document).on('shiny:notification', function(event) {
            setTimeout(function() {
              $('.shiny-notification').addClass('notification-enhanced');
            }, 100);
          });
          
          // Responsive sidebar toggle
          if ($(window).width() < 768) {
            $('.main-sidebar').addClass('sidebar-mini');
          }
          
          $(window).resize(function() {
            if ($(window).width() < 768) {
              $('.main-sidebar').addClass('sidebar-mini');
            } else {
              $('.main-sidebar').removeClass('sidebar-mini');
            }
          });
          
          // Enhanced plotly chart interactions
          $(document).on('plotly_afterplot', function() {
            $('.plotly .plotly-graph-div').addClass('chart-loaded');
          });
          
          // Accessibility improvements
          $('.btn, .form-control, .nav-tabs a').on('keydown', function(e) {
            if (e.key === 'Enter' || e.key === ' ') {
              $(this).click();
            }
          });
          
          // Performance optimization - lazy loading for heavy elements
          var lazyElements = $('.box[data-lazy=\"true\"]');
          if (lazyElements.length > 0) {
            var observer = new IntersectionObserver(function(entries) {
              entries.forEach(function(entry) {
                if (entry.isIntersecting) {
                  $(entry.target).removeClass('lazy-load').addClass('loaded');
                  observer.unobserve(entry.target);
                }
              });
            });
            
            lazyElements.each(function() {
              observer.observe(this);
            });
          }
        });
        
        // Additional CSS for JavaScript enhancements
        var additionalCSS = `
          .loading-state {
            position: relative;
            pointer-events: none;
          }
          
          .loading-overlay {
            position: absolute;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            background: rgba(248, 250, 252, 0.9);
            display: flex;
            flex-direction: column;
            align-items: center;
            justify-content: center;
            z-index: 1000;
            backdrop-filter: blur(2px);
          }
          
          .spinner {
            width: 32px;
            height: 32px;
            border: 3px solid #e2e8f0;
            border-top-color: #2563eb;
            border-radius: 50%;
            animation: spin 1s linear infinite;
            margin-bottom: 8px;
          }
          
          .loading-overlay span {
            color: #64748b;
            font-size: 14px;
            font-weight: 500;
          }
          
          .form-group-focused {
            transform: scale(1.02);
            transition: transform 0.2s ease;
          }
          
          .notification-enhanced {
            border-radius: 8px !important;
            box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15) !important;
            border-left: 4px solid #2563eb !important;
          }
          
          .sidebar-mini .main-sidebar {
            width: 60px !important;
          }
          
          .sidebar-mini .control-card h4::after {
            display: none;
          }
          
          .chart-loaded {
            animation: chartFadeIn 0.5s ease-in-out;
          }
          
          @keyframes chartFadeIn {
            from { opacity: 0; transform: scale(0.95); }
            to { opacity: 1; transform: scale(1); }
          }
          
          .lazy-load {
            opacity: 0.3;
            transform: translateY(20px);
            transition: all 0.5s ease;
          }
          
          .loaded {
            opacity: 1;
            transform: translateY(0);
          }
        `;
        
        $('<style>').text(additionalCSS).appendTo('head');
      "))
    ),
    
    # Main tabbed interface
    tabsetPanel(
      id = "main_tabs",
      type = "tabs",
      
      # Overview Tab
      tabPanel(
        "Dashboard Overview",
        value = "overview",
        
        fluidRow(
          # World Map Section with enhanced title
          box(
            title = div(
              style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
              div(
                style = "display: flex; align-items: center; gap: 8px;",
                "🌍 Global Economic Overview",
                tags$span(class = "badge badge-info", "INTERACTIVE")
              ),
              div(
                style = "font-size: 12px; color: #64748b;",
                textOutput("map_status", inline = TRUE)
              )
            ), 
            status = "primary", 
            solidHeader = TRUE,
            width = 12, 
            height = "450px",
            plotlyOutput("world_map", height = "380px")
          )
        ),
        
        fluidRow(
          # Time Series Chart with enhanced title
          box(
            title = div(
              style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
              div(
                style = "display: flex; align-items: center; gap: 8px;",
                "📈 Economic Trend Analysis",
                tags$span(class = "badge badge-success", "TRENDS")
              ),
              div(
                style = "font-size: 12px; color: #64748b;",
                textOutput("trend_status", inline = TRUE)
              )
            ),
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            height = "400px",
            plotlyOutput("time_series", height = "330px")
          ),
          
          # Top Countries Table with enhanced title
          box(
            title = div(
              style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
              div(
                style = "display: flex; align-items: center; gap: 8px;",
                "🏆 Top 10 Countries",
                tags$span(class = "badge badge-warning", "RANKINGS")
              ),
              div(
                style = "font-size: 12px; color: #64748b;",
                textOutput("rankings_status", inline = TRUE)
              )
            ),
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            height = "400px",
            DT::dataTableOutput("rankings_table", height = "330px")
          )
        )
      ),
      
      # Compare Countries Tab
      tabPanel(
        "Compare Countries",
        value = "compare",
        
        fluidRow(
          # Interactive Country Selection Panel with enhanced styling
          box(
            title = div(
              style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
              div(
                style = "display: flex; align-items: center; gap: 8px;",
                "🎯 Country Selection Manager",
                tags$span(class = "badge badge-info", "MANAGER")
              ),
              div(
                style = "font-size: 12px; color: #64748b;",
                textOutput("selection_status", inline = TRUE)
              )
            ),
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            height = "500px",
            
            # Primary country display
            div(
              class = "control-card",
              style = "margin-bottom: 15px;",
              h5("Primary Country", style = "color: #1e293b; margin-bottom: 10px;"),
              div(
                style = "padding: 10px; background: #eff6ff; border: 2px solid #2563eb; border-radius: 6px; text-align: center;",
                strong(textOutput("primary_country_display", inline = TRUE), style = "color: #1e40af; font-size: 16px;")
              )
            ),
            
            # Add country interface
            div(
              class = "control-card",
              style = "margin-bottom: 15px;",
              h5("Add Countries to Compare", style = "color: #1e293b; margin-bottom: 10px;"),
              selectInput(
                "country_to_add",
                NULL,
                choices = NULL,
                selected = NULL,
                width = "100%"
              ),
              actionButton(
                "add_country_btn",
                "➕ Add Country",
                class = "btn-primary",
                style = "width: 100%; margin-top: 5px;"
              )
            ),
            
            # Selected countries list with remove buttons
            div(
              class = "control-card",
              h5("Selected Countries for Comparison", style = "color: #1e293b; margin-bottom: 10px;"),
              div(
                id = "selected_countries_list",
                style = "max-height: 200px; overflow-y: auto;",
                uiOutput("selected_countries_ui")
              ),
              br(),
              actionButton(
                "clear_all_btn",
                "🗑️ Clear All",
                class = "btn-outline-secondary",
                style = "width: 100%;"
              )
            )
          ),
          
          # Country Comparison Chart with enhanced styling
          box(
            title = div(
              style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
              div(
                style = "display: flex; align-items: center; gap: 8px;",
                "📊 Multi-Country Comparison Analysis",
                tags$span(class = "badge badge-success", "COMPARE")
              ),
              div(
                style = "font-size: 12px; color: #64748b;",
                textOutput("comparison_status", inline = TRUE)
              )
            ),
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            height = "500px",
            plotlyOutput("comparison_chart", height = "430px")
          )
        ),
        
        fluidRow(
          # Enhanced Comparison Metrics Table with professional styling
          box(
            title = div(
              style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
              div(
                style = "display: flex; align-items: center; gap: 8px;",
                "📈 Detailed Comparison Metrics",
                tags$span(class = "badge badge-warning", "METRICS")
              ),
              div(
                style = "font-size: 12px; color: #64748b;",
                textOutput("metrics_status", inline = TRUE)
              )
            ),
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "400px",
            DT::dataTableOutput("comparison_table", height = "330px")
          ),
          
          # Percentage Differences Analysis with enhanced styling
          box(
            title = div(
              style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
              div(
                style = "display: flex; align-items: center; gap: 8px;",
                "🔄 Percentage Differences vs Primary",
                tags$span(class = "badge badge-error", "ANALYSIS")
              ),
              div(
                style = "font-size: 12px; color: #64748b;",
                textOutput("differences_status", inline = TRUE)
              )
            ),
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "400px",
            DT::dataTableOutput("percentage_differences_table", height = "330px")
          )
        )
      )
    )
  )
)

# Define Server Logic with Reactive Programming
server <- function(input, output, session) {
  
  # Input validation and error handling
  validate_inputs <- function() {
    validate(
      need(input$primary_country, "Please select a primary country"),
      need(input$metric, "Please select an economic metric"),
      need(input$year_range, "Please select a year range")
    )
  }
  
  # Enhanced UI feedback outputs
  output$metric_description <- renderText({
    switch(input$metric,
      "gdp" = "Gross Domestic Product measures the total economic output of a country in current US dollars.",
      "cpi" = "Consumer Price Index measures the average change in prices paid by consumers for goods and services.",
      "Select a metric to see its description."
    )
  })
  
  output$time_range_info <- renderText({
    if (!is.null(input$year_range)) {
      years_span <- input$year_range[2] - input$year_range[1] + 1
      paste0("📊 ", years_span, " years selected")
    } else {
      "Select time range"
    }
  })
  
  output$data_coverage_info <- renderText({
    if (!is.null(input$year_range) && !is.null(filtered_data())) {
      available_years <- length(unique(filtered_data()$year))
      total_possible <- input$year_range[2] - input$year_range[1] + 1
      coverage_pct <- round((available_years / total_possible) * 100, 1)
      paste0("📈 ", coverage_pct, "% data coverage")
    } else {
      "Calculating coverage..."
    }
  })
  
  # Status indicators for enhanced boxes
  output$map_status <- renderText({
    if (!is.null(world_map_data())) {
      countries_count <- nrow(world_map_data())
      latest_year <- max(world_map_data()$year, na.rm = TRUE)
      paste0("📊 ", countries_count, " countries • ", latest_year, " data")
    } else {
      "Loading map data..."
    }
  })
  
  output$trend_status <- renderText({
    if (!is.null(input$compare_countries) && !is.null(input$primary_country)) {
      total_countries <- length(c(input$primary_country, input$compare_countries))
      paste0("🌍 ", total_countries, " countries • ", 
             input$year_range[1], "-", input$year_range[2])
    } else {
      "Select countries to analyze"
    }
  })
  
  output$rankings_status <- renderText({
    if (!is.null(rankings_data())) {
      metric_name <- switch(input$metric,
        "gdp" = "GDP",
        "cpi" = "CPI",
        "Metric"
      )
      paste0("🏆 Top 10 by ", metric_name, " • Live rankings")
    } else {
      "Loading rankings..."
    }
  })
  
  # Comparison tab status indicators
  output$selection_status <- renderText({
    if (!is.null(comparison_countries())) {
      selected_count <- length(comparison_countries())
      max_count <- 5
      paste0("📊 ", selected_count, "/", max_count, " countries")
    } else {
      "No countries selected"
    }
  })
  
  output$comparison_status <- renderText({
    if (!is.null(input$primary_country) && !is.null(comparison_countries())) {
      total_countries <- length(comparison_countries()) + 1
      paste0("🌍 ", total_countries, " countries • ", 
             switch(input$metric, "gdp" = "GDP", "cpi" = "CPI", "Metric"), " comparison")
    } else {
      "Select countries to compare"
    }
  })
  
  output$metrics_status <- renderText({
    if (!is.null(comparison_countries()) && length(comparison_countries()) > 0) {
      paste0("📈 Latest values • ", max(years, na.rm = TRUE), " data")
    } else {
      "Add countries to see metrics"
    }
  })
  
  output$differences_status <- renderText({
    if (!is.null(input$primary_country) && !is.null(comparison_countries()) && length(comparison_countries()) > 0) {
      paste0("🔄 vs ", input$primary_country, " • Percentage analysis")
    } else {
      "Select primary and comparison countries"
    }
  })
  
  # Reactive data filtering based on user inputs
  filtered_data <- reactive({
    validate_inputs()
    
    tryCatch({
      # Filter data based on selected metric and year range
      data <- economic_data %>%
        filter(
          indicator_short == input$metric,
          year >= input$year_range[1],
          year <= input$year_range[2],
          !is.na(value)
        )
      
      # Validate that we have data
      validate(
        need(nrow(data) > 0, "No data available for the selected criteria")
      )
      
      return(data)
    }, error = function(e) {
      showNotification(
        paste("Error filtering data:", e$message),
        type = "error",
        duration = 5
      )
      return(NULL)
    })
  })
  
  # Reactive data for primary country
  primary_country_data <- reactive({
    req(filtered_data())
    
    filtered_data() %>%
      filter(country_name == input$primary_country) %>%
      arrange(year)
  })
  
  # Reactive data for comparison countries
  comparison_countries_data <- reactive({
    req(filtered_data())
    
    if (length(input$compare_countries) > 0) {
      filtered_data() %>%
        filter(country_name %in% input$compare_countries) %>%
        arrange(country_name, year)
    } else {
      data.frame()
    }
  })
  
  # Reactive data for world map (latest year data)
  world_map_data <- reactive({
    req(filtered_data())
    
    latest_year <- max(filtered_data()$year, na.rm = TRUE)
    
    filtered_data() %>%
      filter(
        year == latest_year,
        # Filter out regional aggregates and income groups
        !grepl("World|Region|Income|Eastern|Western|Central|Northern|Southern|Arab|Euro|OECD|IBRD|IDA|dividend|members|Fragile|Small states|Least developed|Sub-Saharan|Latin America|East Asia|South Asia|Middle East|North America|European Union", country_name, ignore.case = TRUE)
      ) %>%
      arrange(desc(value))
  })
  
  # Enhanced reactive data for professional rankings table (top 10 countries)
  rankings_data <- reactive({
    req(world_map_data())
    
    tryCatch({
      # Get the latest year data for rankings
      latest_year <- max(world_map_data()$year, na.rm = TRUE)
      
      # Enhanced rankings with better year-over-year calculations
      rankings <- world_map_data() %>%
        # Ensure we have valid data
        filter(!is.na(value), value > 0) %>%
        # Sort by value descending to get top performers
        arrange(desc(value)) %>%
        # Take top 10 countries
        head(10) %>%
        mutate(
          rank = row_number(),
          
          # Enhanced value formatting with proper number formatting
          formatted_value = case_when(
            input$metric == "gdp" & value >= 1e12 ~ paste0("$", format(value / 1e12, digits = 2, nsmall = 1), "T"),
            input$metric == "gdp" & value >= 1e9 ~ paste0("$", format(value / 1e9, digits = 2, nsmall = 1), "B"),
            input$metric == "gdp" ~ paste0("$", format(value / 1e6, digits = 2, nsmall = 1), "M"),
            input$metric == "cpi" ~ format(value, digits = 4, nsmall = 1),
            TRUE ~ paste0(format(value, digits = 3, nsmall = 1), "%")
          ),
          
          # Enhanced YoY change calculations with better handling
          yoy_change_formatted = case_when(
            is.na(yoy_pct_change) ~ "--",
            abs(yoy_pct_change) < 0.01 ~ "0.0%",  # Handle very small changes
            yoy_pct_change > 0 ~ paste0("+", format(yoy_pct_change, digits = 2, nsmall = 1), "%"),
            TRUE ~ paste0(format(yoy_pct_change, digits = 2, nsmall = 1), "%")
          ),
          
          # Enhanced trend indicators with more context
          trend_indicator = case_when(
            is.na(yoy_pct_change) ~ "unknown",
            yoy_pct_change > 2 ~ "increasing",
            yoy_pct_change < -2 ~ "decreasing", 
            TRUE ~ "stable"
          ),
          
          # Add additional metrics for enhanced display
          value_rank = rank,
          country_code_clean = country_code,
          latest_year = latest_year,
          
          # Calculate relative performance (percentile)
          performance_percentile = round((11 - rank) * 10, 0),  # Top rank gets 100%
          
          # Add growth category for better visualization
          growth_category = case_when(
            is.na(yoy_pct_change) ~ "No Data",
            yoy_pct_change > 5 ~ "Strong Growth",
            yoy_pct_change > 0 ~ "Positive Growth",
            yoy_pct_change > -5 ~ "Mild Decline",
            TRUE ~ "Strong Decline"
          )
        )
      
      # Validate that we have data to return
      if (nrow(rankings) == 0) {
        # Return empty data frame with proper structure
        return(data.frame(
          rank = integer(0),
          country_name = character(0),
          value = numeric(0),
          formatted_value = character(0),
          yoy_pct_change = numeric(0),
          yoy_change_formatted = character(0),
          trend_indicator = character(0),
          country_code = character(0),
          performance_percentile = numeric(0),
          growth_category = character(0),
          stringsAsFactors = FALSE
        ))
      }
      
      # Return enhanced rankings data
      rankings %>%
        select(
          rank, country_name, value, formatted_value, 
          yoy_pct_change, yoy_change_formatted, trend_indicator,
          country_code, performance_percentile, growth_category
        )
        
    }, error = function(e) {
      # Enhanced error handling with logging
      message("Error in rankings_data: ", e$message)
      
      # Return sample data for demonstration
      data.frame(
        rank = 1:5,
        country_name = c("United States", "China", "Germany", "Japan", "United Kingdom"),
        value = c(23.32e12, 17.73e12, 4.26e12, 4.94e12, 3.13e12),
        formatted_value = c("$23.3T", "$17.7T", "$4.3T", "$4.9T", "$3.1T"),
        yoy_pct_change = c(2.1, 8.1, 1.1, 0.6, 7.4),
        yoy_change_formatted = c("+2.1%", "+8.1%", "+1.1%", "+0.6%", "+7.4%"),
        trend_indicator = c("increasing", "increasing", "stable", "stable", "increasing"),
        country_code = c("USA", "CHN", "DEU", "JPN", "GBR"),
        performance_percentile = c(100, 90, 80, 70, 60),
        growth_category = c("Positive Growth", "Strong Growth", "Positive Growth", "Positive Growth", "Strong Growth"),
        stringsAsFactors = FALSE
      )
    })
  })
  
  # Reactive value to track selected country from map
  selected_country <- reactiveVal(NULL)
  
  # World Map Output with Advanced Features
  output$world_map <- renderPlotly({
    req(world_map_data())
    
    tryCatch({
      map_data <- world_map_data()
      
      # Dynamic color scaling based on selected metric
      if (input$metric == "gdp") {
        # GDP uses blue scale with logarithmic-like distribution
        color_scale <- list(
          c(0, "#f1f5f9"),      # Very light blue
          c(0.2, "#cbd5e1"),    # Light gray-blue
          c(0.4, "#94a3b8"),    # Medium gray-blue
          c(0.6, "#64748b"),    # Dark gray-blue
          c(0.8, "#3b82f6"),    # Primary blue
          c(1, "#1e40af")       # Dark blue
        )
        colorbar_title <- "GDP (Trillions USD)"
        metric_label <- "GDP"
      } else {
        # CPI uses green scale
        color_scale <- list(
          c(0, "#f0fdf4"),      # Very light green
          c(0.2, "#dcfce7"),    # Light green
          c(0.4, "#bbf7d0"),    # Medium light green
          c(0.6, "#86efac"),    # Medium green
          c(0.8, "#22c55e"),    # Primary green
          c(1, "#15803d")       # Dark green
        )
        colorbar_title <- "Consumer Price Index"
        metric_label <- "CPI"
      }
      
      # Professional hover tooltips with formatted economic data
      hover_text <- paste0(
        "<b>", map_data$country_name, "</b><br>",
        "━━━━━━━━━━━━━━━━━━━━<br>",
        case_when(
          input$metric == "gdp" ~ paste0(
            "💰 GDP: $", format(map_data$value / 1e12, digits = 2, nsmall = 1), " Trillion<br>",
            "📈 Growth: ", ifelse(is.na(map_data$yoy_pct_change), "N/A", 
                                 paste0(ifelse(map_data$yoy_pct_change >= 0, "+", ""), 
                                       format(map_data$yoy_pct_change, digits = 2, nsmall = 1), "%"))
          ),
          input$metric == "cpi" ~ paste0(
            "📊 CPI: ", format(map_data$value, digits = 4, nsmall = 1), "<br>",
            "📈 Change: ", ifelse(is.na(map_data$yoy_pct_change), "N/A", 
                                 paste0(ifelse(map_data$yoy_pct_change >= 0, "+", ""), 
                                       format(map_data$yoy_pct_change, digits = 2, nsmall = 1), "%"))
          ),
          TRUE ~ paste0("Value: ", format(map_data$value, digits = 3, nsmall = 1))
        ),
        "<br>📅 Year: ", map_data$year,
        "<br>🎯 Trend: ", map_data$trend_indicator,
        "<br><i>Click to select country</i>"
      )
      
      # Highlight selected countries (primary + comparison)
      all_selected <- c(input$primary_country, input$compare_countries)
      map_data$is_selected <- map_data$country_name %in% all_selected
      map_data$is_primary <- map_data$country_name == input$primary_country
      
      # Create choropleth map with advanced features
      p <- plot_ly(
        data = map_data,
        type = "choropleth",
        locations = ~country_code,
        z = ~value,
        text = hover_text,
        hovertemplate = "%{text}<extra></extra>",
        colorscale = color_scale,
        showscale = TRUE,
        colorbar = list(
          title = list(
            text = colorbar_title,
            font = list(size = 12, color = "#1e293b")
          ),
          thickness = 15,
          len = 0.7,
          x = 1.02,
          tickfont = list(size = 10, color = "#64748b")
        ),
        # Add stroke for selected countries
        line = list(
          color = ifelse(map_data$is_primary, "#dc2626", 
                        ifelse(map_data$is_selected, "#f59e0b", "#ffffff")),
          width = ifelse(map_data$is_selected, 3, 0.5)
        ),
        # Custom data for click events
        customdata = ~country_name,
        source = "world_map"
      ) %>%
        layout(
          title = list(
            text = paste0("🌍 Global ", metric_label, " Distribution (", max(map_data$year), ")"),
            font = list(size = 18, color = "#1e293b", family = "Arial, sans-serif"),
            x = 0.02,
            y = 0.95
          ),
          geo = list(
            showframe = FALSE,
            showcoastlines = TRUE,
            coastlinecolor = "#94a3b8",
            coastlinewidth = 0.5,
            projection = list(type = "natural earth"),
            bgcolor = "#f8fafc",
            showland = TRUE,
            landcolor = "#f1f5f9",
            showocean = TRUE,
            oceancolor = "#e0f2fe",
            showlakes = TRUE,
            lakecolor = "#e0f2fe"
          ),
          plot_bgcolor = "#f8fafc",
          paper_bgcolor = "#ffffff",
          margin = list(l = 0, r = 50, t = 60, b = 0),
          annotations = list(
            list(
              text = paste0("💡 ", 
                           case_when(
                             input$metric == "gdp" ~ "Darker blue indicates higher GDP",
                             input$metric == "cpi" ~ "Darker green indicates higher CPI",
                             TRUE ~ "Color intensity shows relative values"
                           )),
              x = 0.02, y = 0.02,
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 11, color = "#64748b", style = "italic"),
              bgcolor = "rgba(255,255,255,0.8)",
              bordercolor = "#e2e8f0",
              borderwidth = 1
            )
          )
        ) %>%
        config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"),
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "png",
            filename = paste0("global_", tolower(metric_label), "_map"),
            height = 500,
            width = 900,
            scale = 2
          )
        )
      
      # Add visual feedback for selected countries
      if (length(all_selected) > 0) {
        selected_data <- map_data %>% filter(country_name %in% all_selected)
        
        # Add markers for selected countries
        p <- p %>%
          add_markers(
            data = selected_data,
            x = 0, y = 0,  # Will be positioned by plotly based on country location
            marker = list(
              size = ifelse(selected_data$is_primary, 12, 8),
              color = ifelse(selected_data$is_primary, "#dc2626", "#f59e0b"),
              symbol = ifelse(selected_data$is_primary, "star", "circle"),
              line = list(color = "#ffffff", width = 2)
            ),
            showlegend = FALSE,
            hoverinfo = "skip"
          )
      }
      
      return(p)
      
    }, error = function(e) {
      plot_ly() %>%
        add_annotations(
          text = paste("❌ Error loading world map:", e$message),
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16, color = "#dc2626"),
          bgcolor = "#fef2f2",
          bordercolor = "#fecaca",
          borderwidth = 2
        ) %>%
        layout(
          plot_bgcolor = "#f8fafc",
          paper_bgcolor = "#ffffff"
        )
    })
  })
  
  # Handle map click events for country selection
  observeEvent(event_data("plotly_click", source = "world_map"), {
    click_data <- event_data("plotly_click", source = "world_map")
    
    if (!is.null(click_data) && !is.null(click_data$customdata)) {
      clicked_country <- click_data$customdata
      
      # Update selected country reactive value
      selected_country(clicked_country)
      
      # Update primary country selection
      updateSelectInput(
        session,
        "primary_country",
        selected = clicked_country
      )
      
      # Show notification with visual feedback
      showNotification(
        paste0("🎯 Selected: ", clicked_country),
        type = "message",
        duration = 2
      )
      
      # Optional: Add to comparison if not already selected
      current_compare <- input$compare_countries
      if (!clicked_country %in% current_compare && length(current_compare) < 5) {
        # Remove the clicked country from comparison if it was there
        new_compare <- setdiff(current_compare, clicked_country)
        
        updateCheckboxGroupInput(
          session,
          "compare_countries",
          selected = new_compare
        )
      }
    }
  })
  
  # Enhanced Time Series Chart Output with Multi-Country Support
  output$time_series <- renderPlotly({
    req(filtered_data())
    
    tryCatch({
      # Get all selected countries (primary + comparison, max 5 total)
      all_countries <- c(input$primary_country, input$compare_countries)
      all_countries <- unique(all_countries[!is.na(all_countries)])
      
      # Limit to 5 countries maximum for readability
      if (length(all_countries) > 5) {
        all_countries <- all_countries[1:5]
        showNotification(
          "Time series limited to 5 countries for optimal visualization",
          type = "message",
          duration = 3
        )
      }
      
      # Filter data for selected countries and time range
      time_series_data <- filtered_data() %>%
        filter(
          country_name %in% all_countries,
          year >= input$year_range[1],
          year <= input$year_range[2]
        ) %>%
        arrange(country_name, year)
      
      # Validate data availability
      if (nrow(time_series_data) == 0) {
        return(
          plot_ly() %>%
            add_annotations(
              text = "📊 No data available for the selected countries and time range",
              x = 0.5, y = 0.5,
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 14, color = "#64748b"),
              bgcolor = "#f8fafc",
              bordercolor = "#e2e8f0",
              borderwidth = 1
            ) %>%
            layout(
              plot_bgcolor = "#f8fafc",
              paper_bgcolor = "#ffffff"
            )
        )
      }
      
      # Professional color palette for up to 5 countries
      country_colors <- c(
        "#2563eb",  # Primary blue for main country
        "#10b981",  # Emerald green
        "#f59e0b",  # Amber
        "#ef4444",  # Red
        "#8b5cf6"   # Purple
      )
      
      # Create base plot
      p <- plot_ly()
      
      # Add trend analysis and formatting for each country
      for (i in seq_along(all_countries)) {
        country <- all_countries[i]
        country_data <- time_series_data %>% 
          filter(country_name == country) %>%
          arrange(year)
        
        if (nrow(country_data) > 0) {
          # Calculate trend indicators
          if (nrow(country_data) >= 2) {
            # Calculate overall trend (linear regression slope)
            trend_model <- lm(value ~ year, data = country_data)
            trend_slope <- coef(trend_model)[2]
            
            # Determine trend direction
            trend_direction <- case_when(
              trend_slope > 0 ~ "↗️ Increasing",
              trend_slope < 0 ~ "↘️ Decreasing", 
              TRUE ~ "➡️ Stable"
            )
            
            # Calculate average growth rate
            avg_growth <- mean(country_data$yoy_pct_change, na.rm = TRUE)
            avg_growth_text <- if (!is.na(avg_growth)) {
              paste0("Avg Growth: ", 
                    ifelse(avg_growth >= 0, "+", ""), 
                    format(avg_growth, digits = 2, nsmall = 1), "%")
            } else {
              "Avg Growth: N/A"
            }
          } else {
            trend_direction <- "➡️ Insufficient data"
            avg_growth_text <- "Avg Growth: N/A"
          }
          
          # Determine line style based on country importance
          is_primary <- country == input$primary_country
          line_width <- if (is_primary) 4 else 3
          marker_size <- if (is_primary) 8 else 6
          
          # Enhanced hover template with trend analysis
          hover_template <- paste0(
            "<b>🌍 ", country, "</b><br>",
            "━━━━━━━━━━━━━━━━━━━━<br>",
            "📅 Year: %{x}<br>",
            case_when(
              input$metric == "gdp" ~ paste0("💰 GDP: $%{y:,.0f}<br>"),
              input$metric == "cpi" ~ paste0("📊 CPI: %{y:,.2f}<br>"),
              TRUE ~ paste0("📈 Value: %{y:,.2f}<br>")
            ),
            "📈 YoY Change: ", 
            ifelse(is.na(country_data$yoy_pct_change[match(country_data$year, country_data$year)]), 
                   "N/A", 
                   paste0("%{customdata:+.1f}%")), "<br>",
            "🎯 Trend: ", trend_direction, "<br>",
            "📊 ", avg_growth_text,
            "<extra></extra>"
          )
          
          # Add trace with enhanced styling
          p <- p %>%
            add_trace(
              data = country_data,
              x = ~year,
              y = ~value,
              customdata = ~yoy_pct_change,
              type = "scatter",
              mode = "lines+markers",
              name = country,
              line = list(
                color = country_colors[i], 
                width = line_width,
                shape = "spline",  # Smooth curves
                smoothing = 0.3
              ),
              marker = list(
                color = country_colors[i], 
                size = marker_size,
                line = list(color = "#ffffff", width = 1),
                symbol = if (is_primary) "circle" else "circle-open"
              ),
              hovertemplate = hover_template,
              connectgaps = FALSE,  # Don't connect gaps in data
              # Add trend line if enough data points
              showlegend = TRUE
            )
          
          # Add trend line for primary country if enough data
          if (is_primary && nrow(country_data) >= 3) {
            # Calculate trend line
            trend_line <- predict(lm(value ~ year, data = country_data))
            
            p <- p %>%
              add_trace(
                data = country_data,
                x = ~year,
                y = trend_line,
                type = "scatter",
                mode = "lines",
                name = paste0(country, " (Trend)"),
                line = list(
                  color = country_colors[i],
                  width = 2,
                  dash = "dash"
                ),
                hovertemplate = paste0(
                  "<b>📈 ", country, " Trend Line</b><br>",
                  "Year: %{x}<br>",
                  "Trend Value: %{y:,.2f}<br>",
                  "<extra></extra>"
                ),
                showlegend = FALSE,
                opacity = 0.6
              )
          }
        }
      }
      
      # Enhanced layout with professional styling
      metric_info <- case_when(
        input$metric == "gdp" ~ list(
          title = "GDP (Current US$)",
          format = ",.0f",
          prefix = "$",
          suffix = ""
        ),
        input$metric == "cpi" ~ list(
          title = "Consumer Price Index",
          format = ",.2f", 
          prefix = "",
          suffix = ""
        ),
        TRUE ~ list(
          title = "Economic Indicator",
          format = ",.2f",
          prefix = "",
          suffix = ""
        )
      )
      
      # Calculate y-axis range for better visualization
      y_values <- time_series_data$value[!is.na(time_series_data$value)]
      if (length(y_values) > 0) {
        y_range <- range(y_values)
        y_padding <- (y_range[2] - y_range[1]) * 0.1
        y_axis_range <- c(max(0, y_range[1] - y_padding), y_range[2] + y_padding)
      } else {
        y_axis_range <- NULL
      }
      
      p %>%
        layout(
          title = list(
            text = paste0("📈 Multi-Country Economic Trend Analysis: ", metric_info$title, 
                         " (", input$year_range[1], "-", input$year_range[2], ")"),
            font = list(size = 16, color = "#1e293b", family = "Arial, sans-serif"),
            x = 0.02,
            y = 0.95
          ),
          xaxis = list(
            title = list(
              text = "📅 Year",
              font = list(size = 12, color = "#374151")
            ),
            gridcolor = "#e2e8f0",
            showgrid = TRUE,
            gridwidth = 1,
            tickfont = list(size = 10, color = "#64748b"),
            range = input$year_range,
            dtick = max(1, floor((input$year_range[2] - input$year_range[1]) / 10))
          ),
          yaxis = list(
            title = list(
              text = paste0("📊 ", metric_info$title),
              font = list(size = 12, color = "#374151")
            ),
            gridcolor = "#e2e8f0",
            showgrid = TRUE,
            gridwidth = 1,
            tickfont = list(size = 10, color = "#64748b"),
            tickformat = metric_info$format,
            range = y_axis_range,
            # Add thousand separators for GDP
            tickmode = if (input$metric == "gdp") "linear" else "auto"
          ),
          plot_bgcolor = "#f8fafc",
          paper_bgcolor = "#ffffff",
          # Enhanced legend with better positioning
          legend = list(
            orientation = "v",
            x = 1.02,
            y = 1,
            bgcolor = "rgba(255,255,255,0.9)",
            bordercolor = "#e2e8f0",
            borderwidth = 1,
            font = list(size = 11, color = "#374151"),
            itemsizing = "constant",
            itemwidth = 30
          ),
          # Add professional margins
          margin = list(l = 80, r = 120, t = 80, b = 60),
          # Smooth transitions
          transition = list(
            duration = 500,
            easing = "cubic-in-out"
          ),
          # Add annotations for insights
          annotations = list(
            list(
              text = paste0("💡 Showing ", length(all_countries), " countr", 
                           ifelse(length(all_countries) == 1, "y", "ies"), 
                           " over ", input$year_range[2] - input$year_range[1] + 1, " years"),
              x = 0.02, y = 0.02,
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 10, color = "#64748b", style = "italic"),
              bgcolor = "rgba(255,255,255,0.8)",
              bordercolor = "#e2e8f0",
              borderwidth = 1
            )
          )
        ) %>%
        # Add professional configuration
        config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d"),
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "png",
            filename = paste0("economic_trends_", tolower(gsub(" ", "_", metric_info$title))),
            height = 500,
            width = 900,
            scale = 2
          ),
          # Enable smooth zooming and panning
          scrollZoom = TRUE,
          doubleClick = "reset+autosize"
        )
        
    }, error = function(e) {
      plot_ly() %>%
        add_annotations(
          text = paste("❌ Error loading time series visualization:", e$message),
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 14, color = "#dc2626"),
          bgcolor = "#fef2f2",
          bordercolor = "#fecaca",
          borderwidth = 2
        ) %>%
        layout(
          plot_bgcolor = "#f8fafc",
          paper_bgcolor = "#ffffff"
        )
    })
  })
  
  # Professional Rankings Table Output with Advanced Features
  output$rankings_table <- DT::renderDataTable({
    req(rankings_data())
    
    tryCatch({
      # Enhanced rankings data with professional formatting
      table_data <- rankings_data() %>%
        mutate(
          # Add trend arrows and color coding for YoY changes
          trend_arrow = case_when(
            is.na(yoy_pct_change) ~ "➖",
            yoy_pct_change > 5 ~ "🔥",      # Strong growth
            yoy_pct_change > 0 ~ "📈",      # Positive growth
            yoy_pct_change > -5 ~ "📉",     # Mild decline
            TRUE ~ "⚠️"                     # Strong decline
          ),
          # Enhanced YoY change with color coding
          yoy_display = case_when(
            is.na(yoy_pct_change) ~ '<span style="color: #64748b;">--</span>',
            yoy_pct_change > 5 ~ paste0('<span style="color: #059669; font-weight: 600;">',
                                       trend_arrow, ' +', 
                                       format(yoy_pct_change, digits = 2, nsmall = 1), '%</span>'),
            yoy_pct_change > 0 ~ paste0('<span style="color: #059669;">',
                                       trend_arrow, ' +', 
                                       format(yoy_pct_change, digits = 2, nsmall = 1), '%</span>'),
            yoy_pct_change > -5 ~ paste0('<span style="color: #dc2626;">',
                                        trend_arrow, ' ', 
                                        format(yoy_pct_change, digits = 2, nsmall = 1), '%</span>'),
            TRUE ~ paste0('<span style="color: #dc2626; font-weight: 600;">',
                         trend_arrow, ' ', 
                         format(yoy_pct_change, digits = 2, nsmall = 1), '%</span>')
          ),
          # Enhanced country names with flags (using emoji flags for major countries)
          country_display = case_when(
            country_name == "United States" ~ '<span style="color: #000000; font-weight: 500;">🇺🇸 United States</span>',
            country_name == "China" ~ '<span style="color: #000000; font-weight: 500;">🇨🇳 China</span>', 
            country_name == "Japan" ~ '<span style="color: #000000; font-weight: 500;">🇯🇵 Japan</span>',
            country_name == "Germany" ~ '<span style="color: #000000; font-weight: 500;">🇩🇪 Germany</span>',
            country_name == "India" ~ '<span style="color: #000000; font-weight: 500;">🇮🇳 India</span>',
            country_name == "United Kingdom" ~ '<span style="color: #000000; font-weight: 500;">🇬🇧 United Kingdom</span>',
            country_name == "France" ~ '<span style="color: #000000; font-weight: 500;">🇫🇷 France</span>',
            country_name == "Italy" ~ '<span style="color: #000000; font-weight: 500;">🇮🇹 Italy</span>',
            country_name == "Brazil" ~ '<span style="color: #000000; font-weight: 500;">🇧🇷 Brazil</span>',
            country_name == "Canada" ~ '<span style="color: #000000; font-weight: 500;">🇨🇦 Canada</span>',
            country_name == "Russian Federation" ~ '<span style="color: #000000; font-weight: 500;">🇷🇺 Russian Federation</span>',
            country_name == "Korea, Rep." ~ '<span style="color: #000000; font-weight: 500;">🇰🇷 South Korea</span>',
            country_name == "Australia" ~ '<span style="color: #000000; font-weight: 500;">🇦🇺 Australia</span>',
            country_name == "Spain" ~ '<span style="color: #000000; font-weight: 500;">🇪🇸 Spain</span>',
            country_name == "Mexico" ~ '<span style="color: #000000; font-weight: 500;">🇲🇽 Mexico</span>',
            country_name == "Indonesia" ~ '<span style="color: #000000; font-weight: 500;">🇮🇩 Indonesia</span>',
            country_name == "Netherlands" ~ '<span style="color: #000000; font-weight: 500;">🇳🇱 Netherlands</span>',
            country_name == "Saudi Arabia" ~ '<span style="color: #000000; font-weight: 500;">🇸🇦 Saudi Arabia</span>',
            country_name == "Turkey" ~ '<span style="color: #000000; font-weight: 500;">🇹🇷 Turkey</span>',
            country_name == "Taiwan, China" ~ '<span style="color: #000000; font-weight: 500;">🇹🇼 Taiwan</span>',
            TRUE ~ paste0('<span style="color: #000000; font-weight: 500;">🌍 ', country_name, '</span>')
          ),
          # Enhanced value formatting with better visual hierarchy
          value_display = case_when(
            input$metric == "gdp" ~ paste0('<span style="font-weight: 600; color: #000000;">$',
                                          format(value / 1e12, digits = 2, nsmall = 1), 
                                          '</span><span style="color: #000000; font-size: 0.9em;">T</span>'),
            input$metric == "cpi" ~ paste0('<span style="font-weight: 600; color: #000000;">',
                                          format(value, digits = 4, nsmall = 1), '</span>'),
            TRUE ~ paste0('<span style="font-weight: 600; color: #000000;">',
                         format(value, digits = 3, nsmall = 1), '</span>')
          ),
          # Rank with medal icons for top 3
          rank_display = case_when(
            rank == 1 ~ '<span style="color: #000000; font-weight: 600;">🥇 1st</span>',
            rank == 2 ~ '<span style="color: #000000; font-weight: 600;">🥈 2nd</span>', 
            rank == 3 ~ '<span style="color: #000000; font-weight: 600;">🥉 3rd</span>',
            TRUE ~ paste0('<span style="color: #000000; font-weight: 500;">', rank, 
                         case_when(
                           rank %% 10 == 1 & rank != 11 ~ "st",
                           rank %% 10 == 2 & rank != 12 ~ "nd",
                           rank %% 10 == 3 & rank != 13 ~ "rd",
                           TRUE ~ "th"
                         ), '</span>')
          ),
          # Enhanced trend indicator with context
          trend_display = case_when(
            trend_indicator == "increasing" ~ '<span style="color: #059669;">📈 Rising</span>',
            trend_indicator == "decreasing" ~ '<span style="color: #dc2626;">📉 Falling</span>',
            trend_indicator == "stable" ~ '<span style="color: #64748b;">➡️ Stable</span>',
            TRUE ~ '<span style="color: #64748b;">❓ Unknown</span>'
          )
        ) %>%
        select(
          Rank = rank_display,
          Country = country_display,
          Value = value_display,
          "YoY Change" = yoy_display,
          Trend = trend_display
        )
      
      # Return the enhanced table data
      table_data
      
    }, error = function(e) {
      # Enhanced error handling with better formatting
      data.frame(
        Rank = "❌",
        Country = "Error loading data",
        Value = "--",
        "YoY Change" = "--",
        Trend = "--",
        stringsAsFactors = FALSE
      )
    })
  }, 
  # Enhanced DT options for professional appearance and functionality
  options = list(
    # Core table settings
    pageLength = 10,
    lengthChange = FALSE,
    searching = TRUE,
    searchHighlight = TRUE,
    info = TRUE,
    paging = FALSE,  # Show all 10 rows at once
    ordering = TRUE,
    order = list(list(0, 'asc')),  # Default sort by rank
    
    # Enhanced DOM layout with search and info
    dom = 'ft<"bottom"i>',
    
    # Professional styling and responsive design
    scrollX = FALSE,
    scrollY = "280px",
    scrollCollapse = TRUE,
    autoWidth = FALSE,
    
    # Enhanced column definitions with professional styling
    columnDefs = list(
      # Rank column - center aligned, narrow width
      list(
        className = 'dt-center dt-head-center',
        targets = 0,
        width = '60px',
        orderable = TRUE
      ),
      # Country column - left aligned, wider for flags and names
      list(
        className = 'dt-left dt-head-center',
        targets = 1,
        width = '140px',
        orderable = TRUE
      ),
      # Value column - right aligned for numbers
      list(
        className = 'dt-right dt-head-center',
        targets = 2,
        width = '80px',
        orderable = TRUE,
        type = 'num'
      ),
      # YoY Change column - center aligned with color coding
      list(
        className = 'dt-center dt-head-center',
        targets = 3,
        width = '90px',
        orderable = TRUE,
        type = 'num'
      ),
      # Trend column - center aligned
      list(
        className = 'dt-center dt-head-center',
        targets = 4,
        width = '80px',
        orderable = TRUE
      )
    ),
    
    # Enhanced search functionality
    search = list(
      regex = FALSE,
      caseInsensitive = TRUE,
      search = ""
    ),
    
    # Professional language settings
    language = list(
      search = "🔍 Search countries:",
      searchPlaceholder = "Type country name...",
      info = "Showing top _TOTAL_ countries",
      infoEmpty = "No countries to display",
      infoFiltered = "(filtered from _MAX_ total)",
      zeroRecords = "No matching countries found",
      emptyTable = "No ranking data available"
    ),
    
    # Enhanced initialization with smooth loading and black text
    initComplete = JS(
      "function(settings, json) {",
      "  $(this.api().table().header()).css({",
      "    'background-color': '#f8fafc',",
      "    'color': '#000000',",
      "    'font-weight': '600',",
      "    'font-size': '12px',",
      "    'text-transform': 'uppercase',",
      "    'letter-spacing': '0.5px',",
      "    'border-bottom': '2px solid #e2e8f0'",
      "  });",
      "  $(this.api().table().body()).css({",
      "    'color': '#000000',",
      "    'font-size': '13px'",
      "  });",
      "  $(this.api().table().container()).find('td').css({",
      "    'color': '#000000 !important',",
      "    'font-size': '13px'",
      "  });",
      "  $(this.api().table().container()).find('.dataTables_filter input').css({",
      "    'border': '1px solid #d1d5db',",
      "    'border-radius': '6px',",
      "    'padding': '6px 12px',",
      "    'font-size': '14px',",
      "    'margin-left': '8px',",
      "    'color': '#000000'",
      "  });",
      "  $(this.api().table().container()).find('.dataTables_info').css({",
      "    'color': '#000000',",
      "    'font-size': '12px',",
      "    'margin-top': '8px'",
      "  });",
      "}"
    )
  ), 
  # Additional DT settings
  rownames = FALSE,
  escape = FALSE,  # Allow HTML formatting in cells
  selection = 'single',  # Allow single row selection
  class = 'cell-border stripe hover',  # Professional table styling
  style = 'bootstrap4',  # Use Bootstrap 4 styling
  fillContainer = TRUE,
  height = "330px"
  )
  
  # Reactive values for country comparison management
  comparison_countries <- reactiveVal(c("China", "Germany", "Japan"))
  
  # Primary country display
  output$primary_country_display <- renderText({
    paste0("🌍 ", input$primary_country)
  })
  
  # Update available countries for adding
  observe({
    available_countries <- countries[!countries %in% c(input$primary_country, comparison_countries())]
    
    updateSelectInput(
      session,
      "country_to_add",
      choices = c("Select a country..." = "", available_countries),
      selected = ""
    )
  })
  
  # Add country button logic
  observeEvent(input$add_country_btn, {
    req(input$country_to_add, input$country_to_add != "")
    
    current_countries <- comparison_countries()
    
    if (length(current_countries) >= 5) {
      showNotification(
        "⚠️ Maximum 5 countries can be compared at once",
        type = "warning",
        duration = 3
      )
      return()
    }
    
    if (!input$country_to_add %in% current_countries) {
      new_countries <- c(current_countries, input$country_to_add)
      comparison_countries(new_countries)
      
      showNotification(
        paste0("✅ Added ", input$country_to_add, " to comparison"),
        type = "message",
        duration = 2
      )
      
      # Reset selection
      updateSelectInput(session, "country_to_add", selected = "")
    }
  })
  
  # Remove country function
  remove_country <- function(country_to_remove) {
    current_countries <- comparison_countries()
    new_countries <- current_countries[current_countries != country_to_remove]
    comparison_countries(new_countries)
    
    showNotification(
      paste0("🗑️ Removed ", country_to_remove, " from comparison"),
      type = "message",
      duration = 2
    )
  }
  
  # Clear all countries
  observeEvent(input$clear_all_btn, {
    comparison_countries(character(0))
    showNotification(
      "🗑️ Cleared all comparison countries",
      type = "message",
      duration = 2
    )
  })
  
  # Dynamic UI for selected countries with remove buttons
  output$selected_countries_ui <- renderUI({
    countries_list <- comparison_countries()
    
    if (length(countries_list) == 0) {
      return(
        div(
          style = "text-align: center; color: #64748b; font-style: italic; padding: 20px;",
          "No countries selected for comparison",
          br(),
          "Use the dropdown above to add countries"
        )
      )
    }
    
    # Create list of countries with remove buttons
    country_items <- lapply(seq_along(countries_list), function(i) {
      country <- countries_list[i]
      country_id <- paste0("remove_", gsub("[^A-Za-z0-9]", "_", country))
      
      div(
        style = "display: flex; justify-content: space-between; align-items: center; 
                 padding: 8px 12px; margin-bottom: 5px; 
                 background: #f1f5f9; border: 1px solid #e2e8f0; border-radius: 6px;",
        
        # Country name with flag
        span(
          style = "color: #1e293b; font-weight: 500;",
          case_when(
            country == "China" ~ "🇨🇳 China",
            country == "Germany" ~ "🇩🇪 Germany", 
            country == "Japan" ~ "🇯🇵 Japan",
            country == "United Kingdom" ~ "🇬🇧 United Kingdom",
            country == "France" ~ "🇫🇷 France",
            country == "India" ~ "🇮🇳 India",
            country == "Italy" ~ "🇮🇹 Italy",
            country == "Brazil" ~ "🇧🇷 Brazil",
            country == "Canada" ~ "🇨🇦 Canada",
            country == "Russian Federation" ~ "🇷🇺 Russian Federation",
            country == "Korea, Rep." ~ "🇰🇷 South Korea",
            country == "Australia" ~ "🇦🇺 Australia",
            country == "Spain" ~ "🇪🇸 Spain",
            country == "Mexico" ~ "🇲🇽 Mexico",
            country == "Indonesia" ~ "🇮🇩 Indonesia",
            country == "Netherlands" ~ "🇳🇱 Netherlands",
            country == "Saudi Arabia" ~ "🇸🇦 Saudi Arabia",
            country == "Turkey" ~ "🇹🇷 Turkey",
            TRUE ~ paste0("🌍 ", country)
          )
        ),
        
        # Remove button
        actionButton(
          country_id,
          "❌",
          onclick = paste0("Shiny.setInputValue('remove_country', '", country, "', {priority: 'event'});"),
          class = "btn-sm",
          style = "background: #dc2626; color: white; border: none; border-radius: 4px; 
                   padding: 2px 6px; font-size: 12px; cursor: pointer;"
        )
      )
    })
    
    do.call(tagList, country_items)
  })
  
  # Handle remove country events
  observeEvent(input$remove_country, {
    req(input$remove_country)
    remove_country(input$remove_country)
  })
  
  # Enhanced Country Comparison Chart Output with Professional Multi-Country Bar Charts
  output$comparison_chart <- renderPlotly({
    req(comparison_countries())
    
    tryCatch({
      # Get all countries for comparison (primary + selected)
      all_comparison_countries <- c(input$primary_country, comparison_countries())
      
      if (length(all_comparison_countries) == 1) {
        return(
          plot_ly() %>%
            add_annotations(
              text = "🎯 Add countries using the selection panel to see comparison",
              x = 0.5, y = 0.5,
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 16, color = "#64748b"),
              bgcolor = "#f8fafc",
              bordercolor = "#e2e8f0",
              borderwidth = 1
            ) %>%
            layout(
              plot_bgcolor = "#f8fafc",
              paper_bgcolor = "#ffffff"
            )
        )
      }
      
      # Get latest year data for comparison
      latest_year <- max(filtered_data()$year, na.rm = TRUE)
      
      comparison_data <- filtered_data() %>%
        filter(
          country_name %in% all_comparison_countries,
          year == latest_year,
          !is.na(value)
        ) %>%
        arrange(desc(value)) %>%
        mutate(
          # Add country ranking
          rank = row_number(),
          # Determine if primary country
          is_primary = country_name == input$primary_country,
          # Enhanced country display names with flags
          country_display = case_when(
            country_name == "United States" ~ "🇺🇸 United States",
            country_name == "China" ~ "🇨🇳 China", 
            country_name == "Japan" ~ "🇯🇵 Japan",
            country_name == "Germany" ~ "🇩🇪 Germany",
            country_name == "India" ~ "🇮🇳 India",
            country_name == "United Kingdom" ~ "🇬🇧 United Kingdom",
            country_name == "France" ~ "🇫🇷 France",
            country_name == "Italy" ~ "🇮🇹 Italy",
            country_name == "Brazil" ~ "🇧🇷 Brazil",
            country_name == "Canada" ~ "🇨🇦 Canada",
            country_name == "Russian Federation" ~ "🇷🇺 Russian Federation",
            country_name == "Korea, Rep." ~ "🇰🇷 South Korea",
            country_name == "Australia" ~ "🇦🇺 Australia",
            country_name == "Spain" ~ "🇪🇸 Spain",
            country_name == "Mexico" ~ "🇲🇽 Mexico",
            country_name == "Indonesia" ~ "🇮🇩 Indonesia",
            country_name == "Netherlands" ~ "🇳🇱 Netherlands",
            country_name == "Saudi Arabia" ~ "🇸🇦 Saudi Arabia",
            country_name == "Turkey" ~ "🇹🇷 Turkey",
            TRUE ~ paste0("🌍 ", country_name)
          ),
          # Professional value formatting
          value_formatted = case_when(
            input$metric == "gdp" ~ paste0("$", format(value / 1e12, digits = 2, nsmall = 1), "T"),
            input$metric == "cpi" ~ format(value, digits = 4, nsmall = 1),
            TRUE ~ format(value, digits = 3, nsmall = 1)
          ),
          # YoY change formatting
          yoy_formatted = case_when(
            is.na(yoy_pct_change) ~ "N/A",
            yoy_pct_change > 0 ~ paste0("+", format(yoy_pct_change, digits = 2, nsmall = 1), "%"),
            TRUE ~ paste0(format(yoy_pct_change, digits = 2, nsmall = 1), "%")
          )
        )
      
      if (nrow(comparison_data) == 0) {
        return(
          plot_ly() %>%
            add_annotations(
              text = "📊 No data available for the selected countries",
              x = 0.5, y = 0.5,
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 14, color = "#64748b"),
              bgcolor = "#f8fafc",
              bordercolor = "#e2e8f0",
              borderwidth = 1
            ) %>%
            layout(
              plot_bgcolor = "#f8fafc",
              paper_bgcolor = "#ffffff"
            )
        )
      }
      
      # Professional color palette for comparison
      primary_color <- "#2563eb"  # Blue for primary country
      comparison_colors <- c("#10b981", "#f59e0b", "#ef4444", "#8b5cf6", "#06b6d4")  # Green, amber, red, purple, cyan
      
      # Assign colors based on country type
      comparison_data$bar_color <- ifelse(
        comparison_data$is_primary, 
        primary_color,
        comparison_colors[seq_len(sum(!comparison_data$is_primary))]
      )
      
      # Enhanced hover template with comprehensive information
      hover_template <- paste0(
        "<b>", comparison_data$country_display, "</b><br>",
        "━━━━━━━━━━━━━━━━━━━━━━━━━━<br>",
        "🏆 Rank: #", comparison_data$rank, " of ", nrow(comparison_data), "<br>",
        case_when(
          input$metric == "gdp" ~ paste0("💰 GDP: ", comparison_data$value_formatted, "<br>"),
          input$metric == "cpi" ~ paste0("📊 CPI: ", comparison_data$value_formatted, "<br>"),
          TRUE ~ paste0("📈 Value: ", comparison_data$value_formatted, "<br>")
        ),
        "📈 YoY Change: ", comparison_data$yoy_formatted, "<br>",
        "📅 Year: ", latest_year, "<br>",
        ifelse(comparison_data$is_primary, "🎯 Primary Country", "🔄 Comparison Country"),
        "<extra></extra>"
      )
      
      # Create professional multi-country bar chart
      p <- plot_ly(
        data = comparison_data,
        x = ~reorder(country_display, value),
        y = ~value,
        type = "bar",
        marker = list(
          color = ~bar_color,
          line = list(
            color = "#ffffff", 
            width = 2
          ),
          # Add gradient effect for primary country
          pattern = list(
            shape = ifelse(comparison_data$is_primary, ".", ""),
            size = 4,
            solidity = 0.3
          )
        ),
        text = ~value_formatted,
        textposition = "outside",
        textfont = list(
          size = 11,
          color = "#1e293b",
          family = "Arial, sans-serif"
        ),
        hovertemplate = hover_template,
        name = ""
      ) %>%
        layout(
          title = list(
            text = paste0("🏆 Multi-Country Comparison: ", 
                         case_when(
                           input$metric == "gdp" ~ "GDP Analysis",
                           input$metric == "cpi" ~ "Consumer Price Index",
                           TRUE ~ "Economic Indicator"
                         ), " (", latest_year, ")"),
            font = list(size = 18, color = "#1e293b", family = "Arial, sans-serif"),
            x = 0.02,
            y = 0.95
          ),
          xaxis = list(
            title = list(
              text = "🌍 Countries (Ranked by Performance)",
              font = list(size = 12, color = "#374151")
            ),
            tickangle = -45,
            tickfont = list(size = 10, color = "#64748b"),
            gridcolor = "#e2e8f0",
            showgrid = FALSE,
            categoryorder = "total ascending"
          ),
          yaxis = list(
            title = list(
              text = case_when(
                input$metric == "gdp" ~ "💰 GDP (Trillions USD)",
                input$metric == "cpi" ~ "📊 Consumer Price Index",
                TRUE ~ "📈 Economic Value"
              ),
              font = list(size = 12, color = "#374151")
            ),
            tickfont = list(size = 10, color = "#64748b"),
            gridcolor = "#e2e8f0",
            showgrid = TRUE,
            gridwidth = 1,
            tickformat = case_when(
              input$metric == "gdp" ~ ",.1f",
              input$metric == "cpi" ~ ",.2f",
              TRUE ~ ",.2f"
            ),
            # Add range padding for better visualization
            range = c(0, max(comparison_data$value, na.rm = TRUE) * 1.15)
          ),
          plot_bgcolor = "#f8fafc",
          paper_bgcolor = "#ffffff",
          showlegend = FALSE,
          # Professional margins
          margin = list(l = 80, r = 40, t = 100, b = 120),
          # Add annotations for insights
          annotations = list(
            list(
              text = paste0("💡 ", input$primary_country, " highlighted as primary country • ",
                           "Comparing ", length(all_comparison_countries), " countries total"),
              x = 0.02, y = 0.02,
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 11, color = "#64748b", style = "italic"),
              bgcolor = "rgba(255,255,255,0.9)",
              bordercolor = "#e2e8f0",
              borderwidth = 1
            ),
            # Add ranking annotations for top 3
            if (nrow(comparison_data) >= 1) {
              list(
                text = "🥇",
                x = comparison_data$country_display[1],
                y = comparison_data$value[1],
                xref = "x", yref = "y",
                showarrow = FALSE,
                font = list(size = 20),
                yshift = 20
              )
            },
            if (nrow(comparison_data) >= 2) {
              list(
                text = "🥈",
                x = comparison_data$country_display[2],
                y = comparison_data$value[2],
                xref = "x", yref = "y",
                showarrow = FALSE,
                font = list(size = 18),
                yshift = 20
              )
            },
            if (nrow(comparison_data) >= 3) {
              list(
                text = "🥉",
                x = comparison_data$country_display[3],
                y = comparison_data$value[3],
                xref = "x", yref = "y",
                showarrow = FALSE,
                font = list(size = 16),
                yshift = 20
              )
            }
          )
        ) %>%
        # Professional configuration
        config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d"),
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "png",
            filename = paste0("country_comparison_", tolower(gsub(" ", "_", input$metric))),
            height = 500,
            width = 900,
            scale = 2
          )
        )
      
      # Add trend indicators if YoY data is available
      if (any(!is.na(comparison_data$yoy_pct_change))) {
        # Add small trend arrows above bars
        for (i in seq_len(nrow(comparison_data))) {
          if (!is.na(comparison_data$yoy_pct_change[i])) {
            trend_symbol <- case_when(
              comparison_data$yoy_pct_change[i] > 2 ~ "📈",
              comparison_data$yoy_pct_change[i] < -2 ~ "📉",
              TRUE ~ "➡️"
            )
            
            p <- p %>%
              add_annotations(
                text = trend_symbol,
                x = comparison_data$country_display[i],
                y = comparison_data$value[i],
                xref = "x", yref = "y",
                showarrow = FALSE,
                font = list(size = 12),
                yshift = 35
              )
          }
        }
      }
      
      return(p)
      
    }, error = function(e) {
      plot_ly() %>%
        add_annotations(
          text = paste("❌ Error loading comparison chart:", e$message),
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 14, color = "#dc2626"),
          bgcolor = "#fef2f2",
          bordercolor = "#fecaca",
          borderwidth = 2
        ) %>%
        layout(
          plot_bgcolor = "#f8fafc",
          paper_bgcolor = "#ffffff"
        )
    })
  })
  
  # Enhanced Comparison Metrics Table Output
  output$comparison_table <- DT::renderDataTable({
    req(comparison_countries())
    
    tryCatch({
      # Get all countries for comparison
      all_comparison_countries <- c(input$primary_country, comparison_countries())
      
      if (length(all_comparison_countries) == 1) {
        return(data.frame(
          Country = "Add countries to see comparison",
          Current = "--",
          Previous = "--",
          "YoY Change" = "--",
          Trend = "--",
          stringsAsFactors = FALSE
        ))
      }
      
      latest_year <- max(filtered_data()$year, na.rm = TRUE)
      previous_year <- latest_year - 1
      
      # Get current year data
      current_data <- filtered_data() %>%
        filter(
          country_name %in% all_comparison_countries,
          year == latest_year,
          !is.na(value)
        ) %>%
        select(country_name, current_value = value, current_yoy = yoy_pct_change)
      
      # Get previous year data
      previous_data <- filtered_data() %>%
        filter(
          country_name %in% all_comparison_countries,
          year == previous_year,
          !is.na(value)
        ) %>%
        select(country_name, previous_value = value)
      
      # Create enhanced comparison metrics
      comparison_metrics <- current_data %>%
        left_join(previous_data, by = "country_name") %>%
        arrange(desc(current_value)) %>%
        mutate(
          # Calculate absolute difference
          absolute_difference = current_value - previous_value,
          
          # Calculate percentage change (using YoY if available, otherwise calculate)
          pct_change = ifelse(!is.na(current_yoy), current_yoy, 
                             ifelse(!is.na(previous_value) & previous_value != 0,
                                   (absolute_difference / previous_value) * 100, NA)),
          
          # Enhanced country display with flags and primary indicator
          country_display = case_when(
            country_name == input$primary_country ~ paste0("🎯 ", 
              case_when(
                country_name == "United States" ~ "🇺🇸 United States",
                country_name == "China" ~ "🇨🇳 China", 
                country_name == "Japan" ~ "🇯🇵 Japan",
                country_name == "Germany" ~ "🇩🇪 Germany",
                country_name == "India" ~ "🇮🇳 India",
                country_name == "United Kingdom" ~ "🇬🇧 United Kingdom",
                country_name == "France" ~ "🇫🇷 France",
                country_name == "Italy" ~ "🇮🇹 Italy",
                country_name == "Brazil" ~ "🇧🇷 Brazil",
                country_name == "Canada" ~ "🇨🇦 Canada",
                country_name == "Russian Federation" ~ "🇷🇺 Russian Federation",
                country_name == "Korea, Rep." ~ "🇰🇷 South Korea",
                country_name == "Australia" ~ "🇦🇺 Australia",
                country_name == "Spain" ~ "🇪🇸 Spain",
                country_name == "Mexico" ~ "🇲🇽 Mexico",
                country_name == "Indonesia" ~ "🇮🇩 Indonesia",
                country_name == "Netherlands" ~ "🇳🇱 Netherlands",
                country_name == "Saudi Arabia" ~ "🇸🇦 Saudi Arabia",
                country_name == "Turkey" ~ "🇹🇷 Turkey",
                TRUE ~ country_name
              ), " (Primary)"),
            TRUE ~ case_when(
              country_name == "China" ~ "🇨🇳 China",
              country_name == "Japan" ~ "🇯🇵 Japan",
              country_name == "Germany" ~ "🇩🇪 Germany",
              country_name == "India" ~ "🇮🇳 India",
              country_name == "United Kingdom" ~ "🇬🇧 United Kingdom",
              country_name == "France" ~ "🇫🇷 France",
              country_name == "Italy" ~ "🇮🇹 Italy",
              country_name == "Brazil" ~ "🇧🇷 Brazil",
              country_name == "Canada" ~ "🇨🇦 Canada",
              country_name == "Russian Federation" ~ "🇷🇺 Russian Federation",
              country_name == "Korea, Rep." ~ "🇰🇷 South Korea",
              country_name == "Australia" ~ "🇦🇺 Australia",
              country_name == "Spain" ~ "🇪🇸 Spain",
              country_name == "Mexico" ~ "🇲🇽 Mexico",
              country_name == "Indonesia" ~ "🇮🇩 Indonesia",
              country_name == "Netherlands" ~ "🇳🇱 Netherlands",
              country_name == "Saudi Arabia" ~ "🇸🇦 Saudi Arabia",
              country_name == "Turkey" ~ "🇹🇷 Turkey",
              TRUE ~ paste0("🌍 ", country_name)
            )
          ),
          
          # Professional value formatting
          current_formatted = case_when(
            input$metric == "gdp" ~ paste0('<span style="font-weight: 600; color: #1e293b;">$',
                                          format(current_value / 1e12, digits = 2, nsmall = 1), 
                                          '</span><span style="color: #64748b; font-size: 0.9em;">T</span>'),
            input$metric == "cpi" ~ paste0('<span style="font-weight: 600; color: #1e293b;">',
                                          format(current_value, digits = 4, nsmall = 1), '</span>'),
            TRUE ~ paste0('<span style="font-weight: 600; color: #1e293b;">',
                         format(current_value, digits = 3, nsmall = 1), '</span>')
          ),
          
          previous_formatted = case_when(
            is.na(previous_value) ~ '<span style="color: #64748b;">--</span>',
            input$metric == "gdp" ~ paste0('<span style="color: #64748b;">$',
                                          format(previous_value / 1e12, digits = 2, nsmall = 1), 'T</span>'),
            input$metric == "cpi" ~ paste0('<span style="color: #64748b;">',
                                          format(previous_value, digits = 4, nsmall = 1), '</span>'),
            TRUE ~ paste0('<span style="color: #64748b;">',
                         format(previous_value, digits = 3, nsmall = 1), '</span>')
          ),
          
          # Enhanced YoY change with color coding and trend indicators
          yoy_change_formatted = case_when(
            is.na(pct_change) ~ '<span style="color: #64748b;">--</span>',
            pct_change > 5 ~ paste0('<span style="color: #059669; font-weight: 600;">🔥 +',
                                   format(pct_change, digits = 2, nsmall = 1), '%</span>'),
            pct_change > 0 ~ paste0('<span style="color: #059669;">📈 +',
                                   format(pct_change, digits = 2, nsmall = 1), '%</span>'),
            pct_change > -5 ~ paste0('<span style="color: #dc2626;">📉 ',
                                    format(pct_change, digits = 2, nsmall = 1), '%</span>'),
            TRUE ~ paste0('<span style="color: #dc2626; font-weight: 600;">⚠️ ',
                         format(pct_change, digits = 2, nsmall = 1), '%</span>')
          ),
          
          # Enhanced trend analysis
          trend_display = case_when(
            is.na(pct_change) ~ '<span style="color: #64748b;">❓ Unknown</span>',
            pct_change > 5 ~ '<span style="color: #059669; font-weight: 600;">🚀 Strong Growth</span>',
            pct_change > 0 ~ '<span style="color: #059669;">📈 Growing</span>',
            pct_change > -5 ~ '<span style="color: #dc2626;">📉 Declining</span>',
            TRUE ~ '<span style="color: #dc2626; font-weight: 600;">⚠️ Sharp Decline</span>'
          ),
          
          # Add ranking
          rank = row_number()
        ) %>%
        select(
          Rank = rank,
          Country = country_display,
          Current = current_formatted,
          Previous = previous_formatted,
          "YoY Change" = yoy_change_formatted,
          Trend = trend_display
        )
      
      comparison_metrics
      
    }, error = function(e) {
      data.frame(
        Rank = "❌",
        Country = "Error loading data",
        Current = "--",
        Previous = "--",
        "YoY Change" = "--",
        Trend = "--",
        stringsAsFactors = FALSE
      )
    })
  }, 
  options = list(
    pageLength = 10,
    lengthChange = FALSE,
    searching = FALSE,
    info = FALSE,
    paging = FALSE,
    ordering = TRUE,
    order = list(list(0, 'asc')),
    dom = 't',
    scrollX = FALSE,
    scrollY = "300px",
    scrollCollapse = TRUE,
    columnDefs = list(
      list(className = 'dt-center dt-head-center', targets = 0, width = '50px'),
      list(className = 'dt-left dt-head-center', targets = 1, width = '140px'),
      list(className = 'dt-right dt-head-center', targets = 2, width = '80px'),
      list(className = 'dt-right dt-head-center', targets = 3, width = '80px'),
      list(className = 'dt-center dt-head-center', targets = 4, width = '90px'),
      list(className = 'dt-center dt-head-center', targets = 5, width = '100px')
    ),
    language = list(
      emptyTable = "Add countries to see detailed comparison metrics"
    ),
    initComplete = JS(
      "function(settings, json) {",
      "  $(this.api().table().header()).css({",
      "    'background-color': '#f8fafc',",
      "    'color': '#1e293b',",
      "    'font-weight': '600',",
      "    'font-size': '11px',",
      "    'text-transform': 'uppercase',",
      "    'letter-spacing': '0.5px',",
      "    'border-bottom': '2px solid #e2e8f0'",
      "  });",
      "}"
    )
  ), 
  rownames = FALSE,
  escape = FALSE,
  selection = 'none',
  class = 'cell-border stripe hover',
  style = 'bootstrap4'
  )
  
  # Percentage Differences vs Primary Country Table Output
  output$percentage_differences_table <- DT::renderDataTable({
    req(comparison_countries())
    
    tryCatch({
      # Get all countries for comparison
      all_comparison_countries <- c(input$primary_country, comparison_countries())
      
      if (length(all_comparison_countries) == 1) {
        return(data.frame(
          Country = "Add countries to see differences",
          "Current Difference" = "--",
          "Percentage Difference" = "--",
          "Relative Performance" = "--",
          stringsAsFactors = FALSE
        ))
      }
      
      latest_year <- max(filtered_data()$year, na.rm = TRUE)
      
      # Get latest year data for all countries
      comparison_data <- filtered_data() %>%
        filter(
          country_name %in% all_comparison_countries,
          year == latest_year,
          !is.na(value)
        )
      
      # Get primary country value for comparison
      primary_value <- comparison_data %>%
        filter(country_name == input$primary_country) %>%
        pull(value)
      
      if (length(primary_value) == 0 || is.na(primary_value)) {
        return(data.frame(
          Country = "Primary country data not available",
          "Current Difference" = "--",
          "Percentage Difference" = "--",
          "Relative Performance" = "--",
          stringsAsFactors = FALSE
        ))
      }
      
      primary_value <- primary_value[1]  # Ensure single value
      
      # Calculate differences vs primary country
      differences_data <- comparison_data %>%
        filter(country_name != input$primary_country) %>%  # Exclude primary country
        mutate(
          # Calculate absolute difference
          absolute_diff = value - primary_value,
          
          # Calculate percentage difference
          pct_diff = ((value - primary_value) / primary_value) * 100,
          
          # Enhanced country display
          country_display = case_when(
            country_name == "China" ~ "🇨🇳 China",
            country_name == "Japan" ~ "🇯🇵 Japan",
            country_name == "Germany" ~ "🇩🇪 Germany",
            country_name == "India" ~ "🇮🇳 India",
            country_name == "United Kingdom" ~ "🇬🇧 United Kingdom",
            country_name == "France" ~ "🇫🇷 France",
            country_name == "Italy" ~ "🇮🇹 Italy",
            country_name == "Brazil" ~ "🇧🇷 Brazil",
            country_name == "Canada" ~ "🇨🇦 Canada",
            country_name == "Russian Federation" ~ "🇷🇺 Russian Federation",
            country_name == "Korea, Rep." ~ "🇰🇷 South Korea",
            country_name == "Australia" ~ "🇦🇺 Australia",
            country_name == "Spain" ~ "🇪🇸 Spain",
            country_name == "Mexico" ~ "🇲🇽 Mexico",
            country_name == "Indonesia" ~ "🇮🇩 Indonesia",
            country_name == "Netherlands" ~ "🇳🇱 Netherlands",
            country_name == "Saudi Arabia" ~ "🇸🇦 Saudi Arabia",
            country_name == "Turkey" ~ "🇹🇷 Turkey",
            TRUE ~ paste0("🌍 ", country_name)
          ),
          
          # Format absolute difference
          diff_formatted = case_when(
            input$metric == "gdp" ~ {
              if (absolute_diff >= 0) {
                paste0('<span style="color: #059669; font-weight: 600;">+$',
                      format(absolute_diff / 1e12, digits = 2, nsmall = 1), 'T</span>')
              } else {
                paste0('<span style="color: #dc2626; font-weight: 600;">-$',
                      format(abs(absolute_diff) / 1e12, digits = 2, nsmall = 1), 'T</span>')
              }
            },
            input$metric == "cpi" ~ {
              if (absolute_diff >= 0) {
                paste0('<span style="color: #059669; font-weight: 600;">+',
                      format(absolute_diff, digits = 3, nsmall = 1), '</span>')
              } else {
                paste0('<span style="color: #dc2626; font-weight: 600;">',
                      format(absolute_diff, digits = 3, nsmall = 1), '</span>')
              }
            },
            TRUE ~ {
              if (absolute_diff >= 0) {
                paste0('<span style="color: #059669; font-weight: 600;">+',
                      format(absolute_diff, digits = 2, nsmall = 1), '</span>')
              } else {
                paste0('<span style="color: #dc2626; font-weight: 600;">',
                      format(absolute_diff, digits = 2, nsmall = 1), '</span>')
              }
            }
          ),
          
          # Format percentage difference with enhanced styling
          pct_diff_formatted = case_when(
            pct_diff > 50 ~ paste0('<span style="color: #059669; font-weight: 600;">🚀 +',
                                  format(pct_diff, digits = 2, nsmall = 1), '%</span>'),
            pct_diff > 10 ~ paste0('<span style="color: #059669;">📈 +',
                                  format(pct_diff, digits = 2, nsmall = 1), '%</span>'),
            pct_diff > 0 ~ paste0('<span style="color: #059669;">↗️ +',
                                 format(pct_diff, digits = 2, nsmall = 1), '%</span>'),
            pct_diff > -10 ~ paste0('<span style="color: #dc2626;">↘️ ',
                                   format(pct_diff, digits = 2, nsmall = 1), '%</span>'),
            pct_diff > -50 ~ paste0('<span style="color: #dc2626;">📉 ',
                                   format(pct_diff, digits = 2, nsmall = 1), '%</span>'),
            TRUE ~ paste0('<span style="color: #dc2626; font-weight: 600;">⚠️ ',
                         format(pct_diff, digits = 2, nsmall = 1), '%</span>')
          ),
          
          # Relative performance assessment
          performance_assessment = case_when(
            pct_diff > 50 ~ '<span style="color: #059669; font-weight: 600;">🏆 Much Higher</span>',
            pct_diff > 10 ~ '<span style="color: #059669;">⬆️ Higher</span>',
            pct_diff > 0 ~ '<span style="color: #059669;">↗️ Slightly Higher</span>',
            pct_diff > -10 ~ '<span style="color: #dc2626;">↘️ Slightly Lower</span>',
            pct_diff > -50 ~ '<span style="color: #dc2626;">⬇️ Lower</span>',
            TRUE ~ '<span style="color: #dc2626; font-weight: 600;">📉 Much Lower</span>'
          )
        ) %>%
        arrange(desc(pct_diff)) %>%  # Sort by percentage difference (highest first)
        select(
          Country = country_display,
          "Absolute Difference" = diff_formatted,
          "Percentage Difference" = pct_diff_formatted,
          "Relative Performance" = performance_assessment
        )
      
      # Add header row showing primary country for reference
      if (nrow(differences_data) > 0) {
        primary_country_display <- case_when(
          input$primary_country == "United States" ~ "🇺🇸 United States",
          input$primary_country == "China" ~ "🇨🇳 China",
          input$primary_country == "Japan" ~ "🇯🇵 Japan",
          input$primary_country == "Germany" ~ "🇩🇪 Germany",
          input$primary_country == "India" ~ "🇮🇳 India",
          input$primary_country == "United Kingdom" ~ "🇬🇧 United Kingdom",
          input$primary_country == "France" ~ "🇫🇷 France",
          input$primary_country == "Italy" ~ "🇮🇹 Italy",
          input$primary_country == "Brazil" ~ "🇧🇷 Brazil",
          input$primary_country == "Canada" ~ "🇨🇦 Canada",
          input$primary_country == "Russian Federation" ~ "🇷🇺 Russian Federation",
          input$primary_country == "Korea, Rep." ~ "🇰🇷 South Korea",
          input$primary_country == "Australia" ~ "🇦🇺 Australia",
          input$primary_country == "Spain" ~ "🇪🇸 Spain",
          input$primary_country == "Mexico" ~ "🇲🇽 Mexico",
          input$primary_country == "Indonesia" ~ "🇮🇩 Indonesia",
          input$primary_country == "Netherlands" ~ "🇳🇱 Netherlands",
          input$primary_country == "Saudi Arabia" ~ "🇸🇦 Saudi Arabia",
          input$primary_country == "Turkey" ~ "🇹🇷 Turkey",
          TRUE ~ paste0("🌍 ", input$primary_country)
        )
        
        primary_value_formatted <- case_when(
          input$metric == "gdp" ~ paste0("$", format(primary_value / 1e12, digits = 2, nsmall = 1), "T"),
          input$metric == "cpi" ~ format(primary_value, digits = 4, nsmall = 1),
          TRUE ~ format(primary_value, digits = 3, nsmall = 1)
        )
        
        # Add reference row at the top
        reference_row <- data.frame(
          Country = paste0("🎯 ", primary_country_display, " (Reference)"),
          "Absolute Difference" = '<span style="color: #64748b; font-style: italic;">Baseline</span>',
          "Percentage Difference" = '<span style="color: #64748b; font-style: italic;">0.0%</span>',
          "Relative Performance" = paste0('<span style="color: #2563eb; font-weight: 600;">📍 ', primary_value_formatted, '</span>'),
          stringsAsFactors = FALSE
        )
        
        differences_data <- rbind(reference_row, differences_data)
      }
      
      differences_data
      
    }, error = function(e) {
      data.frame(
        Country = "Error loading data",
        "Absolute Difference" = "--",
        "Percentage Difference" = "--",
        "Relative Performance" = "--",
        stringsAsFactors = FALSE
      )
    })
  }, 
  options = list(
    pageLength = 10,
    lengthChange = FALSE,
    searching = FALSE,
    info = FALSE,
    paging = FALSE,
    ordering = FALSE,  # Keep the custom order (reference first, then by percentage)
    dom = 't',
    scrollX = FALSE,
    scrollY = "300px",
    scrollCollapse = TRUE,
    columnDefs = list(
      list(className = 'dt-left dt-head-center', targets = 0, width = '140px'),
      list(className = 'dt-right dt-head-center', targets = 1, width = '100px'),
      list(className = 'dt-center dt-head-center', targets = 2, width = '100px'),
      list(className = 'dt-center dt-head-center', targets = 3, width = '120px')
    ),
    language = list(
      emptyTable = "Add countries to see percentage differences vs primary country"
    ),
    initComplete = JS(
      "function(settings, json) {",
      "  $(this.api().table().header()).css({",
      "    'background-color': '#f8fafc',",
      "    'color': '#1e293b',",
      "    'font-weight': '600',",
      "    'font-size': '11px',",
      "    'text-transform': 'uppercase',",
      "    'letter-spacing': '0.5px',",
      "    'border-bottom': '2px solid #e2e8f0'",
      "  });",
      "  // Highlight reference row",
      "  $(this.api().table().container()).find('tbody tr:first').css({",
      "    'background-color': '#eff6ff',",
      "    'border': '1px solid #2563eb'",
      "  });",
      "}"
    )
  ), 
  rownames = FALSE,
  escape = FALSE,
  selection = 'none',
  class = 'cell-border stripe hover',
  style = 'bootstrap4'
  )
  
  # Year-over-Year Changes Table Output (keeping for backward compatibility)
  output$yoy_changes_table <- DT::renderDataTable({
    req(input$compare_countries)
    
    tryCatch({
      years_to_show <- tail(sort(unique(filtered_data()$year)), 3)
      
      yoy_data <- filtered_data() %>%
        filter(
          country_name %in% c(input$primary_country, input$compare_countries),
          year %in% years_to_show
        ) %>%
        select(country_name, year, yoy_pct_change) %>%
        pivot_wider(
          names_from = year,
          values_from = yoy_pct_change,
          names_prefix = "Y"
        ) %>%
        mutate(
          across(starts_with("Y"), ~ case_when(
            is.na(.x) ~ "--",
            .x > 0 ~ paste0("+", format(.x, digits = 2, nsmall = 1), "%"),
            TRUE ~ paste0(format(.x, digits = 2, nsmall = 1), "%")
          ))
        )
      
      # Rename columns to show actual years
      names(yoy_data) <- c("Country", as.character(years_to_show))
      
      yoy_data
    }, error = function(e) {
      data.frame(
        Country = "Error",
        "2023" = "--",
        "2022" = "--",
        "2021" = "--"
      )
    })
  }, options = list(
    pageLength = 10,
    searching = FALSE,
    info = FALSE,
    paging = FALSE,
    dom = 't'
  ), rownames = FALSE)
  
  # Input validation and reactive updates
  observe({
    # Update comparison countries when primary country changes
    current_countries <- comparison_countries()
    
    # Remove primary country from comparison if it's there
    if (input$primary_country %in% current_countries) {
      new_countries <- current_countries[current_countries != input$primary_country]
      comparison_countries(new_countries)
      
      showNotification(
        paste0("Removed ", input$primary_country, " from comparison (now primary country)"),
        type = "message",
        duration = 3
      )
    }
  })
  
  # Update legacy compare_countries input to maintain compatibility with existing time series logic
  observe({
    updateCheckboxGroupInput(
      session,
      "compare_countries",
      selected = comparison_countries()
    )
  })
  
  # Reactive validation for data availability
  observe({
    req(input$metric, input$year_range)
    
    available_data <- economic_data %>%
      filter(
        indicator_short == input$metric,
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
    
    if (nrow(available_data) == 0) {
      showNotification(
        "No data available for the selected criteria. Please adjust your selections.",
        type = "warning",
        duration = 5
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)