# Test script for multi-format report generation
# This script tests the Quarto report in HTML, PDF, and DOCX formats

library(quarto)

# Set working directory to quarto folder
setwd("GlobalConsumerPulse/quarto")

# Test parameters for different scenarios
test_params <- list(
  # Full report with default parameters
  default = list(
    start_year = 2010,
    end_year = 2023,
    focus_countries = c("United States", "China", "Germany", "Japan", "United Kingdom"),
    primary_indicator = "gdp",
    report_type = "full"
  ),
  
  # Recent years focus
  recent = list(
    start_year = 2018,
    end_year = 2023,
    focus_countries = c("United States", "China", "India", "Brazil", "Russia"),
    primary_indicator = "inflation",
    report_type = "recent_trends"
  ),
  
  # European focus
  europe = list(
    start_year = 2015,
    end_year = 2023,
    focus_countries = c("Germany", "France", "United Kingdom", "Italy", "Spain"),
    primary_indicator = "cpi",
    report_type = "regional"
  )
)

# Function to render report with parameters
render_report <- function(format, params, output_name) {
  cat("Rendering", format, "report with", output_name, "parameters...\n")
  
  tryCatch({
    quarto_render(
      input = "report.qmd",
      output_format = format,
      output_file = paste0("report_", output_name, "_", gsub(":", "", format)),
      execute_params = params
    )
    cat("✓ Successfully rendered", format, "report\n")
    return(TRUE)
  }, error = function(e) {
    cat("✗ Error rendering", format, "report:", e$message, "\n")
    return(FALSE)
  })
}

# Test HTML format (interactive)
cat("=== Testing HTML Format ===\n")
html_results <- list()
for (param_name in names(test_params)) {
  html_results[[param_name]] <- render_report("html", test_params[[param_name]], param_name)
}

# Test PDF format (static)
cat("\n=== Testing PDF Format ===\n")
pdf_results <- list()
for (param_name in names(test_params)) {
  pdf_results[[param_name]] <- render_report("pdf", test_params[[param_name]], param_name)
}

# Test DOCX format (static)
cat("\n=== Testing DOCX Format ===\n")
docx_results <- list()
for (param_name in names(test_params)) {
  docx_results[[param_name]] <- render_report("docx", test_params[[param_name]], param_name)
}

# Summary of results
cat("\n=== Test Results Summary ===\n")
cat("HTML Results:\n")
for (param_name in names(html_results)) {
  status <- if (html_results[[param_name]]) "✓ PASS" else "✗ FAIL"
  cat("  ", param_name, ":", status, "\n")
}

cat("PDF Results:\n")
for (param_name in names(pdf_results)) {
  status <- if (pdf_results[[param_name]]) "✓ PASS" else "✗ FAIL"
  cat("  ", param_name, ":", status, "\n")
}

cat("DOCX Results:\n")
for (param_name in names(docx_results)) {
  status <- if (docx_results[[param_name]]) "✓ PASS" else "✗ FAIL"
  cat("  ", param_name, ":", status, "\n")
}

# Overall success rate
total_tests <- length(test_params) * 3  # 3 formats
successful_tests <- sum(unlist(html_results)) + sum(unlist(pdf_results)) + sum(unlist(docx_results))
success_rate <- round((successful_tests / total_tests) * 100, 1)

cat("\nOverall Success Rate:", success_rate, "%\n")
cat("Successful tests:", successful_tests, "out of", total_tests, "\n")

if (success_rate >= 80) {
  cat("✓ Multi-format report generation is working well!\n")
} else {
  cat("⚠ Some issues detected with multi-format generation\n")
}