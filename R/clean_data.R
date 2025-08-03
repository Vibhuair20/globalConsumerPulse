# Global Consumer Pulse Dashboard - Data Processing
# Professional data cleaning and transformation functions for World Bank economic data

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

#' Read and parse World Bank CSV files
#' 
#' This function handles the standard World Bank CSV format which includes
#' metadata rows at the top that need to be skipped. It provides robust
#' error handling and logging for the data ingestion process.
#' 
#' @param file_path Character. Path to the World Bank CSV file
#' @param skip_rows Integer. Number of header rows to skip (default: 4)
#' 
#' @return A cleaned tibble with standardized column names
#' 
#' @details
#' World Bank CSV files typically have 4 metadata rows at the top:
#' - Row 1: Indicator name
#' - Row 2: Indicator code  
#' - Row 3: Empty
#' - Row 4: Column headers
#' 
#' The function automatically handles file existence checking and provides
#' informative error messages for debugging.
#' 
#' @examples
#' \dontrun{
#' # Read GDP data
#' gdp_data <- read_world_bank_csv("../data/raw/API_NY/GDP.csv")
#' 
#' # Read with custom skip rows
#' custom_data <- read_world_bank_csv("custom_file.csv", skip_rows = 2)
#' }
#' 
#' @seealso \code{\link{clean_column_names}}, \code{\link{convert_to_long_format}}
#' 
#' @export
read_world_bank_csv <- function(file_path, skip_rows = 4) {
  
  # Check if file exists
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  # Read the CSV file, skipping metadata rows
  tryCatch({
    data <- read_csv(file_path, skip = skip_rows, show_col_types = FALSE)
    
    # Log successful read
    cat("Successfully read:", basename(file_path), "\n")
    cat("Dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
    
    return(data)
    
  }, error = function(e) {
    stop(paste("Error reading file", file_path, ":", e$message))
  })
}

#' Clean and standardize World Bank data column names
#' 
#' Standardizes the column naming convention for World Bank datasets by
#' ensuring consistent names for the first four metadata columns and
#' cleaning year column names to extract valid 4-digit years.
#' 
#' @param data Tibble. Raw World Bank data with original column names
#' 
#' @return Tibble with cleaned and standardized column names
#' 
#' @details
#' The function performs the following transformations:
#' - Standardizes first 4 columns: country_name, country_code, indicator_name, indicator_code
#' - Extracts 4-digit years from year columns using regex
#' - Removes columns with invalid or missing year names
#' - Maintains data integrity while ensuring consistent structure
#' 
#' @examples
#' \dontrun{
#' # Clean column names after reading raw data
#' raw_data <- read_csv("world_bank_file.csv", skip = 4)
#' clean_data <- clean_column_names(raw_data)
#' }
#' 
#' @seealso \code{\link{read_world_bank_csv}}, \code{\link{convert_to_long_format}}
#' 
#' @export
clean_column_names <- function(data) {
  
  # Standardize the first few columns which are consistent across World Bank files
  names(data)[1:4] <- c("country_name", "country_code", "indicator_name", "indicator_code")
  
  # Clean year column names (remove any extra characters)
  year_cols <- names(data)[5:ncol(data)]
  year_cols <- str_extract(year_cols, "\\d{4}")
  
  # Handle missing/empty column names by removing those columns
  valid_years <- !is.na(year_cols) & year_cols != ""
  
  if (any(!valid_years)) {
    # Remove columns with invalid year names
    invalid_cols <- which(!valid_years) + 4  # Adjust for the first 4 columns
    data <- data[, -invalid_cols]
    year_cols <- year_cols[valid_years]
  }
  
  names(data)[5:ncol(data)] <- year_cols
  
  return(data)
}

#' Convert World Bank wide format to long format
#' 
#' @param data World Bank data in wide format (years as columns)
#' @return Tibble in long format with year and value columns
convert_to_long_format <- function(data) {
  
  # Clean column names first
  data <- clean_column_names(data)
  
  # Convert to long format
  data_long <- data %>%
    pivot_longer(
      cols = matches("^\\d{4}$"),  # Select year columns
      names_to = "year",
      values_to = "value",
      names_transform = list(year = as.integer)
    ) %>%
    # Remove rows with missing values
    filter(!is.na(value)) %>%
    # Select and arrange columns
    select(country_name, country_code, indicator_name, indicator_code, year, value) %>%
    arrange(country_name, year)
  
  return(data_long)
}

#' Standardize country names for consistency across datasets
#' 
#' @param data Tibble with country_name column
#' @return Tibble with standardized country names
standardize_country_names <- function(data) {
  
  # Common country name standardizations
  country_mappings <- c(
    "United States" = "United States",
    "Russian Federation" = "Russia",
    "Korea, Rep." = "South Korea",
    "Iran, Islamic Rep." = "Iran",
    "Egypt, Arab Rep." = "Egypt",
    "Venezuela, RB" = "Venezuela"
  )
  
  # Apply standardizations
  data <- data %>%
    mutate(
      country_name = case_when(
        country_name %in% names(country_mappings) ~ country_mappings[country_name],
        TRUE ~ country_name
      )
    )
  
  return(data)
}

#' Process a single World Bank dataset
#' 
#' @param file_path Path to World Bank CSV file
#' @param indicator_short_name Short name for the indicator (e.g., "gdp", "inflation")
#' @return Processed tibble ready for analysis
process_world_bank_file <- function(file_path, indicator_short_name) {
  
  cat("\n=== Processing", indicator_short_name, "data ===\n")
  
  # Read and process the file
  data <- read_world_bank_csv(file_path) %>%
    convert_to_long_format() %>%
    standardize_country_names() %>%
    # Add short indicator name for easier reference
    mutate(indicator_short = indicator_short_name) %>%
    # Filter out aggregated regions (keep only countries)
    filter(!str_detect(country_code, "^(WLD|EUU|ECS|LCN|MEA|NAC|SAS|SSF|EAS|EAP)"))
  
  # Log processing results
  cat("Countries found:", length(unique(data$country_name)), "\n")
  cat("Year range:", min(data$year, na.rm = TRUE), "-", max(data$year, na.rm = TRUE), "\n")
  cat("Total observations:", nrow(data), "\n")
  
  return(data)
}

#' Handle missing values with appropriate strategies
#' 
#' @param data Tibble with economic data
#' @param indicator_type Type of indicator (gdp, inflation, cpi, gdp_deflator)
#' @return Tibble with missing value handling applied
handle_missing_values <- function(data, indicator_type) {
  
  # Log missing value statistics
  missing_count <- sum(is.na(data$value))
  total_count <- nrow(data)
  missing_pct <- round((missing_count / total_count) * 100, 2)
  
  cat("Missing values for", indicator_type, ":", missing_count, "/", total_count, 
      "(", missing_pct, "%)\n")
  
  # Strategy: Remove rows with missing values but log countries with high missingness
  countries_with_high_missing <- data %>%
    group_by(country_name) %>%
    summarise(
      total_obs = n(),
      missing_obs = sum(is.na(value)),
      missing_pct = round((missing_obs / total_obs) * 100, 2),
      .groups = "drop"
    ) %>%
    filter(missing_pct > 50) %>%
    arrange(desc(missing_pct))
  
  if (nrow(countries_with_high_missing) > 0) {
    cat("Countries with >50% missing data:\n")
    print(countries_with_high_missing)
  }
  
  # Remove rows with missing values
  data_clean <- data %>%
    filter(!is.na(value))
  
  cat("After removing missing values:", nrow(data_clean), "observations remaining\n")
  
  return(data_clean)
}

#' Clean GDP data from World Bank CSV
#' 
#' @param file_path Path to GDP CSV file
#' @return Processed GDP data in long format
clean_gdp_data <- function(file_path = "../../data/raw/API_NY/GDP.csv") {
  
  cat("\n=== Processing GDP Data ===\n")
  
  if (!file.exists(file_path)) {
    stop(paste("GDP file not found:", file_path))
  }
  
  # Read and process GDP data (skip first 4 header rows)
  gdp_data <- read_world_bank_csv(file_path, skip_rows = 4) %>%
    convert_to_long_format() %>%
    standardize_country_names() %>%
    mutate(indicator_short = "gdp") %>%
    # Filter out aggregated regions (keep only countries)
    filter(!str_detect(country_code, "^(WLD|EUU|ECS|LCN|MEA|NAC|SAS|SSF|EAS|EAP)")) %>%
    # Handle missing values
    handle_missing_values("GDP")
  
  cat("GDP data processed:", nrow(gdp_data), "observations\n")
  cat("Countries:", length(unique(gdp_data$country_name)), "\n")
  cat("Year range:", min(gdp_data$year), "-", max(gdp_data$year), "\n")
  
  return(gdp_data)
}

#' Clean inflation data from World Bank CSV
#' 
#' @param file_path Path to inflation CSV file
#' @return Processed inflation data in long format
clean_inflation_data <- function(file_path = "../../data/raw/API_FP/API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_122376.csv") {
  
  cat("\n=== Processing Inflation Data ===\n")
  
  if (!file.exists(file_path)) {
    stop(paste("Inflation file not found:", file_path))
  }
  
  # Read and process inflation data (skip first 4 header rows)
  inflation_data <- read_world_bank_csv(file_path, skip_rows = 4) %>%
    convert_to_long_format() %>%
    standardize_country_names() %>%
    mutate(indicator_short = "inflation") %>%
    # Filter out aggregated regions (keep only countries)
    filter(!str_detect(country_code, "^(WLD|EUU|ECS|LCN|MEA|NAC|SAS|SSF|EAS|EAP)")) %>%
    # Handle missing values
    handle_missing_values("Inflation")
  
  cat("Inflation data processed:", nrow(inflation_data), "observations\n")
  cat("Countries:", length(unique(inflation_data$country_name)), "\n")
  cat("Year range:", min(inflation_data$year), "-", max(inflation_data$year), "\n")
  
  return(inflation_data)
}

#' Clean CPI data from World Bank CSV
#' 
#' @param file_path Path to CPI CSV file
#' @return Processed CPI data in long format
clean_cpi_data <- function(file_path = "../../data/raw/API_FP-2/API_FP.CPI.TOTL_DS2_en_csv_v2_37831.csv") {
  
  cat("\n=== Processing CPI Data ===\n")
  
  if (!file.exists(file_path)) {
    stop(paste("CPI file not found:", file_path))
  }
  
  # Read and process CPI data (skip first 4 header rows)
  cpi_data <- read_world_bank_csv(file_path, skip_rows = 4) %>%
    convert_to_long_format() %>%
    standardize_country_names() %>%
    mutate(indicator_short = "cpi") %>%
    # Filter out aggregated regions (keep only countries)
    filter(!str_detect(country_code, "^(WLD|EUU|ECS|LCN|MEA|NAC|SAS|SSF|EAS|EAP)")) %>%
    # Handle missing values
    handle_missing_values("CPI")
  
  cat("CPI data processed:", nrow(cpi_data), "observations\n")
  cat("Countries:", length(unique(cpi_data$country_name)), "\n")
  cat("Year range:", min(cpi_data$year), "-", max(cpi_data$year), "\n")
  
  return(cpi_data)
}

#' Clean GDP deflator data from World Bank CSV
#' 
#' @param file_path Path to GDP deflator CSV file
#' @return Processed GDP deflator data in long format
clean_gdp_deflator_data <- function(file_path = "../../data/raw/API_NY-2/API_NY.GDP.DEFL.KD.ZG_DS2_en_csv_v2_37873.csv") {
  
  cat("\n=== Processing GDP Deflator Data ===\n")
  
  if (!file.exists(file_path)) {
    stop(paste("GDP deflator file not found:", file_path))
  }
  
  # Read and process GDP deflator data (skip first 4 header rows)
  gdp_deflator_data <- read_world_bank_csv(file_path, skip_rows = 4) %>%
    convert_to_long_format() %>%
    standardize_country_names() %>%
    mutate(indicator_short = "gdp_deflator") %>%
    # Filter out aggregated regions (keep only countries)
    filter(!str_detect(country_code, "^(WLD|EUU|ECS|LCN|MEA|NAC|SAS|SSF|EAS|EAP)")) %>%
    # Handle missing values
    handle_missing_values("GDP Deflator")
  
  cat("GDP deflator data processed:", nrow(gdp_deflator_data), "observations\n")
  cat("Countries:", length(unique(gdp_deflator_data$country_name)), "\n")
  cat("Year range:", min(gdp_deflator_data$year), "-", max(gdp_deflator_data$year), "\n")
  
  return(gdp_deflator_data)
}

#' Main function to process all World Bank data files
#' 
#' This function orchestrates the entire data cleaning process for all economic
#' indicators, providing a complete pipeline from raw World Bank CSV files to
#' cleaned, analysis-ready datasets with comprehensive logging and error handling.
#' 
#' @return List of processed datasets by indicator type (gdp, inflation, cpi, gdp_deflator)
#' 
#' @details
#' The function performs the following operations:
#' 1. Creates output directory structure if needed
#' 2. Processes each economic indicator dataset:
#'    - GDP (Current US$): NY.GDP.MKTP.CD
#'    - Inflation Rate: FP.CPI.TOTL.ZG  
#'    - Consumer Price Index: FP.CPI.TOTL
#'    - GDP Deflator: NY.GDP.DEFL.KD.ZG
#' 3. Applies comprehensive data cleaning and validation
#' 4. Exports individual cleaned datasets to CSV files
#' 5. Provides detailed logging and error reporting
#' 
#' @section File Structure:
#' Expected input files in data/raw/:
#' - API_NY/GDP.csv (GDP data)
#' - API_FP/API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_122376.csv (Inflation)
#' - API_FP-2/API_FP.CPI.TOTL_DS2_en_csv_v2_37831.csv (CPI)
#' - API_NY-2/API_NY.GDP.DEFL.KD.ZG_DS2_en_csv_v2_37873.csv (GDP Deflator)
#' 
#' @section Output:
#' Creates cleaned CSV files in data/cleaned/:
#' - gdp_cleaned.csv
#' - inflation_cleaned.csv
#' - cpi_cleaned.csv
#' - gdp_deflator_cleaned.csv
#' 
#' @examples
#' \dontrun{
#' # Process all World Bank data files
#' processed_data <- process_all_data()
#' 
#' # Access individual datasets
#' gdp_data <- processed_data$gdp
#' inflation_data <- processed_data$inflation
#' }
#' 
#' @seealso \code{\link{clean_gdp_data}}, \code{\link{clean_inflation_data}}, 
#'          \code{\link{clean_cpi_data}}, \code{\link{clean_gdp_deflator_data}}
#' 
#' @export
process_all_data <- function() {
  
  cat("=== Starting Global Consumer Pulse Data Processing ===\n")
  
  # Create output directory if it doesn't exist
  if (!dir.exists("../../data/cleaned")) {
    dir.create("../../data/cleaned", recursive = TRUE)
    cat("Created cleaned data directory\n")
  }
  
  # Process each dataset
  processed_data <- list()
  
  # Process GDP data
  tryCatch({
    processed_data$gdp <- clean_gdp_data()
  }, error = function(e) {
    cat("Error processing GDP data:", e$message, "\n")
  })
  
  # Process inflation data
  tryCatch({
    processed_data$inflation <- clean_inflation_data()
  }, error = function(e) {
    cat("Error processing inflation data:", e$message, "\n")
  })
  
  # Process CPI data
  tryCatch({
    processed_data$cpi <- clean_cpi_data()
  }, error = function(e) {
    cat("Error processing CPI data:", e$message, "\n")
  })
  
  # Process GDP deflator data
  tryCatch({
    processed_data$gdp_deflator <- clean_gdp_deflator_data()
  }, error = function(e) {
    cat("Error processing GDP deflator data:", e$message, "\n")
  })
  
  # Save individual processed datasets
  for (indicator in names(processed_data)) {
    if (!is.null(processed_data[[indicator]])) {
      output_file <- paste0("../../data/cleaned/", indicator, "_cleaned.csv")
      write_csv(processed_data[[indicator]], output_file)
      cat("Saved", indicator, "data to", output_file, "\n")
    }
  }
  
  cat("\n=== Data processing complete ===\n")
  cat("Processed datasets:", paste(names(processed_data), collapse = ", "), "\n")
  
  return(processed_data)
}

#' Calculate year-over-year changes and growth rates
#' 
#' @param data Tibble with country, year, and value columns
#' @param indicator_type Type of indicator for appropriate calculations
#' @return Tibble with additional calculated fields
calculate_derived_metrics <- function(data, indicator_type) {
  
  cat("Calculating derived metrics for", indicator_type, "\n")
  
  # Calculate year-over-year changes and growth rates
  data_with_metrics <- data %>%
    arrange(country_name, year) %>%
    group_by(country_name) %>%
    mutate(
      # Previous year value for calculations
      value_prev = lag(value, 1),
      
      # Year-over-year absolute change
      yoy_change = value - value_prev,
      
      # Year-over-year percentage change
      yoy_pct_change = case_when(
        is.na(value_prev) | value_prev == 0 ~ NA_real_,
        TRUE ~ ((value - value_prev) / abs(value_prev)) * 100
      ),
      
      # Growth rate (for GDP, use percentage change; for rates like inflation, use absolute change)
      growth_rate = case_when(
        indicator_type == "gdp" ~ yoy_pct_change,
        indicator_type %in% c("inflation", "cpi", "gdp_deflator") ~ yoy_change,
        TRUE ~ yoy_pct_change
      ),
      
      # Trend indicator (simplified: positive, negative, stable)
      trend_indicator = case_when(
        is.na(growth_rate) ~ "unknown",
        abs(growth_rate) < 0.1 ~ "stable",
        growth_rate > 0 ~ "increasing",
        growth_rate < 0 ~ "decreasing",
        TRUE ~ "stable"
      ),
      
      # 3-year moving average for smoothing
      value_3yr_avg = zoo::rollmean(value, k = 3, fill = NA, align = "right")
    ) %>%
    ungroup() %>%
    # Remove the temporary previous value column
    select(-value_prev)
  
  # Log calculation results
  calculated_rows <- sum(!is.na(data_with_metrics$yoy_change))
  total_rows <- nrow(data_with_metrics)
  
  cat("Calculated metrics for", calculated_rows, "out of", total_rows, "observations\n")
  
  return(data_with_metrics)
}

#' Implement data validation and quality checks
#' 
#' @param data Tibble with economic data
#' @param indicator_type Type of indicator for validation rules
#' @return List with validation results and cleaned data
validate_data_quality <- function(data, indicator_type) {
  
  cat("\n=== Data Quality Validation for", indicator_type, "===\n")
  
  # Initialize validation results
  validation_results <- list(
    indicator = indicator_type,
    total_observations = nrow(data),
    issues = list(),
    warnings = character(),
    passed = TRUE
  )
  
  # Check 1: Missing values
  missing_values <- sum(is.na(data$value))
  missing_pct <- round((missing_values / nrow(data)) * 100, 2)
  
  if (missing_pct > 20) {
    validation_results$warnings <- c(validation_results$warnings,
                                   paste("High missing values:", missing_pct, "%"))
  }
  
  validation_results$issues$missing_values <- list(
    count = missing_values,
    percentage = missing_pct
  )
  
  # Check 2: Outliers (values beyond reasonable economic ranges)
  outlier_thresholds <- list(
    gdp = list(min = 0, max = 30e12),  # $30 trillion max GDP
    inflation = list(min = -50, max = 1000),  # -50% to 1000% inflation
    cpi = list(min = 0, max = 1000),  # CPI index 0-1000
    gdp_deflator = list(min = -50, max = 1000)  # -50% to 1000% deflator change
  )
  
  if (indicator_type %in% names(outlier_thresholds)) {
    thresholds <- outlier_thresholds[[indicator_type]]
    outliers <- data %>%
      filter(!is.na(value)) %>%
      filter(value < thresholds$min | value > thresholds$max)
    
    if (nrow(outliers) > 0) {
      validation_results$warnings <- c(validation_results$warnings,
                                     paste("Found", nrow(outliers), "potential outliers"))
      validation_results$issues$outliers <- outliers %>%
        select(country_name, year, value) %>%
        arrange(desc(abs(value)))
    }
  }
  
  # Check 3: Data coverage by country
  country_coverage <- data %>%
    filter(!is.na(value)) %>%
    group_by(country_name) %>%
    summarise(
      years_available = n(),
      year_range = paste(min(year), "-", max(year)),
      coverage_pct = round((n() / length(unique(data$year))) * 100, 2),
      .groups = "drop"
    ) %>%
    arrange(coverage_pct)
  
  low_coverage_countries <- country_coverage %>%
    filter(coverage_pct < 50)
  
  if (nrow(low_coverage_countries) > 0) {
    validation_results$warnings <- c(validation_results$warnings,
                                   paste(nrow(low_coverage_countries), "countries with <50% data coverage"))
    validation_results$issues$low_coverage_countries <- low_coverage_countries
  }
  
  # Check 4: Temporal consistency
  temporal_gaps <- data %>%
    filter(!is.na(value)) %>%
    group_by(country_name) %>%
    arrange(year) %>%
    mutate(year_gap = year - lag(year, default = year[1])) %>%
    filter(year_gap > 1) %>%
    ungroup()
  
  if (nrow(temporal_gaps) > 0) {
    validation_results$warnings <- c(validation_results$warnings,
                                   paste("Found", nrow(temporal_gaps), "temporal gaps in data"))
  }
  
  # Log validation results
  cat("Total observations:", validation_results$total_observations, "\n")
  cat("Missing values:", validation_results$issues$missing_values$count, 
      "(", validation_results$issues$missing_values$percentage, "%)\n")
  
  if (length(validation_results$warnings) > 0) {
    cat("Warnings:\n")
    for (warning in validation_results$warnings) {
      cat("  -", warning, "\n")
    }
  } else {
    cat("No data quality issues detected\n")
  }
  
  return(list(
    validation = validation_results,
    data = data
  ))
}

#' Combine all economic indicators into unified dataset
#' 
#' @param processed_data List of processed datasets from process_all_data()
#' @return Unified tibble with all economic indicators
combine_economic_datasets <- function(processed_data = NULL) {
  
  cat("\n=== Combining Economic Datasets ===\n")
  
  # If no processed data provided, load from cleaned files
  if (is.null(processed_data)) {
    cat("Loading data from cleaned CSV files...\n")
    
    processed_data <- list()
    
    # Load existing cleaned files
    cleaned_files <- list(
      gdp = "../../data/cleaned/gdp_cleaned.csv",
      inflation = "../../data/cleaned/inflation_cleaned.csv", 
      cpi = "../../data/cleaned/cpi_cleaned.csv",
      gdp_deflator = "../../data/cleaned/gdp_deflator_cleaned.csv"
    )
    
    for (indicator in names(cleaned_files)) {
      file_path <- cleaned_files[[indicator]]
      if (file.exists(file_path)) {
        processed_data[[indicator]] <- read_csv(file_path, show_col_types = FALSE)
        cat("Loaded", indicator, "data:", nrow(processed_data[[indicator]]), "observations\n")
      } else {
        cat("Warning: File not found:", file_path, "\n")
      }
    }
  }
  
  # Combine all datasets
  combined_data <- tibble()
  
  for (indicator in names(processed_data)) {
    if (!is.null(processed_data[[indicator]])) {
      
      # Add calculated metrics
      data_with_metrics <- calculate_derived_metrics(processed_data[[indicator]], indicator)
      
      # Validate data quality
      validation_result <- validate_data_quality(data_with_metrics, indicator)
      
      # Combine with main dataset
      combined_data <- bind_rows(combined_data, validation_result$data)
      
      cat("Added", indicator, "data:", nrow(validation_result$data), "observations\n")
    }
  }
  
  # Final data processing and standardization
  unified_data <- combined_data %>%
    # Ensure consistent column types
    mutate(
      country_name = as.character(country_name),
      country_code = as.character(country_code),
      year = as.integer(year),
      value = as.numeric(value),
      indicator_short = as.character(indicator_short),
      yoy_change = as.numeric(yoy_change),
      yoy_pct_change = as.numeric(yoy_pct_change),
      growth_rate = as.numeric(growth_rate),
      trend_indicator = as.character(trend_indicator)
    ) %>%
    # Add metadata columns
    mutate(
      data_quality_score = case_when(
        is.na(value) ~ 0,
        is.na(yoy_change) ~ 0.7,
        TRUE ~ 1.0
      ),
      last_updated = Sys.Date()
    ) %>%
    # Arrange for better readability
    arrange(country_name, indicator_short, year)
  
  # Log final results
  cat("\n=== Unified Dataset Summary ===\n")
  cat("Total observations:", nrow(unified_data), "\n")
  cat("Countries:", length(unique(unified_data$country_name)), "\n")
  cat("Indicators:", paste(unique(unified_data$indicator_short), collapse = ", "), "\n")
  cat("Year range:", min(unified_data$year, na.rm = TRUE), "-", max(unified_data$year, na.rm = TRUE), "\n")
  
  # Summary by indicator
  indicator_summary <- unified_data %>%
    group_by(indicator_short) %>%
    summarise(
      observations = n(),
      countries = n_distinct(country_name),
      years = n_distinct(year),
      missing_values = sum(is.na(value)),
      .groups = "drop"
    )
  
  cat("\nIndicator Summary:\n")
  print(indicator_summary)
  
  return(unified_data)
}

#' Export unified economic dataset with proper data types
#' 
#' @param unified_data Unified economic dataset
#' @param output_path Path for output CSV file
#' @return TRUE if export successful
export_unified_dataset <- function(unified_data, output_path = "../../data/cleaned/economic_data.csv") {
  
  cat("\n=== Exporting Unified Dataset ===\n")
  
  # Ensure output directory exists
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output directory:", output_dir, "\n")
  }
  
  # Export main dataset
  tryCatch({
    write_csv(unified_data, output_path)
    cat("Successfully exported unified dataset to:", output_path, "\n")
    cat("File size:", round(file.size(output_path) / 1024 / 1024, 2), "MB\n")
    
    # Create data dictionary
    dictionary_path <- gsub("\\.csv$", "_dictionary.csv", output_path)
    
    data_dictionary <- tibble(
      column_name = names(unified_data),
      data_type = sapply(unified_data, class),
      description = c(
        "Standardized country name",
        "ISO 3-letter country code", 
        "Full indicator name from World Bank",
        "World Bank indicator code",
        "Year of observation",
        "Indicator value in original units",
        "Short indicator name (gdp, inflation, cpi, gdp_deflator)",
        "Year-over-year absolute change",
        "Year-over-year percentage change", 
        "Growth rate (context-dependent calculation)",
        "Trend classification (increasing/decreasing/stable/unknown)",
        "3-year rolling average value",
        "Data quality score (0-1)",
        "Date when data was last processed"
      )
    )
    
    write_csv(data_dictionary, dictionary_path)
    cat("Data dictionary exported to:", dictionary_path, "\n")
    
    # Create summary statistics file
    summary_path <- gsub("\\.csv$", "_summary.csv", output_path)
    
    summary_stats <- unified_data %>%
      group_by(indicator_short) %>%
      summarise(
        total_observations = n(),
        unique_countries = n_distinct(country_name),
        year_range_start = min(year, na.rm = TRUE),
        year_range_end = max(year, na.rm = TRUE),
        missing_values = sum(is.na(value)),
        missing_percentage = round((sum(is.na(value)) / n()) * 100, 2),
        mean_value = round(mean(value, na.rm = TRUE), 2),
        median_value = round(median(value, na.rm = TRUE), 2),
        min_value = min(value, na.rm = TRUE),
        max_value = max(value, na.rm = TRUE),
        .groups = "drop"
      )
    
    write_csv(summary_stats, summary_path)
    cat("Summary statistics exported to:", summary_path, "\n")
    
    return(TRUE)
    
  }, error = function(e) {
    cat("Error exporting dataset:", e$message, "\n")
    return(FALSE)
  })
}

#' Main function to build unified economic dataset with quality controls
#' 
#' This function implements Task 3: combines all indicators, adds calculated fields,
#' implements quality controls, and exports the final dataset
build_unified_economic_dataset <- function() {
  
  cat("=== Building Unified Economic Dataset with Quality Controls ===\n")
  cat("Task 3: Combine GDP, inflation, CPI, and GDP deflator data\n")
  cat("Adding calculated fields and implementing quality controls\n\n")
  
  # Load required library for moving averages
  if (!require(zoo, quietly = TRUE)) {
    # Set CRAN mirror and install zoo package
    options(repos = c(CRAN = "https://cran.rstudio.com/"))
    install.packages("zoo")
    library(zoo)
  }
  
  # Step 1: Combine all economic datasets
  unified_data <- combine_economic_datasets()
  
  # Step 2: Export unified dataset with proper data types
  export_success <- export_unified_dataset(unified_data)
  
  if (export_success) {
    cat("\n=== Task 3 Completed Successfully ===\n")
    cat("✓ Combined GDP, inflation, CPI, and GDP deflator data\n")
    cat("✓ Added calculated fields: year-over-year changes, growth rates, trend indicators\n")
    cat("✓ Implemented data validation and quality checks with logging\n")
    cat("✓ Exported cleaned data to data/cleaned/economic_data.csv with proper data types\n")
    cat("✓ Utilized existing cleaned data files\n")
    
    return(unified_data)
  } else {
    cat("\n=== Task 3 Failed ===\n")
    cat("Error occurred during dataset export\n")
    return(NULL)
  }
}

# Example usage (uncomment to run):
# processed_data <- process_all_data()

# Individual dataset processing examples:
# gdp_data <- clean_gdp_data()
# inflation_data <- clean_inflation_data()
# cpi_data <- clean_cpi_data()
# gdp_deflator_data <- clean_gdp_deflator_data()

# Task 3: Build unified economic dataset
# unified_data <- build_unified_economic_dataset()