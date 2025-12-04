# ============================================================================
# Introduction to Data Science - Group Assignment
# Data Cleaning and Merging 

# Load required packages
library(tidyverse)
library(dplyr)
library(readr)

# ============================================================================
# PART 1: Load Data
# ============================================================================

cat("=== STEP 1: LOADING DATA ===\n\n")

# Load the three required datasets
gdp <- read_csv("gdp-per-capita-worldbank.csv")
neet <- read_csv("youth-not-in-education-employment-training.csv")
continent <- read_csv("continents-according-to-our-world-in-data.csv")

# Load additional datasets
edu_raw <- read_csv("government_expenditure_on_education.csv", skip = 4)
ldc_data <- read_csv("ldc_data.csv")
sdi_data <- read_csv("sustainable_development_index.csv")
population_raw <- read_csv("population_data.csv")

cat("GDP data:", nrow(gdp), "rows\n")
cat("NEET data:", nrow(neet), "rows\n")
cat("Continent data:", nrow(continent), "rows\n")
cat("Education data:", nrow(edu_raw), "rows\n")
cat("LDC classification:", nrow(ldc_data), "rows\n")
cat("SDI data:", nrow(sdi_data), "rows\n")
cat("Population data:", nrow(population_raw), "rows\n\n")

# ============================================================================
# PART 2: Data Cleaning and Renaming
# ============================================================================

cat("=== STEP 2: CLEANING DATA ===\n\n")

# Clean GDP data
gdp_clean <- gdp %>%
  rename(
    country = Entity, 
    code = Code, 
    year = Year, 
    gdp_per_capita = `GDP per capita, PPP (constant 2017 international $)`
  ) %>%
  distinct(code, year, .keep_all = TRUE)

# Clean NEET data
neet_clean <- neet %>%
  rename(
    country = Entity, 
    code = Code, 
    year = Year, 
    neet_share = `Share of youth not in education, employment or training, total (% of youth population)`
  ) %>%
  distinct(code, year, .keep_all = TRUE)

# Clean continent classification data
continent_clean <- continent %>%
  rename(
    country = Entity, 
    code = Code, 
    year = Year, 
    continent = Continent
  ) %>%
  filter(continent != "Antarctica") %>%
  select(code, continent) %>%
  distinct()

# Clean education expenditure data
edu_long <- edu_raw %>%
  select(`Country Name`, `Country Code`, matches("^\\d{4}$")) %>%
  pivot_longer(
    cols = -c(`Country Name`, `Country Code`),
    names_to = "year", 
    values_to = "edu_expenditure_gdp"
  ) %>%
  rename(country = `Country Name`, code = `Country Code`) %>%
  mutate(year = as.integer(year)) %>%
  distinct(code, year, .keep_all = TRUE)

# Clean LDC classification data
ldc_clean <- ldc_data %>%
  rename(
    ccode = CCODE,
    iso3 = `ISO -3`,
    country_ldc = Countries,
    ldc_status = Status
  ) %>%
  mutate(
    code = iso3,
    ldc_status = toupper(trimws(ldc_status)),
    is_ldc = ldc_status == "LDC"
  ) %>%
  select(code, ldc_status, is_ldc) %>%
  distinct()

cat("LDC classification cleaned\n")
cat("  - LDC countries:", sum(ldc_clean$is_ldc), "\n")
cat("  - ODC/Other countries:", sum(!ldc_clean$is_ldc), "\n\n")

# Clean SDI data
sdi_clean <- sdi_data %>%
  rename(code = iso) %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "year",
    values_to = "sdi_score"
  ) %>%
  mutate(
    year = as.integer(year),
    sdi_score = as.numeric(sdi_score)
  ) %>%
  filter(!is.na(sdi_score), year >= 2000) %>%
  select(code, year, sdi_score) %>%
  distinct(code, year, .keep_all = TRUE)

cat("SDI data cleaned\n")
cat("  - Year range:", min(sdi_clean$year, na.rm = TRUE), "to", 
    max(sdi_clean$year, na.rm = TRUE), "\n")
cat("  - Countries:", n_distinct(sdi_clean$code), "\n\n")

# ============================================================================
# Clean Population data - WORLD BANK WIDE FORMAT
# ============================================================================

cat("Cleaning population data (World Bank format)...\n")

# Read with skip to handle metadata rows
pop_raw <- read_csv("population_data.csv", skip = 4, show_col_types = FALSE)

cat("Original data:", nrow(pop_raw), "rows,", ncol(pop_raw), "columns\n")

# World Bank format typically has:
# Country Name | Country Code | Indicator Name | Indicator Code | 1960 | 1961 | ...

# Find the country code column (usually 2nd column)
code_col_idx <- 2

# Find year columns (4-digit numbers)
all_cols <- colnames(pop_raw)
year_cols <- all_cols[grepl("^[12][0-9]{3}$", all_cols)]

if(length(year_cols) == 0) {
  cat("Warning: No year columns found with 4-digit pattern.\n")
  cat("Trying to detect year columns by position (columns 5 onwards)...\n")
  # Assume years start from column 5
  year_cols <- colnames(pop_raw)[5:ncol(pop_raw)]
  # Filter to only numeric columns
  year_cols <- year_cols[sapply(pop_raw[year_cols], is.numeric)]
}

cat("Found", length(year_cols), "year columns\n")
cat("Year range:", head(year_cols, 1), "to", tail(year_cols, 1), "\n")

# Convert to long format
population_clean <- pop_raw %>%
  # Select country code + all year columns
  select(code = all_of(code_col_idx), all_of(year_cols)) %>%
  # Convert to long format
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "population"
  ) %>%
  # Clean data types
  mutate(
    code = as.character(code),
    year = as.integer(year),
    population = as.numeric(population)
  ) %>%
  # Remove missing values
  filter(!is.na(code), !is.na(year), !is.na(population)) %>%
  # Remove duplicates
  distinct(code, year, .keep_all = TRUE)

# Verify success
if(nrow(population_clean) == 0) {
  cat("\n❌ ERROR: Cleaning produced 0 rows!\n")
  cat("Sample of original data:\n")
  print(head(pop_raw[, 1:5], 3))
  stop("Population cleaning failed")
}

year_min <- min(population_clean$year, na.rm = TRUE)
year_max <- max(population_clean$year, na.rm = TRUE)

if(is.infinite(year_min)) {
  cat("\n❌ ERROR: Year conversion failed\n")
  stop("Could not extract years from column names")
}

cat("✓ Population data cleaned\n")
cat("  Year range:", year_min, "to", year_max, "\n")
cat("  Countries:", n_distinct(population_clean$code), "\n")
cat("  Total observations:", nrow(population_clean), "\n\n")

# ============================================================================
# PART 3: Merge All Datasets
# ============================================================================

cat("=== STEP 3: MERGING ALL DATASETS ===\n\n")

# Merge step by step with clear documentation
master_data <- gdp_clean %>%
  # Step 1: Merge NEET data
  left_join(
    neet_clean, 
    by = c("code", "year"), 
    suffix = c("_gdp", "_neet")
  ) %>%
  # Step 2: Merge education expenditure data
  left_join(
    edu_long, 
    by = c("code", "year")
  ) %>%
  # Step 3: Merge LDC classification (by code only)
  left_join(
    ldc_clean,
    by = "code"
  ) %>%
  # Step 4: Merge SDI data
  left_join(
    sdi_clean,
    by = c("code", "year")
  ) %>%
  # Step 5: Merge Population data (NEW!)
  left_join(
    population_clean,
    by = c("code", "year")
  ) %>%
  # Step 6: Merge continent classification
  left_join(
    continent_clean, 
    by = "code"
  ) %>%
  # Handle duplicate country columns and set default values
  mutate(
    country = coalesce(country_gdp, country_neet, country),
    ldc_status = ifelse(is.na(ldc_status), "UNKNOWN", ldc_status),
    is_ldc = ifelse(is.na(is_ldc), FALSE, is_ldc)
  ) %>%
  # Reorganize columns in logical order
  select(
    # Identifiers
    country, code, year, continent,
    # Development status
    ldc_status, is_ldc,
    # Economic indicators
    gdp_per_capita, 
    # Population (NEW!)
    population,
    # Employment/Education indicators
    neet_share, edu_expenditure_gdp,
    # Sustainability indicator
    sdi_score
  ) %>%
  # Keep observations with at least one piece of data
  filter(!is.na(gdp_per_capita) | !is.na(neet_share) | 
           !is.na(edu_expenditure_gdp) | !is.na(sdi_score) | !is.na(population)) %>%
  # Keep only data with continent classification
  filter(!is.na(continent))

cat("Master dataset created successfully!\n")
cat("Total observations:", nrow(master_data), "\n")
cat("Countries:", n_distinct(master_data$code), "\n")
cat("Year range:", min(master_data$year), "to", max(master_data$year), "\n\n")

# ============================================================================
# PART 4: Data Quality Check
# ============================================================================

cat("=== STEP 4: DATA QUALITY CHECK ===\n\n")

# View structure
glimpse(master_data)

# Check LDC distribution
cat("\n--- LDC Status Distribution ---\n")
ldc_summary <- master_data %>%
  distinct(code, .keep_all = TRUE) %>%
  count(ldc_status) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))
print(ldc_summary)

# Check data completeness by continent
cat("\n--- Data Completeness by Continent ---\n")
data_coverage <- master_data %>%
  group_by(continent) %>%
  summarise(
    n_countries = n_distinct(code),
    n_ldc = sum(is_ldc, na.rm = TRUE),
    n_observations = n(),
    gdp_coverage = round(sum(!is.na(gdp_per_capita)) / n() * 100, 1),
    neet_coverage = round(sum(!is.na(neet_share)) / n() * 100, 1),
    edu_coverage = round(sum(!is.na(edu_expenditure_gdp)) / n() * 100, 1),
    sdi_coverage = round(sum(!is.na(sdi_score)) / n() * 100, 1),
    pop_coverage = round(sum(!is.na(population)) / n() * 100, 1),  # NEW!
    .groups = "drop"
  )
print(data_coverage)

# Check missing values
cat("\n--- Missing Value Statistics ---\n")
master_data %>%
  summarise(
    total_obs = n(),
    gdp_missing = sum(is.na(gdp_per_capita)),
    neet_missing = sum(is.na(neet_share)),
    edu_missing = sum(is.na(edu_expenditure_gdp)),
    sdi_missing = sum(is.na(sdi_score)),
    pop_missing = sum(is.na(population)),  # NEW!
    ldc_unknown = sum(ldc_status == "UNKNOWN")
  ) %>%
  print()

# Check population coverage for LDCs (important for weighted analysis)
cat("\n--- Population Coverage for LDCs ---\n")
ldc_pop_check <- master_data %>%
  filter(is_ldc == TRUE, year >= 2015, year <= 2023) %>%
  summarise(
    total_obs = n(),
    pop_available = sum(!is.na(population)),
    pop_coverage_pct = round(pop_available / total_obs * 100, 1)
  )
print(ldc_pop_check)

# ============================================================================
# PART 5: Save Cleaned Data
# ============================================================================

cat("\n=== STEP 5: SAVING CLEANED DATA ===\n\n")

# Save master dataset
write_csv(master_data, "master_dataset.csv")
cat("✓ Saved: master_dataset.csv\n")

# Save a summary for reference
summary_stats <- data.frame(
  Dataset = c("Total Observations", "Unique Countries", "Year Range", 
              "LDC Countries", "Non-LDC Countries", "Continents",
              "Population Coverage (%)"),
  Value = c(
    nrow(master_data),
    n_distinct(master_data$code),
    paste(min(master_data$year), "-", max(master_data$year)),
    sum(master_data %>% distinct(code, .keep_all = TRUE) %>% pull(is_ldc)),
    sum(!(master_data %>% distinct(code, .keep_all = TRUE) %>% pull(is_ldc))),
    n_distinct(master_data$continent),
    round(sum(!is.na(master_data$population)) / nrow(master_data) * 100, 1)
  )
)

write_csv(summary_stats, "data_summary.csv")
cat("✓ Saved: data_summary.csv\n\n")




