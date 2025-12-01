# ============================================================================
# Introduction to Data Science - Group Assignment

# Load required packages
library(tidyverse)
library(dplyr)
library(readr)

# ============================================================================
# PART 1: Load Data
# ============================================================================
# Note: All CSV files should be in the same directory as this R script

# Load the required datasets
gdp <- read_csv("gdp-per-capita-worldbank.csv")
neet <- read_csv("youth-not-in-education-employment-training.csv")
continent <- read_csv("continents-according-to-our-world-in-data.csv")

# Load the additional datasets
edu_raw <- read_csv("government_expenditure_on_education.csv", skip = 4)

# Load SDI Score Data (Socio-demographic Index)
# Source: Global Burden of Disease Study
sdi_raw <- read_csv("SDI+and+Components+(1990-2022).csv")

cat("\n=== Data loaded successfully ===\n")
cat("GDP data:", nrow(gdp), "rows\n")
cat("NEET data:", nrow(neet), "rows\n")
cat("Continent data:", nrow(continent), "rows\n")
cat("Education data:", nrow(edu_raw), "rows\n")
cat("SDI data:", nrow(sdi_raw), "rows\n\n")

# ============================================================================
# PART 2: Data Cleaning and Renaming
# ============================================================================

# Clean GDP data
gdp_clean <- gdp %>%
  rename(
    country = Entity, 
    code = Code, 
    year = Year, 
    gdp_per_capita = `GDP per capita, PPP (constant 2017 international $)`
  ) %>%
  distinct(code, year, .keep_all = TRUE) # Remove duplicate rows

# Clean NEET data
neet_clean <- neet %>%
  rename(
    country = Entity, 
    code = Code, 
    year = Year, 
    neet_share = `Share of youth not in education, employment or training, total (% of youth population)`
  ) %>%
  distinct(code, year, .keep_all = TRUE) # Remove duplicate rows

# Clean continent classification data
continent_clean <- continent %>%
  rename(
    country = Entity, 
    code = Code, 
    year = Year, 
    continent = Continent
  ) %>%
  filter(continent != "Antarctica") %>% # Exclude Antarctica (as per assignment requirements)
  select(code, continent) %>%
  distinct() # Keep only country code and continent, remove duplicates

# Clean education expenditure data (convert from wide to long format)
edu_long <- edu_raw %>%
  # Select country name, country code, and year columns (year columns are 4-digit numbers)
  select(`Country Name`, `Country Code`, matches("^\\d{4}$")) %>%
  # Convert from wide to long format
  pivot_longer(
    cols = -c(`Country Name`, `Country Code`),
    names_to = "year", 
    values_to = "edu_expenditure_gdp"
  ) %>%
  # Rename columns
  rename(country = `Country Name`, code = `Country Code`) %>%
  # Convert year to integer format
  mutate(year = as.integer(year)) %>%
  # Remove duplicate rows
  distinct(code, year, .keep_all = TRUE)

# Clean SDI data (Socio-demographic Index)
# SDI is a composite measure of development status based on average income per person,
# educational attainment, and fertility rates
sdi_clean <- sdi_raw %>%
  rename(
    code = iso
  ) %>%
  # Pivot the year columns (1990 to 2022) into long format
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "year",
    values_to = "sdi_score"
  ) %>%
  # Convert year to integer and sdi_score to numeric
  mutate(
    year = as.integer(year),
    sdi_score = as.numeric(sdi_score)
  ) %>%
  # Filter: Only keep data from 2000 onwards to align with other datasets
  filter(year >= 2000) %>%
  # Remove observations where SDI score is NA
  filter(!is.na(sdi_score)) %>%
  # Keep only necessary columns
  select(code, year, sdi_score) %>%
  # Remove duplicate rows based on code and year
  distinct(code, year, .keep_all = TRUE)

cat("=== Data cleaned successfully ===\n")
cat("SDI data filtered to years 2000-2022\n")
cat("SDI observations:", nrow(sdi_clean), "\n\n")

# ============================================================================
# PART 3: Merge All Datasets
# ============================================================================

# Explanation of merging logic:
# 1. Start with GDP data as the base
# 2. Merge NEET and education expenditure data by country code and year
# 3. Merge SDI score data by country code and year
# 4. Finally add continent classification (only by country code, as continent doesn't change over time)
# 5. Use left_join to retain all GDP data, even if other data is missing

master_data <- gdp_clean %>%
  # Merge NEET data
  left_join(
    neet_clean, 
    by = c("code", "year"), 
    suffix = c("_gdp", "_neet")
  ) %>%
  # Merge education expenditure data
  left_join(
    edu_long, 
    by = c("code", "year")
  ) %>%
  # Merge SDI score data
  left_join(
    sdi_clean,
    by = c("code", "year")
  ) %>%
  # Merge continent classification
  left_join(
    continent_clean, 
    by = "code"
  ) %>%
  # Handle duplicate country columns created by merging
  # coalesce selects the first non-NA value
  mutate(country = coalesce(country_gdp, country_neet, country)) %>%
  # Remove duplicate columns with suffixes and reorganize column order
  select(
    country, code, year, continent, 
    gdp_per_capita, neet_share, edu_expenditure_gdp, sdi_score
  ) %>%
  # Keep only observations with at least one piece of data
  filter(!is.na(gdp_per_capita) | !is.na(neet_share) | 
           !is.na(edu_expenditure_gdp) | !is.na(sdi_score)) %>%
  # Keep only data with continent classification (excludes Antarctica and unclassifiable regions)
  filter(!is.na(continent))

cat("=== Master dataset created ===\n")
cat("Total observations:", nrow(master_data), "\n")
cat("Countries:", n_distinct(master_data$code), "\n")
cat("Year range:", min(master_data$year), "to", max(master_data$year), "\n\n")

# ============================================================================
# PART 4: Data Quality Check
# ============================================================================

# View the structure of cleaned data
glimpse(master_data)

# Check data range
cat("\nYear range:", min(master_data$year, na.rm = TRUE), "to", 
    max(master_data$year, na.rm = TRUE), "\n")

# Check data completeness by continent
data_coverage <- master_data %>%
  group_by(continent) %>%
  summarise(
    n_countries = n_distinct(code),
    n_observations = n(),
    gdp_coverage = round(sum(!is.na(gdp_per_capita)) / n() * 100, 1),
    neet_coverage = round(sum(!is.na(neet_share)) / n() * 100, 1),
    edu_coverage = round(sum(!is.na(edu_expenditure_gdp)) / n() * 100, 1),
    sdi_coverage = round(sum(!is.na(sdi_score)) / n() * 100, 1),
    .groups = "drop"
  )

cat("\nData completeness by continent:\n")
print(data_coverage)

# Check for missing values
cat("\nMissing value statistics for each variable:\n")
master_data %>%
  summarise(
    gdp_missing = sum(is.na(gdp_per_capita)),
    neet_missing = sum(is.na(neet_share)),
    edu_missing = sum(is.na(edu_expenditure_gdp)),
    sdi_missing = sum(is.na(sdi_score)),
    total_obs = n()
  ) %>%
  print()

# Check SDI score distribution
cat("\nSDI Score Statistics:\n")
master_data %>%
  filter(!is.na(sdi_score)) %>%
  summarise(
    min_sdi = min(sdi_score),
    q25_sdi = quantile(sdi_score, 0.25),
    median_sdi = median(sdi_score),
    q75_sdi = quantile(sdi_score, 0.75),
    max_sdi = max(sdi_score),
    mean_sdi = mean(sdi_score)
  ) %>%
  print()

# ============================================================================
# PART 5: Save Cleaned Data
# ============================================================================

# Save as CSV file for subsequent analysis
write_csv(master_data, "master_dataset.csv")

cat("\nData cleaning and merging completed!\n")
cat("Cleaned data has been saved as 'master_dataset.csv'\n")

# Display first 10 rows of data
cat("\nPreview of first 10 rows:\n")
head(master_data, 10)

# Display sample with SDI data
cat("\nSample rows with SDI data:\n")
master_data %>%
  filter(!is.na(sdi_score)) %>%
  head(10) %>%
  print()
# End of Data Cleaning and Merging Code
# ============================================================================
