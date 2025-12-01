# ============================================================================
# TARGET 1: FIGURES FOR REPORT 
# UN SDG 8.1 - GDP Growth in Least Developed Countries
# ============================================================================
#
# INSTRUCTIONS:
# 1. Run the data cleaning script (merge_clean_ids_project.R) first
# 2. This will generate master_dataset.csv in your working directory
# 3. Then run this script to generate all 5 figures
# 4. Figures will be saved in the same directory
#
# ============================================================================

# Clear environment
rm(list = ls())

# Load required packages
library(tidyverse)
library(ggplot2)
library(scales)

cat("\n")
cat("============================================================================\n")
cat("              TARGET 1: CREATING 5 KEY FIGURES FOR REPORT                   \n")
cat("============================================================================\n\n")

# ============================================================================
# STEP 1: LOAD DATA
# ============================================================================

cat("Step 1: Loading master_dataset.csv...\n")

# Check if master_dataset.csv exists
if (!file.exists("master_dataset.csv")) {
  stop("ERROR: master_dataset.csv not found!\n",
       "Please run merge_clean_ids_project.R first to generate the master dataset.\n")
}

# Load master dataset
master_data <- read_csv("master_dataset.csv", show_col_types = FALSE)

cat("✓ Data loaded successfully\n")
cat("  Total observations:", nrow(master_data), "\n")
cat("  Countries:", n_distinct(master_data$code), "\n")
cat("  Year range:", min(master_data$year), "to", max(master_data$year), "\n\n")

# ============================================================================
# STEP 2: PREPARE DATA FOR ANALYSIS
# ============================================================================

cat("Step 2: Calculating GDP growth rates...\n")

# Calculate GDP growth rates
analysis_data <- master_data %>%
  filter(!is.na(gdp_per_capita)) %>%
  arrange(code, year) %>%
  group_by(code) %>%
  mutate(gdp_growth_rate = (gdp_per_capita / lag(gdp_per_capita) - 1) * 100) %>%
  ungroup()

# Focus on SDG period: 2015-2023
analysis_period <- analysis_data %>%
  filter(year >= 2015, year <= 2023, !is.na(gdp_growth_rate))

cat("✓ Data prepared\n")
cat("  Analysis period: 2015-2023\n")
cat("  Observations with growth data:", nrow(analysis_period), "\n\n")

# ============================================================================
# STEP 3: CREATE FIGURES
# ============================================================================

cat("Step 3: Creating figures...\n\n")

# ----------------------------------------------------------------------------
# FIGURE 1: GDP GROWTH TRENDS
# ----------------------------------------------------------------------------

cat("Creating Figure 1: GDP Growth Trends...\n")

growth_trends <- analysis_period %>%
  filter(!is.na(continent), !is.na(is_ldc)) %>%
  group_by(continent, is_ldc, year) %>%
  summarise(avg_growth = mean(gdp_growth_rate, na.rm = TRUE), .groups = "drop") %>%
  mutate(group_label = ifelse(is_ldc, "LDCs", "Non-LDCs"))

fig1 <- ggplot(growth_trends, aes(x = year, y = avg_growth, 
                                  color = group_label, linetype = group_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 7, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 2015.5, y = 7.5, label = "7% Target", 
           color = "red", size = 3.5, hjust = 0) +
  facet_wrap(~continent, ncol = 3) +
  labs(
    title = "Figure 1: GDP Per Capita Growth Rates by Continent",
    subtitle = "LDCs vs Non-LDCs (2015-2023) | Red line indicates 7% annual growth target",
    x = "Year", y = "Average GDP Growth Rate (%)",
    color = "Country Group", linetype = "Country Group"
  ) +
  scale_color_manual(values = c("LDCs" = "#e74c3c", "Non-LDCs" = "#3498db")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10),
        legend.position = "bottom", strip.text = element_text(face = "bold"))

ggsave("figure1_growth_trends.png", fig1, width = 14, height = 10, dpi = 300)
cat("  ✓ Saved: figure1_growth_trends.png\n\n")

# ----------------------------------------------------------------------------
# FIGURE 2: TARGET ACHIEVEMENT
# ----------------------------------------------------------------------------

cat("Creating Figure 2: Target Achievement...\n")

target_achievement <- analysis_period %>%
  filter(!is.na(continent), is_ldc == TRUE) %>%
  group_by(continent) %>%
  summarise(
    total_obs = n(),
    above_7pct = sum(gdp_growth_rate >= 7, na.rm = TRUE),
    pct_achieving = (above_7pct / total_obs) * 100,
    .groups = "drop"
  )

fig2 <- ggplot(target_achievement, aes(x = reorder(continent, pct_achieving), 
                                       y = pct_achieving, fill = continent)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct_achieving, 1), "%\n(", 
                               above_7pct, "/", total_obs, ")")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(
    title = "Figure 2: Percentage of LDCs Achieving 7% Growth Target",
    subtitle = "By Continent (2015-2023) | Numbers show: percentage (observations ≥7% / total)",
    x = NULL, y = "Percentage Achieving ≥7% Growth (%)"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10))

ggsave("figure2_target_achievement.png", fig2, width = 10, height = 6, dpi = 300)
cat("  ✓ Saved: figure2_target_achievement.png\n\n")

# ----------------------------------------------------------------------------
# FIGURE 3: TOP AND BOTTOM PERFORMERS
# ----------------------------------------------------------------------------

cat("Creating Figure 3: Top and Bottom Performers...\n")

ldc_performance <- analysis_period %>%
  filter(is_ldc == TRUE) %>%
  group_by(country, code, continent) %>%
  summarise(avg_growth = mean(gdp_growth_rate, na.rm = TRUE),
            n_years = n(), .groups = "drop") %>%
  filter(n_years >= 3)

top_10 <- ldc_performance %>% arrange(desc(avg_growth)) %>% 
  head(10) %>% mutate(performance = "Top 10 Performers")
bottom_10 <- ldc_performance %>% arrange(avg_growth) %>% 
  head(10) %>% mutate(performance = "Bottom 10 Performers")

performers <- bind_rows(top_10, bottom_10)

fig3 <- ggplot(performers, aes(x = avg_growth, y = reorder(country, avg_growth),
                               fill = continent)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_vline(xintercept = 7, linetype = "dashed", color = "red", linewidth = 1) +
  facet_wrap(~performance, scales = "free_y", ncol = 1) +
  labs(
    title = "Figure 3: Best and Worst Performing LDCs",
    subtitle = "Average GDP Growth Rate (2015-2023) | Red line: 7% target",
    x = "Average Annual GDP Growth Rate (%)", y = NULL,
    fill = "Continent"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10),
        strip.text = element_text(face = "bold", size = 11))

ggsave("figure3_performers.png", fig3, width = 12, height = 10, dpi = 300)
cat("  ✓ Saved: figure3_performers.png\n\n")

# ----------------------------------------------------------------------------
# FIGURE 4: COVID-19 IMPACT
# ----------------------------------------------------------------------------

cat("Creating Figure 4: COVID-19 Impact...\n")

covid_impact <- analysis_period %>%
  filter(is_ldc == TRUE) %>%
  mutate(period = case_when(
    year >= 2015 & year <= 2019 ~ "Pre-COVID\n(2015-2019)",
    year >= 2020 & year <= 2023 ~ "COVID Era\n(2020-2023)"
  )) %>%
  filter(!is.na(period), !is.na(continent)) %>%
  group_by(continent, period) %>%
  summarise(avg_growth = mean(gdp_growth_rate, na.rm = TRUE), .groups = "drop")

fig4 <- ggplot(covid_impact, aes(x = continent, y = avg_growth, fill = period)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_hline(yintercept = 7, linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(aes(label = paste0(round(avg_growth, 1), "%")),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +
  labs(
    title = "Figure 4: Impact of COVID-19 on LDC Growth Rates",
    subtitle = "Comparison before and during pandemic | Red line: 7% target",
    x = "Continent", y = "Average GDP Growth Rate (%)",
    fill = "Period"
  ) +
  scale_fill_manual(values = c("Pre-COVID\n(2015-2019)" = "#3498db", 
                               "COVID Era\n(2020-2023)" = "#e74c3c")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figure4_covid_impact.png", fig4, width = 10, height = 7, dpi = 300)
cat("  ✓ Saved: figure4_covid_impact.png\n\n")

# ----------------------------------------------------------------------------
# FIGURE 5: SDI vs GDP GROWTH
# ----------------------------------------------------------------------------
cat("Creating Figure 5: SDI vs GDP Growth...\n")

sdi_growth <- analysis_period %>%
  filter(!is.na(sdi_score), !is.na(continent)) %>%
  mutate(group_label = ifelse(is_ldc, "LDCs", "Non-LDCs"))

fig5 <- ggplot(sdi_growth, aes(x = sdi_score, y = gdp_growth_rate, 
                               color = group_label)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  geom_hline(yintercept = 7, linetype = "dashed", color = "red", linewidth = 1) +
  facet_wrap(~continent, ncol = 3) +
  labs(
    title = "Figure 5: Sustainable Development vs Economic Growth",
    subtitle = "SDI Score vs GDP Growth Rate (2015-2023) | Red line: 7% target",
    x = "Sustainable Development Index (Higher = More Sustainable)",
    y = "GDP Growth Rate (%)",
    color = "Country Group"
  ) +
  scale_color_manual(values = c("LDCs" = "#e74c3c", "Non-LDCs" = "#3498db")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10),
        legend.position = "bottom", strip.text = element_text(face = "bold")) +
  coord_cartesian(ylim = c(-10, 10))  

ggsave("figure5_sdi_vs_growth.png", fig5, width = 14, height = 10, dpi = 300)
cat("  ✓ Saved: figure5_sdi_vs_growth.png\n\n")

cat("  ✓ Saved: figure5_sdi_vs_growth.png\n\n")



