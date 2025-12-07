# ============================================================================
# TARGET 1: FIGURES FOR REPORT 
# UN SDG 8.1 - GDP Growth in Least Developed Countries
# ============================================================================

# Clear environment
rm(list = ls())

# Load required packages
library(tidyverse)
library(ggplot2)
library(scales)

cat("\n")
cat("============================================================================\n")
cat("              TARGET 1: CREATING FIGURES 3-6 FOR REPORT                    \n")
cat("============================================================================\n\n")

# ============================================================================
# STEP 1: LOAD DATA
# ============================================================================

cat("Step 1: Loading master_dataset.csv...\n")
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

# Auto-detect available GDP data year range
gdp_year_range <- analysis_data %>%
  filter(!is.na(gdp_per_capita)) %>%
  summarise(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )

cat("✓ GDP data year range detected:", gdp_year_range$min_year, "to", gdp_year_range$max_year, "\n")

# Focus on SDG period: 2015 onwards, up to max available year
analysis_period <- analysis_data %>%
  filter(year >= 2015, year <= gdp_year_range$max_year, !is.na(gdp_growth_rate))

# Create year label for figures
year_label <- paste0("2015-", gdp_year_range$max_year)

cat("✓ Data prepared\n")
cat("  Analysis period:", year_label, "\n")
cat("  Observations with growth data:", nrow(analysis_period), "\n\n")

# ============================================================================
# STEP 3: CREATE FIGURES
# ============================================================================

cat("Step 3: Creating figures...\n\n")

# ----------------------------------------------------------------------------
# FIGURE 3: GDP GROWTH TRENDS BY CONTINENT (was Figure 1)
# Population-weighted for Non-LDCs, simple average for LDCs
# ----------------------------------------------------------------------------
cat("Creating Figure 3: GDP Growth Trends by Continent...\n")

growth_trends <- analysis_period %>%
  filter(!is.na(continent), !is.na(is_ldc), !is.na(population)) %>%
  group_by(continent, is_ldc, year) %>%
  summarise(
    # LDCs: Simple average (each country = 1 unit)
    # Non-LDCs: Population-weighted (larger countries have more weight)
    avg_growth = ifelse(
      first(is_ldc),
      mean(gdp_growth_rate, na.rm = TRUE), 
      sum(gdp_growth_rate * population, na.rm = TRUE) / 
        sum(population, na.rm = TRUE)  
    ),
    .groups = "drop"
  ) %>%
  mutate(group_label = ifelse(is_ldc, "LDCs", "Non-LDCs"))

fig3 <- ggplot(growth_trends, aes(x = year, y = avg_growth, color = group_label)) +
  # COVID-19 period highlight (2019-2020)
  annotate("rect", 
           xmin = 2019, xmax = 2020, 
           ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.15) +
  
  # COVID label
  annotate("text", 
           x = 2019.5, y = Inf, 
           label = "COVID-19", 
           color = "gray30", 
           size = 4, 
           vjust = 1.5, 
           fontface = "italic") +
  
  # Main lines (SOLID)
  geom_line(linewidth = 1.4) +
  geom_point(size = 3) +
  
  # Reference lines: 7% target (red dashed) and 2% baseline (blue dashed)
  geom_hline(yintercept = 7, linetype = "dashed", color = "#e74c3c", linewidth = 1.2) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "#3498db", linewidth = 1.2) +
  
  # Labels for reference lines
  annotate("text", x = 2015.5, y = 7.5, label = "7% Target", 
           color = "#e74c3c", size = 4.5, hjust = 0, fontface = "bold") +
  annotate("text", x = 2015.5, y = 2.5, label = "2% Baseline", 
           color = "#3498db", size = 4, hjust = 0, fontface = "bold") +
  
  facet_wrap(~continent, ncol = 3) +
  
  labs(
    title = "Figure 3: GDP Per Capita Growth Rates by Continent",
    subtitle = paste0("LDCs vs Non-LDCs (", year_label, ") | LDCs: simple avg; Non-LDCs: population-weighted"),
    x = "Year", 
    y = "Average GDP Growth Rate (%)",
    color = "Country Group"
  ) +
  
  scale_color_manual(values = c("LDCs" = "#e74c3c", "Non-LDCs" = "#3498db")) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 13),
    strip.text = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 13),
    axis.text.x = element_text(size = 12)
  )

ggsave("figure3_growth_trends.png", fig3, width = 14, height = 10, dpi = 300)
cat("  ✓ Saved: figure3_growth_trends.png\n")
cat("    Year range:", year_label, "\n")
cat("    Method: LDCs (simple avg), Non-LDCs (population-weighted)\n\n")

# ----------------------------------------------------------------------------
# FIGURE 4: GROWTH DISTRIBUTION BY CONTINENT (was Figure 2)
# ----------------------------------------------------------------------------
growth_box <- analysis_period %>%
  filter(!is.na(gdp_growth_rate), !is.na(continent), !is.na(is_ldc)) %>%
  mutate(group_label = ifelse(is_ldc, "LDCs", "Non-LDCs"))

fig4 <- ggplot(growth_box, aes(x = continent, y = gdp_growth_rate, fill = group_label)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.6,
               position = position_dodge(width = 0.7)) +
  
  # Reference lines: 7% target (red dashed) and 2% baseline (blue dashed)
  geom_hline(yintercept = 7, linetype = "dashed", color = "#e74c3c", linewidth = 1.2) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "#3498db", linewidth = 1.2) +
  
  labs(
    title = "Figure 4: Distribution of GDP Growth by Continent",
    subtitle = paste0("LDCs vs Non-LDCs (", year_label, ") | Red dashed: 7% target; Blue dashed: 2% baseline"),
    x = "Continent",
    y = "GDP Growth Rate (%)",
    fill = "Country Group"
  ) +
  scale_fill_manual(values = c("LDCs" = "#e74c3c", "Non-LDCs" = "#3498db")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 13),
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
    axis.text.y = element_text(size = 13),
    strip.text = element_text(face = "bold")
  ) +
  coord_cartesian(ylim = c(-10, 10))   

ggsave("figure4_growth_distribution.png", fig4, width = 10, height = 7, dpi = 300)
cat("  ✓ Saved: figure4_growth_distribution.png\n\n")

# ----------------------------------------------------------------------------
# FIGURE 5: SHARE OF LDCs ACHIEVING 7% TARGET (was Figure 3)
# ----------------------------------------------------------------------------
cat("Creating Figure 5: Share of LDCs Achieving Target...\n")

ldc_count_by_continent <- analysis_period %>%
  filter(is_ldc == TRUE) %>%
  distinct(code, continent) %>%
  group_by(continent) %>%
  summarise(n_ldcs = n_distinct(code), .groups = "drop")

target_achievement <- analysis_period %>%
  filter(!is.na(continent), is_ldc == TRUE) %>%
  group_by(continent) %>%
  summarise(
    total_obs = n(),
    above_7pct = sum(gdp_growth_rate >= 7, na.rm = TRUE),
    pct_achieving = round((above_7pct / total_obs) * 100, 1),
    .groups = "drop"
  ) %>%
  left_join(ldc_count_by_continent, by = "continent")

fig5 <- ggplot(target_achievement, 
               aes(x = reorder(continent, pct_achieving), 
                   y = pct_achieving, 
                   fill = continent)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) +
  
  geom_text(aes(label = paste0(
    round(pct_achieving, 1), "%\n",
    "(", n_ldcs, " LDCs | ", total_obs, " obs)"
  )), hjust = -0.1, size = 5) +
  
  coord_flip() +
  
  labs(
    title = "Figure 5: Share of LDC Country-Years Achieving 7% Growth",
    subtitle = paste0("By Continent (", year_label, ") | 42 of 45 UN LDCs analyzed | Numbers: % (LDC count | obs)"),
    x = NULL, 
    y = "Percentage Achieving ≥7% Growth (%)"
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_brewer(palette = "Set2") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 11),
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 14, face = "bold")
  )

ggsave("figure5_target_achievement.png", fig5, width = 10, height = 6, dpi = 300)
cat("  ✓ Saved: figure5_target_achievement.png\n\n")

# ----------------------------------------------------------------------------
# FIGURE 6: SUSTAINABLE DEVELOPMENT vs ECONOMIC GROWTH (was Figure 4/5)
# ----------------------------------------------------------------------------

cat("Creating Figure 6: Sustainable Development vs Growth...\n")

sdi_growth <- analysis_period %>%
  filter(!is.na(sdi_score), !is.na(continent)) %>%
  mutate(group_label = ifelse(is_ldc, "LDCs", "Non-LDCs"))

fig6 <- ggplot(sdi_growth, aes(x = sdi_score, y = gdp_growth_rate, 
                               color = group_label)) +
  geom_point(alpha = 0.4, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.3) +
  
  # Reference lines: 7% target (red dashed) and 2% baseline (blue dashed)
  geom_hline(yintercept = 7, linetype = "dashed", color = "#e74c3c", linewidth = 1.2) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "#3498db", linewidth = 1.2) +
  
  facet_wrap(~continent, ncol = 3) +
  
  labs(
    title = "Figure 6: Sustainable Development vs Economic Growth",
    subtitle = paste0("SDI Score vs GDP Growth (", year_label, ") | Red dashed: 7% target; Blue dashed: 2% baseline"),
    x = "Sustainable Development Index (Higher = More Sustainable)",
    y = "GDP Growth Rate (%)",
    color = "Country Group"
  ) +
  scale_color_manual(values = c("LDCs" = "#e74c3c", "Non-LDCs" = "#3498db")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 13),
    strip.text = element_text(face = "bold", size = 14),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 12)
  ) +
  coord_cartesian(ylim = c(-10, 10))  

ggsave("figure6_sdi_vs_growth.png", fig6, width = 14, height = 10, dpi = 300)
cat("  ✓ Saved: figure6_sdi_vs_growth.png\n\n")















