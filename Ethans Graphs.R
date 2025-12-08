library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)

master <- read.csv('master_dataset.csv')
view(master)

# Load required library
library(tidyr)

# 4. GDP per capita growth rate (%) over time by continent
cont_master <- master %>% group_by(continent, year) %>%
  mutate(gdp_per_capita = mean(gdp_per_capita, na.rm = T),
    neet_share = mean(neet_share, na.rm = T),
    du_expenditure_gdp = mean(edu_expenditure_gdp, na.rm = T),
  ) %>%
  ungroup() %>% select(year:edu_expenditure_gdp) %>%
  distinct(year, continent, .keep_all = T)
cont_master <- cont_master %>%
  group_by(continent) %>%
  arrange(year) %>%
  mutate(gdp_growth_rate = (gdp_per_capita - lag(gdp_per_capita)) / lag(gdp_per_capita) * 100) %>%
  ungroup()

ggplot(cont_master, aes(year, gdp_growth_rate, colour = continent)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.4),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black", alpha = 0.5) +
  labs(x = 'Year', y = 'GDP per Capita Growth Rate (%)', colour = 'Continent',
       title = 'GDP per Capita Growth Rate by Continent')

# 5. NEET change 2010 → 2020 (bar chart)
neet_change <- master %>%
  filter(year %in% c(2010, 2020)) %>%
  group_by(continent, year) %>%
  summarise(avg_neet = mean(neet_share, na.rm = T), .groups = 'drop') %>%
  pivot_wider(names_from = year, values_from = avg_neet, names_prefix = "year_") %>%
  mutate(neet_change = year_2020 - year_2010) %>%
  filter(!is.na(neet_change))

ggplot(neet_change, aes(x = reorder(continent, neet_change), y = neet_change, fill = continent)) +
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_line(colour = "grey70"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", colour = "black") +
  coord_flip() +
  labs(x = 'Continent', y = 'Change in NEET Share (percentage points)', 
       fill = 'Continent',
       title = 'NEET Share Change from 2010 to 2020 by Continent')

# 6. GDP per capita vs NEET (scatterplot, coloured by continent)
ggplot(master, aes(log10(gdp_per_capita), neet_share, colour = continent)) +
  theme_minimal() + 
  theme(
    panel.grid.major = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.4),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = FALSE, fullrange = TRUE, linewidth = 1.2) +
  labs(x = 'GDP per Capita (log 10)', y = 'NEET Share (%)', 
       colour = 'Continent',
       title = 'Relationship between GDP per Capita and NEET Share')

# 7. Education spending vs NEET (scatterplot)
ggplot(master, aes(edu_expenditure_gdp, neet_share, colour = continent)) +
  theme_minimal() + 
  theme(
    panel.grid.major = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.4),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = FALSE, fullrange = TRUE, linewidth = 1.2) +
  labs(x = 'Education Spending (% of GDP)', y = 'NEET Share (%)', 
       colour = 'Continent',
       title = 'Relationship between Education Spending and NEET Share')

# 8. GDP per capita distribution (boxplot)
ggplot(master, aes(x = continent, y = log10(gdp_per_capita), fill = continent)) +
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.4),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_boxplot(alpha = 0.7, linewidth = 0.8) +
  labs(x = 'Continent', y = 'GDP per Capita (log 10)', 
       fill = 'Continent',
       title = 'Distribution of GDP per Capita by Continent')

# 9. NEET distribution (boxplot)
ggplot(master, aes(x = continent, y = neet_share, fill = continent)) +
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.4),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_boxplot(alpha = 0.7, linewidth = 0.8) +
  labs(x = 'Continent', y = 'NEET Share (%)', 
       fill = 'Continent',
       title = 'Distribution of NEET Share by Continent')

#11: Calculate GDP growth rate for each country, then average by continent
gdp_growth_data <- master %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(gdp_growth_rate = (gdp_per_capita - lag(gdp_per_capita)) / lag(gdp_per_capita) * 100) %>%
  ungroup() %>%
  group_by(continent, year) %>%
  summarise(avg_growth_rate = mean(gdp_growth_rate, na.rm = TRUE), .groups = 'drop')

gdp_growth_data <- gdp_growth_data %>%
  group_by(continent) %>%
  arrange(year) %>%
  mutate(growth_5yr_avg = zoo::rollmean(avg_growth_rate, k = 5, fill = NA, align = "center")) %>%
  ungroup()

ggplot(gdp_growth_data, aes(year, avg_growth_rate, colour = continent)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.4),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_smooth(se = FALSE, linewidth = 1.2, span = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black", alpha = 0.5) +
  labs(x = 'Year', y = 'Average GDP per Capita Growth Rate (%)', colour = 'Continent',
       title = 'GDP per Capita Growth Rate Trends by Continent')

