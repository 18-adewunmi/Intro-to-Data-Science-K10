

# Calculate population-weighted NEET for baseline and recent periods by continent
continent_neet_weighted <- country_target_achievement %>%
  filter(!is.na(baseline_neet) & !is.na(recent_neet)) %>%  # Only countries with both baseline and recent data
  group_by(continent) %>%
  summarise(
    # Population-weighted NEET values
    baseline_weighted_neet = sum(baseline_neet * baseline_pop, na.rm = TRUE) / sum(baseline_pop, na.rm = TRUE),
    recent_weighted_neet = sum(recent_neet * recent_pop, na.rm = TRUE) / sum(recent_pop, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    # Calculate percentage change
    weighted_neet_change = ((recent_weighted_neet - baseline_weighted_neet) / baseline_weighted_neet) * 100
  )

# Create bar chart
ggplot(continent_neet_weighted, aes(x = reorder(continent, -weighted_neet_change), 
                                    y = weighted_neet_change, 
                                    fill = weighted_neet_change < 0)) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_text(aes(label = paste0(round(weighted_neet_change, 1), "%")), 
            vjust = ifelse(continent_neet_weighted$weighted_neet_change < 0, 1.5, -0.5),
            fontface = "bold",
            size = 4) +
  scale_fill_manual(
    values = c("TRUE" = "#2166ac", "FALSE" = "#b2182b"), 

  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.4),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  labs(
    x = "Continent",
    y = "Population-Weighted NEET Change (%)",
    fill = "NEET Trend",
    title = "Population-Weighted NEET Change by Continent",
    subtitle = "Baseline: 2010-2015 (last available) vs Recent: 2016-2020 (prioritize 2020)"
  )

# Print the table as well
print("Population-weighted NEET change by continent:")
print(continent_neet_weighted)

# Save to CSV
write_csv(continent_neet_weighted, "continent_population_weighted_neet_change.csv")
