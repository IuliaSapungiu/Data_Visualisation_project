# ==============================================================================
# UK Air Quality Analysis - Research Question 1 (EDA)
# How did daily concentrations of PM₂.₅, PM₁₀, NO₂, and O₃ vary across 
# major UK cities during and after COVID-19 pandemic (2020-2024)?
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP: Load Required Packages
# ------------------------------------------------------------------------------

#install.packages(c("tidyverse", "lubridate", "ggplot2", "patchwork"))

library(tidyverse)    # Data manipulation and visualization
library(lubridate)    # Date handling
library(ggplot2)      # Plotting (also loaded via tidyverse, but explicit here)
library(patchwork)    # Combining multiple plots
library(scales)

# Set theme for all plots
theme_set(theme_minimal(base_size = 12))

# Define consistent color palette (colorblind-friendly)
city_colors <- c(
  "London" = "#fc9003",      # Orange
  "Edinburgh" = "#56B4E9",   # Sky blue
  "Manchester" = "#009E73",  # Green
  "Sheffield" = "#fc035e"    # Vermillion
)

period_colors <- c(
  "Pandemic" = "#0072B2",        # Blue
  "Post-Pandemic" = "#a10b0b"    # Red
)

# Create output directory for plots
dir.create("plots", showWarnings = FALSE)

# Create processed_data directory if it doesn't exist
dir.create("processed_data", showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 2. DATA IMPORT AND CLEANING
# ------------------------------------------------------------------------------

# NOTE: Skip first 10 rows (3 header + 7 metadata rows) to get to column headers
raw_data <- read_csv("uk_air_quality_data.csv", skip = 10, show_col_types = FALSE)

# View structure
glimpse(raw_data)

# Clean and reshape the data
##  Rename first column to Date and convert it into a proper date format
air_quality <- raw_data %>%
  rename(Date = 1) %>%  
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  # Select only pollutant value columns (exclude status/flag columns)
  select(
    Date,
    # Edinburgh
    Edin_O3   = 2, Edin_NO2  = 4, Edin_SO2  = 6, Edin_CO   = 8, 
    Edin_PM10 = 10, Edin_PM25 = 12,
    
    # London
    Lon_O3   = 14, Lon_NO2  = 16, Lon_SO2  = 18, Lon_CO   = 20,
    Lon_PM10 = 22, Lon_PM25 = 24,
    
    # Manchester
    Man_O3   = 26, Man_NO2  = 28, Man_SO2  = 30, 
    Man_PM10 = 32, Man_PM25 = 34,
    
    # Sheffield
    Shef_O3  = 36, Shef_NO2 = 38, 
    Shef_PM10 = 40, Shef_PM25 = 42
  ) %>%
  # Convert all pollutant columns to numeric (handles "No data" as NA)
  mutate(across(-Date, ~as.numeric(as.character(.)))) %>%
  # Filter out rows with invalid dates
  filter(!is.na(Date))

glimpse(air_quality)
colSums(is.na(air_quality))


# Create long format for easier analysis
air_quality_long <- air_quality %>%
  pivot_longer(
    cols = -Date,
    names_to = c("City", "Pollutant"),
    names_sep = "_",
    values_to = "Concentration"
  ) %>%
  # Clean city names
  mutate(
    City = case_when(
      City == "Edin" ~ "Edinburgh",
      City == "Lon" ~ "London",
      City == "Man" ~ "Manchester",
      City == "Shef" ~ "Sheffield"
    ),
    # Clean pollutant names
    Pollutant = case_when(
      Pollutant == "PM25" ~ "PM2.5",
      Pollutant == "PM10" ~ "PM10",
      Pollutant == "NO2" ~ "NO2",
      Pollutant == "O3" ~ "O3",
      Pollutant == "SO2" ~ "SO2",
      Pollutant == "CO" ~ "CO"
    )
  ) %>%
  # Add temporal variables
  mutate(
    Year = year(Date),
    Month = month(Date),
    YearMonth = floor_date(Date, "month"),
    # Define pandemic period: 2020-2021 (Pandemic) vs 2022-2024 (Post-pandemic)
    Period = case_when(
      Year >= 2020 & Year <= 2021 ~ "Pandemic",
      Year >= 2022 ~ "Post-Pandemic"
    ),
    Period = factor(Period, levels = c("Pandemic", "Post-Pandemic"))
  )

# Summary statistics (BEFORE removing NAs)
cat("\n=== DATA SUMMARY ===\n")
cat("Date range:", min(air_quality_long$Date, na.rm = TRUE), "to", max(air_quality_long$Date, na.rm = TRUE), "\n")
cat("Total rows in long format:", nrow(air_quality_long), "\n")
cat("Cities:", paste(unique(air_quality_long$City), collapse = ", "), "\n")
cat("Pollutants:", paste(unique(air_quality_long$Pollutant), collapse = ", "), "\n\n")
colSums(is.na(air_quality_long))

# Check data completeness by city and pollutant (BEFORE removing NAs)
cat("=== DATA COMPLETENESS CHECK ===\n")
data_completeness <- air_quality_long %>%
  group_by(City, Pollutant) %>%
  summarise(
    n_obs = n(),
    n_missing = sum(is.na(Concentration)),
    pct_complete = round((1 - n_missing/n_obs) * 100, 2),
    .groups = "drop"
  )
print(data_completeness)

# Now remove rows with missing concentration values for analysis
air_quality_long <- air_quality_long %>%
  filter(!is.na(Concentration))

cat("\nRows after removing NAs:", nrow(air_quality_long), "\n")


# ------------------------------------------------------------------------------
# 3. PREPARE DATA FOR VISUALIZATIONS
# ------------------------------------------------------------------------------

cat("\n=== PREPARING DATA FOR VISUALIZATIONS ===\n")

# Filter for PM2.5 only (our focus pollutant)
pm25_data <- air_quality_long %>%
  filter(Pollutant == "PM2.5", !is.na(Period))

cat("PM2.5 observations:", nrow(pm25_data), "\n")

# Calculate monthly averages for PM2.5
pm25_monthly <- pm25_data %>%
  group_by(City, YearMonth, Period, Year) %>%
  summarise(
    Monthly_Mean = mean(Concentration, na.rm = TRUE),
    Monthly_SD = sd(Concentration, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  )

write_csv(pm25_monthly, "processed_data/pm25_monthly.csv")

# Calculate period summaries for PM2.5
pm25_period_summary <- pm25_data %>%
  group_by(City, Period) %>%
  summarise(
    Mean = mean(Concentration, na.rm = TRUE),
    Median = median(Concentration, na.rm = TRUE),
    SD = sd(Concentration, na.rm = TRUE),
    SE = sd(Concentration, na.rm = TRUE) / sqrt(n()),
    Q25 = quantile(Concentration, 0.25, na.rm = TRUE),
    Q75 = quantile(Concentration, 0.75, na.rm = TRUE),
    Min = min(Concentration, na.rm = TRUE),
    Max = max(Concentration, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

write_csv(pm25_period_summary, "processed_data/pm25_period_summary.csv")

# ------------------------------------------------------------------------------
# CHART 1: TEMPORAL TRENDS - Monthly PM2.5 Time Series
# Section 2: Theoretical Frameworks
# ------------------------------------------------------------------------------

cat("\n=== Creating Chart 1: Temporal Trends ===\n")

chart1_temporal <- ggplot(pm25_monthly, 
                          aes(x = YearMonth, y = Monthly_Mean, 
                              color = City, group = City)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 2.5, alpha = 0.7) +
  
  # Add vertical line to mark post-pandemic period
  geom_vline(xintercept = as.Date("2022-01-01"), 
             linetype = "dashed", color = "gray30", linewidth = 1) +
  annotate("text", x = as.Date("2022-01-01"), 
           y = max(pm25_monthly$Monthly_Mean, na.rm = TRUE) * 0.95,
           label = "Post-Pandemic Period Begins", 
           angle = 90, vjust = -0.5, size = 3.5, color = "gray30") +
  
  # Styling
  scale_color_manual(values = city_colors) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  
  labs(
    title = "Chart 1: Monthly PM2.5 Concentrations Across UK Cities (2020-2024)",
    subtitle = "Temporal patterns reveal pandemic impact and post-lockdown recovery",
    x = "Month",
    y = expression(paste("Monthly Mean PM"[2.5], " (μg/m³)")),
    color = "City",
    caption = "Data source: UK Air Quality Archive"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    plot.caption = element_text(color = "gray50", size = 9, hjust = 0)
  )

##print in R display
print(chart1_temporal)

ggsave("plots/chart1_temporal_trends.png", chart1_temporal, 
       width = 12, height = 6, dpi = 300)
cat("Saved: plots/chart1_temporal_trends.png\n")

# ------------------------------------------------------------------------------
# CHART 2: PERIOD COMPARISON - Boxplots with Statistical Annotations
# Section 3: Accessibility
# ------------------------------------------------------------------------------

cat("\n=== Creating Chart 2: Period Comparison ===\n")

# Calculate statistical significance
t_test_results <- pm25_data %>%
  group_by(City) %>%
  summarise(
    p_value = t.test(
      Concentration[Period == "Pandemic"],
      Concentration[Period == "Post-Pandemic"]
    )$p.value,
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    .groups = "drop"
  )

# Get y-axis max for positioning significance stars
y_max <- max(pm25_data$Concentration, na.rm = TRUE)

chart2_comparison <- ggplot(pm25_data, 
                            aes(x = City, y = Concentration, fill = Period)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3, outlier.size = 1,
               position = position_dodge(width = 0.8)) +
  
  # Add mean points
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               position = position_dodge(width = 0.8), 
               color = "black", fill = "white", stroke = 1) +
  
  # Add significance annotations
  geom_text(data = t_test_results, 
            aes(x = City, y = y_max * 0.95, label = significance),
            inherit.aes = FALSE, size = 5, fontface = "bold") +
  
  # Styling
  scale_fill_manual(values = period_colors,
                    labels = c("Pandemic\n(2020-2021)", 
                               "Post-Pandemic\n(2022-2024)")) +
  scale_y_continuous(breaks = seq(0, ceiling(y_max/5)*5, 5)) +
  
  labs(
    title = "Chart 2: PM2.5 Distribution Comparison - Pandemic vs Post-Pandemic",
    subtitle = "Boxplots show median (line), mean (diamond), and statistical significance",
    x = "City",
    y = expression(paste("PM"[2.5], " Concentration (μg/m³)")),
    fill = "Period",
    caption = "Statistical significance: *** p<0.001, ** p<0.01, * p<0.05, ns = not significant"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 11, face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(color = "gray50", size = 9, hjust = 0)
  )

print(chart2_comparison)
ggsave("plots/chart2_period_comparison.png", chart2_comparison, 
       width = 10, height = 7, dpi = 300)
cat("Saved: plots/chart2_period_comparison.png\n")

# ------------------------------------------------------------------------------
# CHART 3: MEAN COMPARISON - Simple Bar Chart (Pandemic vs Post-Pandemic)
# Section 4: Visualization Choice
# ------------------------------------------------------------------------------

cat("\n=== Creating Chart 3: Mean Comparison Bar Chart ===\n")

chart3_means <- ggplot(pm25_period_summary, 
                       aes(x = City, y = Mean, fill = Period)) +
  geom_col(position = position_dodge(width = 0.8), alpha = 0.8, width = 0.75) +
  
  # Add error bars (standard error)
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                position = position_dodge(width = 0.8),
                width = 0.3, linewidth = 0.8) +
  
  # Add value labels on bars
  geom_text(aes(label = sprintf("%.1f", Mean)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  
  # Styling
  scale_fill_manual(values = period_colors,
                    labels = c("Pandemic\n(2020-2021)", 
                               "Post-Pandemic\n(2022-2024)")) +
  scale_y_continuous(breaks = seq(0, 25, 5), 
                     limits = c(0, max(pm25_period_summary$Mean) * 1.15),
                     expand = expansion(mult = c(0, 0.05))) +
  
  labs(
    title = "Chart 3: Mean PM2.5 Concentrations - Pandemic vs Post-Pandemic Comparison",
    subtitle = "All cities show elevated PM2.5 levels in the post-pandemic period",
    x = "City",
    y = expression(paste("Mean PM"[2.5], " Concentration (μg/m³)")),
    fill = "Period",
    caption = "Error bars represent standard error of the mean"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 11, face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(color = "gray50", size = 9, hjust = 0)
  )

print(chart3_means)
ggsave("plots/chart3_mean_comparison.png", chart3_means, 
       width = 10, height = 7, dpi = 300)
cat("Saved: plots/chart3_mean_comparison.png\n")

# ------------------------------------------------------------------------------
# CHART 4: POLLUTANT CORRELATION - PM2.5 vs NO2 by Period
# Section 5: Implications and Improvements
# ------------------------------------------------------------------------------

cat("\n=== Creating Chart 4: Pollutant Correlation by Period ===\n")

correlation_data <- air_quality_long %>%
  filter(Pollutant %in% c("PM2.5", "NO2"), !is.na(Period)) %>%
  select(Date, City, Period, Pollutant, Concentration) %>%
  pivot_wider(names_from = Pollutant, values_from = Concentration) %>%
  filter(!is.na(PM2.5) & !is.na(NO2))

cat("Correlation data points:", nrow(correlation_data), "\n")
cat("\nData preparation complete!\n")

# Calculate correlation coefficients for each city-period combination
cor_stats <- correlation_data %>%
  group_by(City, Period) %>%
  summarise(
    cor = cor(PM2.5, NO2, use = "complete.obs"),
    x_pos = max(NO2, na.rm = TRUE) * 0.7,
    y_pos = max(PM2.5, na.rm = TRUE) * 0.95,
    label = paste0("r = ", round(cor, 3)),
    .groups = "drop"
  )

chart4_correlation <- ggplot(correlation_data, 
                             aes(x = NO2, y = PM2.5, color = Period)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.2) +
  
  # Add correlation coefficients
  geom_text(data = cor_stats, 
            aes(x = x_pos, y = y_pos, label = label, color = Period),
            size = 3, fontface = "bold", show.legend = FALSE) +
  
  # Facet by city
  facet_wrap(~City, ncol = 2, scales = "free") +
  
  # Styling
  scale_color_manual(values = period_colors,
                     labels = c("Pandemic\n(2020-2021)", 
                                "Post-Pandemic\n(2022-2024)")) +
  
  labs(
    title = "Chart 4: PM2.5 and NO2 Correlation Analysis by Period",
    subtitle = "Strong positive correlation suggests traffic emissions as key PM2.5 source across both periods",
    x = expression(paste("NO"[2], " Concentration (μg/m³)")),
    y = expression(paste("PM"[2.5], " Concentration (μg/m³)")),
    color = "Period",
    caption = "Linear regression lines with 95% confidence intervals; r = Pearson correlation coefficient"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray95", color = "gray70"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(color = "gray50", size = 9, hjust = 0)
  )

print(chart4_correlation)
ggsave("plots/chart4_pollutant_correlation_by_period.png", chart4_correlation, 
       width = 11, height = 8, dpi = 300)
cat("Saved: plots/chart4_pollutant_correlation_by_period.png\n")

# ------------------------------------------------------------------------------
# COMPOSITE VISUALIZATION - All 4 Charts Combined
# Section 1: Knowledge Building
# ------------------------------------------------------------------------------

cat("\n=== Creating Composite Visualization ===\n")

composite_viz <- (chart1_temporal / (chart2_comparison | chart3_means) / chart4_correlation) +
  plot_annotation(
    title = "UK Air Quality Analysis: PM2.5 Concentrations During and After COVID-19 (2020-2024)",
    subtitle = "Four perspectives reveal pandemic impact, recovery patterns, and pollutant relationships across major UK cities",
    caption = "Data: UK Air Quality Archive | Cities: London, Edinburgh, Manchester, Sheffield",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, color = "gray40", hjust = 0.5),
      plot.caption = element_text(size = 9, color = "gray50")
    )
  ) +
  plot_layout(heights = c(1.2, 1.5, 1.5))

print(composite_viz)
ggsave("plots/composite_visualization_all4.png", composite_viz, 
       width = 16, height = 18, dpi = 300)
cat("Saved: plots/composite_visualization_all4.png\n")

# ------------------------------------------------------------------------------
# SUMMARY STATISTICS FOR REPORT
# ------------------------------------------------------------------------------

cat("\n=== SUMMARY STATISTICS FOR REPORT ===\n")

# Overall statistics
cat("\n--- Overall PM2.5 Statistics by Period ---\n")
print(pm25_period_summary)

# Percentage changes
cat("\n--- Percentage Change (Post-Pandemic vs Pandemic) ---\n")
pct_change <- pm25_period_summary %>%
  select(City, Period, Mean) %>%
  pivot_wider(names_from = Period, values_from = Mean) %>%
  mutate(
    Difference = `Post-Pandemic` - Pandemic,
    Pct_Change = round((Difference / Pandemic) * 100, 2)
  )
print(pct_change)

# Correlation summary by period
cat("\n--- PM2.5 vs NO2 Correlations by City and Period ---\n")
correlations <- correlation_data %>%
  group_by(City, Period) %>%
  summarise(
    Pearson_r = round(cor(PM2.5, NO2, use = "complete.obs"), 3),
    n_observations = n(),
    .groups = "drop"
  )
print(correlations)