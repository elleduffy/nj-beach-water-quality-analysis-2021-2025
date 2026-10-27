############################################################
# Multi-Year Analysis of Recreational Water Quality at
# New Jersey Beaches (2021–2025)
#
# Author: Elle Duffy
# Date: Summer 2026
# Version: 1.1
#
# Purpose:
# Explore recreational water quality at New Jersey beaches
# using Enterococcus monitoring data collected between
# 2021 and 2025. This analysis examines overall water
# quality and identifies trends across time, season,
# and beach location.
############################################################

# ==========================================
# Section 1: Load Packages
# ==========================================

library(dplyr)
library(ggplot2)

# ==========================================
# Section 2: Import Data
# ==========================================

# Read NJDEP beach water quality dataset
beach_data <- read.csv("Data/nj_beach_water_quality_2021_2026.csv")

setwd("/Users/elleduffy/Desktop/nj-beach-water-quality-analysis-2021-2026-2")

# Verify successful data import
head(beach_data)
names(beach_data)

# ==========================================
# Section 3: Data Cleaning & Preparation
# ==========================================

# Convert sampling dates to Date format
beach_data$Result_Date <- as.Date(
  beach_data$Result_Date,
  format = "%m/%d/%Y"
)

# Convert Enterococcus measurements to numeric
beach_data$Result_Measure <- as.numeric(beach_data$Result_Measure)

# Remove observations with missing measurements
clean_data <- beach_data %>%
  filter(!is.na(Result_Measure))

# Create variables used throughout the analysis
clean_data <- clean_data %>%
  mutate(
    Year = format(Result_Date, "%Y"),
    Month = factor(
      format(Result_Date, "%m"),
      levels = c("01","02","03","04","05","06",
                 "07","08","09","10","11","12"),
      labels = c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec")
    ),
    unsafe = Result_Measure > 104
  )

# Verify cleaned dataset
summary(clean_data)

# ==========================================
# Section 4: Descriptive Statistics
# ==========================================

# Calculate the total number of water samples
total_samples <- nrow(clean_data)

# Calculate summary statistics for Enterococcus concentrations
mean_bacteria <- mean(clean_data$Result_Measure)
median_bacteria <- median(clean_data$Result_Measure)
min_bacteria <- min(clean_data$Result_Measure)
max_bacteria <- max(clean_data$Result_Measure)
sd_bacteria <- sd(clean_data$Result_Measure)

# Calculate selected quantiles
bacteria_quantiles <- quantile(
  clean_data$Result_Measure,
  probs = c(0.50, 0.75, 0.90, 0.95, 0.99)
)

# Calculate the number and percentage of unsafe samples
unsafe_samples <- sum(clean_data$unsafe)
percent_unsafe <- mean(clean_data$unsafe) * 100

# Display results
cat("=========================================\n")
cat("Descriptive Statistics\n")
cat("=========================================\n")
cat("Total samples:", total_samples, "\n")
cat("Mean Enterococcus:", round(mean_bacteria, 2), "CFU/100 mL\n")
cat("Median Enterococcus:", median_bacteria, "CFU/100 mL\n")
cat("Minimum:", min_bacteria, "CFU/100 mL\n")
cat("Maximum:", max_bacteria, "CFU/100 mL\n")
cat("Standard deviation:", round(sd_bacteria, 2), "\n")
cat("Unsafe samples:", unsafe_samples, "\n")
cat("Percent unsafe:", round(percent_unsafe, 2), "%\n\n")

cat("Selected Quantiles (CFU/100 mL):\n")
print(bacteria_quantiles)

# ==========================================
# Section 5: Annual Trends
# ==========================================

# Summarize Enterococcus data by year
year_summary <- clean_data %>%
  group_by(Year) %>%
  summarise(
    Total_Samples = n(),
    Unsafe_Samples = sum(unsafe),
    Percent_Unsafe = mean(unsafe) * 100,
    Mean_Enterococcus = mean(Result_Measure),
    Median_Enterococcus = median(Result_Measure)
  ) %>%
  ungroup()

# Display yearly summary table
print(year_summary)

# Create yearly trend figure
ggplot(year_summary,
       aes(x = Year,
           y = Percent_Unsafe,
           group = 1)) +
  
  geom_line(
    color = "steelblue",
    linewidth = 1
  ) +
  
  geom_point(
    color = "steelblue",
    size = 3
  ) +
  
  labs(
    title = "Annual Percentage of Unsafe Samples",
    x = "Year",
    y = "Samples Exceeding EPA Standard (%)"
  ) +
  
  theme_classic()

ggsave(
  "Figures/yearly_unsafe_levels.png",
  width = 8,
  height = 5,
  dpi = 300
)

# ==========================================
# Section 6: Seasonal Trends
# ==========================================

# Summarize Enterococcus data by month
monthly_summary <- clean_data %>%
  group_by(Month) %>%
  summarise(
    Total_Samples = n(),
    Unsafe_Samples = sum(unsafe),
    Percent_Unsafe = mean(unsafe) * 100,
    Mean_Enterococcus = mean(Result_Measure, na.rm = TRUE),
    Median_Enterococcus = median(Result_Measure, na.rm = TRUE)
  ) %>%
  ungroup()

# Display monthly summary table
print(monthly_summary)

# Create seasonal trend figure
ggplot(
  monthly_summary,
  aes(
    x = Month,
    y = Percent_Unsafe,
    group = 1
  )
) +
  
  geom_line(
    color = "steelblue",
    linewidth = 1
  ) +
  
  geom_point(
    color = "steelblue",
    size = 3
  ) +
  
  labs(
    title = "Monthly Percentage of Unsafe Samples",
    x = "Month",
    y = "Samples Exceeding EPA Standard (%)"
  ) +
  
  theme_classic()

ggsave(
  "Figures/monthly_unsafe_levels.png",
  width = 8,
  height = 5,
  dpi = 300
)

# ==========================================
# Section 7: Beach Comparisons
# ==========================================

# Summarize Enterococcus data by beach
beach_summary <- clean_data %>%
  group_by(Beach_Name) %>%
  summarise(
    Total_Samples = n(),
    Unsafe_Samples = sum(unsafe),
    Percent_Unsafe = mean(unsafe) * 100,
    Mean_Enterococcus = mean(Result_Measure, na.rm = TRUE),
    Median_Enterococcus = median(Result_Measure, na.rm = TRUE)
  ) %>%
  filter(Total_Samples >= 100) %>%
  arrange(desc(Percent_Unsafe)) %>%
  ungroup()

# Display beach summary table
print(beach_summary)

# Select the top 10 beaches with the highest percentage of unsafe samples
top_beaches <- beach_summary %>%
  slice_head(n = 10)

# Create beach comparison figure
ggplot(
  top_beaches,
  aes(
    x = reorder(Beach_Name, Percent_Unsafe),
    y = Percent_Unsafe
  )
) +
  
  geom_col(fill = "steelblue") +
  
  coord_flip() +
  
  labs(
    title = "Top 10 Beaches by Unsafe Sample Rate",
    x = "Beach",
    y = "Samples Exceeding EPA Standard (%)"
  ) +
  
  theme_classic()

ggsave(
  "Figures/top_10_unsafe_beaches.png",
  width = 8,
  height = 5,
  dpi = 300
)

# ==========================================
# Section 8: Distribution of Enterococcus Levels
# ==========================================

# Create histogram of Enterococcus concentrations
ggplot(clean_data, aes(x = Result_Measure)) +
  
  geom_histogram(
    bins = 50,
    fill = "steelblue",
    color = "white"
  ) +
  
  geom_vline(
    xintercept = 104,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  
  labs(
    title = "Distribution of Enterococcus Concentrations",
    x = "Enterococcus (CFU/100 mL)",
    y = "Number of Samples"
  ) +
  
  coord_cartesian(xlim = c(0, 200)) +
  
  theme_classic()
  
  ggsave(
    "Figures/enterococcus_distribution.png",
    width = 8,
    height = 5,
   dpi = 300
  )
  
  # ==========================================
  # Section 9: Water Quality Over Time
  # ==========================================
  
  ggplot(
    clean_data,
    aes(
      x = Result_Date,
      y = Result_Measure
    )
  ) +
    
    geom_point(
      alpha = 0.25,
      size = 0.5,
      color = "steelblue"
    ) +
    
    geom_hline(
      yintercept = 104,
      color = "red",
      linetype = "dashed",
      linewidth = 1
    ) +
    
    labs(
      title = "Enterococcus Concentrations Over Time",
      x = "Sampling Date",
      y = "Enterococcus (CFU/100 mL)"
    ) +
    
    theme_classic()
  
  ggsave(
    "Figures/enterococcus_over_time.png",
    width = 9,
    height = 5,
    dpi = 300
  )
  
  # ==========================================
  # Section 10: Statistical Analysis
  # ==========================================
  
  # Convert variables to factors
  clean_data$Year <- as.factor(clean_data$Year)
  clean_data$Beach_Name <- as.factor(clean_data$Beach_Name)
  
  # Logistic regression by year
  model_year <- glm(
    unsafe ~ Year,
    family = binomial,
    data = clean_data
  )
  
  summary(model_year)
  
  # Create season categories
  clean_data <- clean_data %>%
    mutate(
      Season = case_when(
        Month %in% c("May", "Jun") ~ "Early Summer",
        Month %in% c("Jul", "Aug") ~ "Peak Summer",
        Month %in% c("Sep") ~ "Late Summer",
        TRUE ~ "Other"
      )
    )
  
  clean_data$Season <- factor(
    clean_data$Season,
    levels = c(
      "Early Summer",
      "Peak Summer",
      "Late Summer",
      "Other"
    )
  )
  
  # Logistic regression by season
  model_season <- glm(
    unsafe ~ Season,
    family = binomial,
    data = clean_data
  )
  
  summary(model_season)
  
  # Logistic regression by beach
  model_beach <- glm(
    unsafe ~ Beach_Name,
    family = binomial,
    data = clean_data
  )
  
  summary(model_beach)
  
  # Calculate odds ratios
  exp(coef(model_year))
  exp(coef(model_season))
  exp(coef(model_beach))
  
# ==========================================
# End of Analysis
# ==========================================
#
# This script completes the full analysis of recreational
# water quality at New Jersey beaches using Enterococcus
# monitoring data collected between 2021 and 2026.
#
# The workflow includes:
#   - Importing and preparing the dataset
#   - Summarizing water quality measurements
#   - Exploring annual and seasonal patterns
#   - Comparing water quality across beaches
#   - Creating publication-ready figures
#   - Evaluating trends using logistic regression
#
# All figures and statistical results presented in the
# accompanying manuscript were generated from this script.
#
# Independent research project by Elle Duffy
# Summer 2026
#
# Repository:
# https://github.com/elleduffy/nj-beach-water-quality-analysis-2021-2026-2
#
# End of script.

  
 -------------------------
    
  
  getwd()
  list.files() 
  list.files("Data")
  data <- read.csv("Data/nj_beach_water_quality_2021_2026.csv")
  names(data)
  str(data)
  head(data)
  
  data$Result_Measure <- as.numeric(data$Result_Measure)
  
  data$Result_Date <- as.Date(data$Result_Date, format = "%m/%d/%Y")
  
  data$Year <- as.numeric(format(data$Result_Date, "%Y"))
  
  data$Month <- as.numeric(format(data$Result_Date, "%m"))
  
  data$Unsafe <- ifelse(data$Result_Measure > 104, 1, 0)
  
  summary(data$Result_Measure)
  table(data$Year)
  table(data$Month)
  table(data$Unsafe)
  id="3c2t8k"
  readLines("beach_water_quality_analysis.R")
  grep(
    "logistic|glm|season|unsafe|regression",
    readLines("beach_water_quality_analysis.R"),
    ignore.case = TRUE,
    value = TRUE
  )
  summary(model_year)
  summary(model_season)
  summary(model_beach)
  lines <- readLines("beach_water_quality_analysis.R")
  
  lines[grep("model_year|model_season|model_beach", lines)]
  lines[which(grepl("model_year <- glm", lines)):which(grepl("exp\\(coef\\(model_beach\\)\\)", lines))]
  clean_data <- data[!is.na(data$Result_Measure), ]
  clean_data$unsafe <- clean_data$Result_Measure > 104
  table(clean_data$unsafe)
  names(clean_data)
  table(clean_data$unsafe)
  table(clean_data$Month)
  
  clean_data$Season <- dplyr::case_when(
    clean_data$Month %in% c(5, 6) ~ "Early Summer",
    clean_data$Month %in% c(7, 8) ~ "Peak Summer",
    clean_data$Month == 9 ~ "Late Summer",
    TRUE ~ "Other"
  )
  
  clean_data$Season <- factor(
    clean_data$Season,
    levels = c("Early Summer", "Peak Summer", "Late Summer", "Other")
  )
  table(clean_data$Season)
  table(clean_data$unsafe)
  table(clean_data$Month)
  table(clean_data$Season)
  clean_data$Season <- droplevels(clean_data$Season)
  levels(clean_data$Season)
  
  
  
  
  model_year <- glm(
    unsafe ~ Year,
    family = binomial,
    data = clean_data
  )
  
  model_season <- glm(
    unsafe ~ Season,
    family = binomial,
    data = clean_data
  )
  
  model_beach <- glm(
    unsafe ~ Beach_Name,
    family = binomial,
    data = clean_data
  )
  
  
  summary(model_year)
  exp(coef(model_year))
  exp(confint(model_year))
  
  
  summary(model_season)
  exp(coef(model_season))
  exp(confint(model_season))
  
  
  summary(model_beach)
  exp(coef(model_beach))
  exp(confint(model_beach))
  
  anova(model_beach, test = "Chisq")
  
  table(clean_data$Beach_Name)
  table(clean_data$Beach_Name, clean_data$unsafe)
  sort(table(clean_data$Beach_Name[clean_data$unsafe]), decreasing = TRUE)
  
  
  
  
  list.files(recursive = TRUE)  
  list.files("Desktop")
  
  list.files("Desktop/nj-beach-water-quality-analysis-2021-2026-2/Data")
  