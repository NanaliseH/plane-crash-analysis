# Load necessary libraries
# Uncomment and run if not installed
# install.packages("readr")
# install.packages("tidyverse")
# install.packages("dplyr")
library(readr)
library(tidyverse)  # includes dplyr, ggplot2, and others
library(dplyr)
library(lubridate)  # for date functions
library(ggplot2)

# Step 1: Load the data
crashes <- read_csv("aircraft_crash_data.csv") 

# Step 2: Check structure and column names
glimpse(crashes)
summary(crashes)
names(crashes)

# Step 3: Convert 'acc. date' to Date type
# Initial attempt was with incorrect format "%m/%d/%Y"
# Correct format based on your data: day-month-year with abbreviated month and two-digit year ("%d-%b-%y")
crashes <- crashes %>%
  mutate(Date = as.Date(`acc. date`, format = "%d-%b-%y"))

# Step 4: Extract Year for trend analysis
crashes <- crashes %>%
  mutate(Year = year(Date))

# Step 5: Check for missing key info in important columns
colSums(is.na(crashes[, c("acc. date", "type", "reg.", "operator", "fat.", "location", "dmg")]))

# Step 6: Clean 'fat.' column (fatalities)
# Replace empty strings "" with NA to prevent coercion warnings
crashes <- crashes %>%
  mutate(fat. = na_if(fat., "")) %>%
  mutate(fat. = as.numeric(fat.))

# Step 7: Filter out invalid or missing years (remove unrealistic dates)
crashes <- crashes %>%
  filter(!is.na(Year) & Year >= 1900 & Year <= 2024)

# Step 8: Remove rows with any missing key info if needed
crashes_clean <- crashes[complete.cases(crashes[, c("acc. date", "type", "reg.", "operator", "fat.", "location", "dmg")]), ]

# Step 9: Summarize overall statistics
summary_stats <- crashes_clean %>%
  summarise(
    total_crashes = n(),
    total_fatalities = sum(fat., na.rm = TRUE),
    avg_fatalities_per_crash = mean(fat., na.rm = TRUE)
  )

# Step 10: Crashes and fatalities per year for time trends
crashes_per_year <- crashes_clean %>%
  group_by(Year) %>%
  summarise(
    crashes = n(),
    fatalities = sum(fat., na.rm = TRUE)
  )

# Step 11: Crashes and fatalities by type of aircraft
crashes_by_type <- crashes_clean %>%
  group_by(type) %>%
  summarise(
    crashes = n(),
    fatalities = sum(fat., na.rm = TRUE)
  ) %>%
  arrange(desc(crashes))

# Step 12: Top operators by fatalities
top_operators <- crashes_clean %>%
  group_by(operator) %>%
  summarise(
    crashes = n(),
    fatalities = sum(fat., na.rm = TRUE)
  ) %>%
  arrange(desc(fatalities)) %>%
  head(10)

# Step 13: Plot crashes and fatalities over time
ggplot(crashes_per_year, aes(x = Year)) +
  geom_line(aes(y = crashes), color = "blue") +
  geom_line(aes(y = fatalities), color = "red") +
  labs(title = "Crashes and Fatalities per Year",
       y = "Count",
       x = "Year") +
  theme_minimal()

# Optional: Check summaries and glimpse data after cleaning
head(crashes_per_year)
summary(crashes_per_year)

