# Aviation Safety Analysis

## Overview

This project analyzes historical aircraft accident data using R. The goal was to clean, transform, visualize, and summarize aviation accident records to identify trends in crashes and fatalities over time.

The project demonstrates a complete exploratory data analysis (EDA) workflow, including data cleaning, feature engineering, statistical summaries, and data visualization.

## Objectives

* Analyze historical aircraft accident data
* Identify trends in crashes and fatalities over time
* Explore accident patterns across aircraft types
* Examine operators with the highest number of fatalities
* Practice data cleaning and exploratory data analysis techniques

## Dataset

The dataset contains aircraft accident records including:

* Accident date
* Aircraft type
* Registration number
* Operator
* Location
* Damage information
* Fatalities

## Data Preparation

The dataset was cleaned and prepared using several preprocessing steps:

* Converted accident dates into R Date format
* Extracted accident year for trend analysis
* Handled missing values
* Converted fatalities to numeric format
* Removed incomplete or invalid records
* Filtered unrealistic date values

## Analysis Performed

### Time Trend Analysis

* Number of crashes per year
* Number of fatalities per year

### Aircraft Analysis

* Crash frequency by aircraft type
* Fatalities by aircraft type

### Operator Analysis

* Operators with the highest number of fatalities
* Crash frequency by operator

## Visualizations

The project includes visualizations showing:

* Annual crash trends
* Annual fatality trends
* Comparative analysis of crashes and fatalities over time

## Tools Used

* R
* Tidyverse
* dplyr
* ggplot2
* readr
* lubridate

## Skills Demonstrated

* Data Cleaning
* Exploratory Data Analysis (EDA)
* Data Visualization
* Feature Engineering
* Statistical Summarization
* R Programming
* Data Transformation

## Key Findings

The analysis revealed patterns in aviation accidents across different years, aircraft types, and operators. Data visualization helped identify periods with higher accident rates and operators associated with larger numbers of fatalities.

## Future Improvements

* Interactive dashboards using Shiny
* Geographic accident mapping
* Predictive modeling of accident severity
* Time-series forecasting
* Additional aviation safety metrics
