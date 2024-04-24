library(tidyverse)
library(lubridate)
library(ggplot2)
library(broom)
library(MASS)  # for logistic regression
library(knitr)  # for displaying sample sizes

# Load the dataset
merged_data <- read.csv("C:/Users/Kashi/Desktop/Data Analysis/Merged_Data.csv", 
                        header = TRUE,
                        stringsAsFactors = FALSE)

# Convert Arrest.Date to date format
merged_data$Arrest.Date <- as.Date(merged_data$Arrest.Date, format = "%Y-%m-%d")

# Create a new column for crime type
merged_data$Crime.Type <- ifelse(merged_data$Drug.Related == TRUE, "Drug Crime",
                                 ifelse(merged_data$Arrest.Weapon != "None", "Weapon Crime", "Non-Weapon Crime"))

# Create binary columns for each weather event, handling missing values
weather_columns <- c("WT01", "WT02", "WT03", "WT04", "WT05", "WT06", "WT07", "WT08", "WT09")
weather_events <- c("Fog", "HeavyFog", "Thunder", "IcePellets", "Hail", "GlazeRime", "Dust", "Smoke", "BlowingSnow")

for (i in seq_along(weather_columns)) {
  column_name <- weather_columns[i]
  event_name <- weather_events[i]
  merged_data[[event_name]] <- ifelse(!is.na(merged_data[[column_name]]) & merged_data[[column_name]] == 1, 1, 0)
}

# Count the number of instances of each weather event
weather_event_counts <- merged_data %>%
  summarise(across(all_of(weather_events), ~sum(.x, na.rm = TRUE)))

# Print the weather event counts
print(weather_event_counts)

# Calculate crime rates
crime_rates <- merged_data %>%
  group_by(Crime.Type, across(all_of(weather_events))) %>%
  summarise(
    TotalCrimes = n(),
    TotalDays = n_distinct(Arrest.Date),
    .groups = "drop"
  ) %>%
  mutate(CrimeRate = TotalCrimes / TotalDays) %>%
  ungroup()

# Add weather event descriptions
crime_rates <- crime_rates %>%
  mutate(
    WeatherEvent = case_when(
      Fog == 1 ~ "Fog",
      HeavyFog == 1 ~ "HeavyFog",
      Thunder == 1 ~ "Thunder",
      IcePellets == 1 ~ "IcePellets",
      Hail == 1 ~ "Hail",
      GlazeRime == 1 ~ "GlazeRime",
      Dust == 1 ~ "Dust",
      Smoke == 1 ~ "Smoke",
      BlowingSnow == 1 ~ "BlowingSnow",
      TRUE ~ "None"
    )
  )

# Calculate overall sample sizes for each weather event
overall_sample_sizes <- merged_data %>%
  summarise(across(all_of(weather_events), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "WeatherEvent", values_to = "SampleSize") %>%
  bind_rows(data.frame(WeatherEvent = "None", SampleSize = nrow(merged_data) - rowSums(merged_data[, weather_events], na.rm = TRUE)))

# Summary statistics and hypothesis testing
summary_stats_and_tests <- list()
for (crime_type in unique(crime_rates$Crime.Type)) {
  subset_data <- crime_rates %>%
    filter(Crime.Type == crime_type)
  
  # Summary statistics by weather event
  summary_stats <- subset_data %>%
    group_by(WeatherEvent) %>%
    summarise(
      Mean = mean(CrimeRate),
      Median = median(CrimeRate),
      SD = sd(CrimeRate),
      Min = min(CrimeRate),
      Max = max(CrimeRate),
      .groups = 'drop'
    )
  
  # Calculate sample sizes for each weather event within the crime type
  sample_sizes <- merged_data %>%
    filter(Crime.Type == crime_type) %>%
    summarise(across(all_of(weather_events), ~sum(.x, na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "WeatherEvent", values_to = "SampleSize") %>%
    bind_rows(data.frame(WeatherEvent = "None", SampleSize = sum(merged_data$Crime.Type == crime_type) - rowSums(merged_data[merged_data$Crime.Type == crime_type, weather_events], na.rm = TRUE)))
  
  test_output <- list(
    Stats = summary_stats,
    SampleSizes = sample_sizes
  )
  
  summary_stats_and_tests[[crime_type]] <- test_output
}

# Customize the plot theme and aesthetics
plot_theme <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0.5),
        plot.caption.position = "plot",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_line(color = "gray90"),
        panel.border = element_rect(color = "gray30", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        plot.margin = margin(10, 20, 10, 20))

# Create a plot for each crime type
plots <- list()
for (crime_type in unique(crime_rates$Crime.Type)) {
  subset_data <- crime_rates %>%
    filter(Crime.Type == crime_type)
  
  # Calculate the average crime rate for each weather event within the crime type
  aggregated_data <- subset_data %>%
    group_by(WeatherEvent) %>%
    summarise(AvgCrimeRate = mean(CrimeRate))
  
  plot_title <- paste("Crime Rates for", crime_type)
  plots[[crime_type]] <- ggplot(aggregated_data, aes(x = WeatherEvent, y = AvgCrimeRate, fill = WeatherEvent)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = plot_title, x = "Weather Event", y = "Arrest Rate") +
    plot_theme +
    scale_fill_discrete(name = "Weather Event") +
    scale_fill_brewer(name = "Weather Event", palette = "Set1")
  
  # Print each plot
  print(plots[[crime_type]])
  cat("\n\n")
}

# Print overall sample sizes
cat("Overall Sample Sizes:\n")
print(overall_sample_sizes)
cat("\n\n")

# Optionally print or examine summary statistics, hypothesis test results, and sample sizes
for (crime_type in names(summary_stats_and_tests)) {
  cat("Crime Type:", crime_type, "\n")
  cat("Summary Statistics:\n")
  print(summary_stats_and_tests[[crime_type]]$Stats)
  
  cat("\nHypothesis Test Results:\n")
  
  # Get crime rates for non-adverse weather conditions (None)
  non_adverse_rates <- subset(crime_rates, Crime.Type == crime_type & WeatherEvent == "None")$CrimeRate
  
  for (weather_event in unique(summary_stats_and_tests[[crime_type]]$Stats$WeatherEvent)) {
    if (weather_event != "None") {
      # Get crime rates for the specific weather event
      event_rates <- subset(crime_rates, Crime.Type == crime_type & WeatherEvent == weather_event)$CrimeRate
      
      if (length(event_rates) >= 2 && length(non_adverse_rates) >= 2) {
        test_result <- t.test(event_rates, non_adverse_rates, alternative = "greater")
        cat("Hypothesis Test (", weather_event, " > None):\n", sep = "")
        cat("t-statistic:", round(test_result$statistic, 3), "\n")
        cat("p-value:", round(test_result$p.value, 3), "\n")
        cat("Confidence Interval (95%):", round(test_result$conf.int[1], 3), "-", ifelse(is.finite(test_result$conf.int[2]), round(test_result$conf.int[2], 3), "Inf"), "\n\n")
      } else {
        cat("Hypothesis Test (", weather_event, " > None):\n", sep = "")
        cat("Insufficient data for hypothesis testing\n\n")
      }
    }
  }
  
  cat("Sample Sizes (Crime Type Specific):\n")
  print(summary_stats_and_tests[[crime_type]]$SampleSizes)
  cat("\n\n")
}