library(tidyverse)
library(lubridate)
library(ggplot2)
library(broom)

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

# Calculate crime rates
crime_rates <- merged_data %>%
  group_by(Crime.Type, Drug.Type, Arrest.Weapon, across(all_of(weather_events))) %>%
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
    ),
    EventPresence = case_when(
      Fog == 1 ~ "Present",
      HeavyFog == 1 ~ "Present",
      Thunder == 1 ~ "Present",
      IcePellets == 1 ~ "Present",
      Hail == 1 ~ "Present",
      GlazeRime == 1 ~ "Present",
      Dust == 1 ~ "Present",
      Smoke == 1 ~ "Present",
      BlowingSnow == 1 ~ "Present",
      TRUE ~ "Absent"
    )
  )

# Get unique weapon types
unique_weapons <- unique(crime_rates$Arrest.Weapon)

# Filter for each unique weapon type
filtered_combinations <- crime_rates %>%
  filter(Arrest.Weapon %in% unique_weapons)

# Summary statistics and hypothesis testing
summary_stats_and_tests <- list()
sample_size_list <- list()

for (weapon in unique_weapons) {
  subset_data <- filtered_combinations %>%
    filter(Arrest.Weapon == weapon)
  
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
  
  # Hypothesis testing for the presence of the first weather event vs. its absence
  sample_sizes <- subset_data %>%
    group_by(WeatherEvent) %>%
    summarise(SampleSize = n(), .groups = 'drop')
  
  present_rates <- subset_data %>% filter(EventPresence == "Present") %>% pull(CrimeRate)
  absent_rates <- subset_data %>% filter(EventPresence == "Absent") %>% pull(CrimeRate)
  
  if (length(present_rates) >= 2 && length(absent_rates) >= 2) {
    t_test_result <- t.test(present_rates, absent_rates, alternative = "greater")
    test_output <- paste0(
      "Hypothesis Test (Adverse Weather > Non-Adverse Weather):\n",
      "t-statistic: ", round(t_test_result$statistic, 3), "\n",
      "p-value: ", t_test_result$p.value, "\n",
      "Confidence Interval (95%): ", round(t_test_result$conf.int[1], 3), " - Inf\n"
    )
  } else {
    test_output <- "Insufficient data for hypothesis testing.\n"
  }
  
  summary_stats_and_tests[[weapon]] <- list(Stats = summary_stats, Test = test_output, SampleSizes = sample_sizes)
  sample_size_list[[weapon]] <- sample_sizes
}

# Print sample sizes in a tidy format for each unique weapon type
for (weapon in unique_weapons) {
  cat("Sample Sizes for", weapon, ":\n")
  print(sample_size_list[[weapon]], row.names = FALSE)
  cat("\n")
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

# Loop through each unique weapon type and create a plot
plots <- list()
for (weapon in unique_weapons) {
  subset_data <- filtered_combinations %>%
    filter(Arrest.Weapon == weapon)
  
  # Calculate the average arrest rate for each weather event within the specific combination
  aggregated_data <- subset_data %>%
    group_by(WeatherEvent) %>%
    summarise(AvgCrimeRate = mean(CrimeRate))
  
  plot_title <- paste("Weapon Type:", weapon)
  plots[[weapon]] <- ggplot(aggregated_data, aes(x = WeatherEvent, y = AvgCrimeRate, fill = WeatherEvent)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = plot_title, x = "Weather Event", y = "Arrest Rate") +
    plot_theme +
    scale_fill_brewer(name = "Weather Event", palette = "Set1")
  
  # Print each plot
  print(plots[[weapon]])
  cat("\n\n")
}

# Check if the sum of sample sizes matches the total number of observations
for (weapon in unique_weapons) {
  total_observations <- nrow(filtered_combinations %>%
                               filter(Arrest.Weapon == weapon))
  
  sum_sample_sizes <- sum(sample_size_list[[weapon]]$SampleSize)
  
  if (total_observations != sum_sample_sizes) {
    warning(paste("Sample size mismatch for", weapon))
    cat("Total observations:", total_observations, "\n")
    cat("Sum of sample sizes:", sum_sample_sizes, "\n\n")
  } else {
    cat("Sample sizes match for", weapon, "\n\n")
  }
}

# Verify that the sample sizes match the counts of unique weather events
for (weapon in unique_weapons) {
  subset_data <- filtered_combinations %>%
    filter(Arrest.Weapon == weapon)
  
  unique_events <- subset_data %>%
    group_by(WeatherEvent) %>%
    summarise(EventCount = n(), .groups = 'drop')
  
  if (!all(sample_size_list[[weapon]]$SampleSize == unique_events$EventCount)) {
    warning(paste("Sample size mismatch with unique event counts for", weapon))
    cat("Sample sizes:\n")
    print(sample_size_list[[weapon]], row.names = FALSE)
    cat("\nUnique event counts:\n")
    print(unique_events, row.names = FALSE)
    cat("\n")
  } else {
    cat("Sample sizes match unique event counts for", weapon, "\n\n")
  }
}

# Double-check the sample sizes using an alternative approach
for (weapon in unique_weapons) {
  subset_data <- filtered_combinations %>%
    filter(Arrest.Weapon == weapon)
  
  alternative_sample_sizes <- subset_data %>%
    count(WeatherEvent)
  
  if (!all(sample_size_list[[weapon]]$SampleSize == alternative_sample_sizes$n)) {
    warning(paste("Sample size mismatch using alternative approach for", weapon))
    cat("Sample sizes:\n")
    print(sample_size_list[[weapon]], row.names = FALSE)
    cat("\nAlternative sample sizes:\n")
    print(alternative_sample_sizes, row.names = FALSE)
    cat("\n")
  } else {
    cat("Sample sizes match using alternative approach for", weapon, "\n\n")
  }
}

# Print summary statistics and hypothesis test results
for (weapon in unique_weapons) {
  cat("Weapon:", weapon, "\n\n")
  cat("Summary Statistics:\n")
  print(summary_stats_and_tests[[weapon]]$Stats, row.names = FALSE)
  cat("\nSample Sizes by Weather:\n")
  print(summary_stats_and_tests[[weapon]]$SampleSizes, row.names = FALSE)
  cat("\n")
  cat(summary_stats_and_tests[[weapon]]$Test)
  cat("\n")
}