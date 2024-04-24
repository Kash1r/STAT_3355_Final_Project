# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# Read the dataset (assuming it's a CSV file)
data <- read.csv("C:/Users/Kashi/Desktop/Data Analysis/Merged_Data.csv", stringsAsFactors = FALSE)

# Data Preprocessing
data$Arrest.Date <- as.Date(data$Arrest.Date)
data$Arrest.Hour <- hour(hms(data$Arrest.Time))

# Define outdoor areas
outdoor_areas <- c("Outdoor Area Public/Private", "Parking Lot (All Others)",
                   "Parking Lot (Apartment)", "Parking Lot (Park)",
                   "Highway, Street, Alley ETC", "City Park/Rec/Tennis/Golf/Trail",
                   "Apartment Parking Lot", "Condominium/Townhome Parking",
                   "Airport - Love Field", "Motor Vehicle")

# Create a function to abbreviate weather names
abbreviate_weather <- function(weather) {
  case_when(
    weather == "WT01" ~ "Fog",
    weather == "WT02" ~ "Hvy Fog",
    weather == "WT03" ~ "Thunder",
    weather == "WT04" ~ "Ice Pel",
    weather == "WT05" ~ "Hail",
    weather == "WT06" ~ "Glaze",
    weather == "WT07" ~ "Dust",
    weather == "WT08" ~ "Smoke",
    weather == "WT09" ~ "Blow Snow",
    TRUE ~ "None"
  )
}

# Calculate average arrest rate for Noneerse weather
non_adv_data <- data %>%
  filter(Arrest.Location %in% outdoor_areas,
         is.na(WT01) & is.na(WT02) & is.na(WT03) & is.na(WT04) & is.na(WT05) &
           is.na(WT06) & is.na(WT07) & is.na(WT08) & is.na(WT09)) %>%
  group_by(Arrest.Location, Arrest.Date) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Weather = "None",
         Arrest.Rate = Count / 1)  # Assuming each row represents a single day

# Create box plots and perform hypothesis testing for each outdoor area
for (area in outdoor_areas) {
  area_data <- data %>%
    filter(Arrest.Location == area) %>%
    mutate(WT01 = ifelse(WT01 == 1, "WT01", "None"),
           WT02 = ifelse(WT02 == 1, "WT02", "None"),
           WT03 = ifelse(WT03 == 1, "WT03", "None"),
           WT04 = ifelse(WT04 == 1, "WT04", "None"),
           WT05 = ifelse(WT05 == 1, "WT05", "None"),
           WT06 = ifelse(WT06 == 1, "WT06", "None"),
           WT07 = ifelse(WT07 == 1, "WT07", "None"),
           WT08 = ifelse(WT08 == 1, "WT08", "None"),
           WT09 = ifelse(WT09 == 1, "WT09", "None")) %>%
    pivot_longer(cols = starts_with("WT"), names_to = "Weather", values_to = "Condition") %>%
    filter(Condition != "None") %>%
    group_by(Arrest.Location, Weather, Arrest.Date) %>%
    summarise(Count = n(), .groups = "drop") %>%
    mutate(Weather = abbreviate_weather(Weather),
           Arrest.Rate = Count / 1)  # Assuming each row represents a single day
  
  # Combine adverse weather data with Noneerse weather data
  combined_data <- rbind(area_data, non_adv_data %>% filter(Arrest.Location == area))
  
  # Sample sizes
  sample_sizes <- combined_data %>%
    group_by(Weather) %>%
    summarise(SampleSize = n(), .groups = "drop")
  
  # Summary statistics
  summary_stats <- combined_data %>%
    group_by(Weather) %>%
    summarise(
      Mean = mean(Arrest.Rate),
      Median = median(Arrest.Rate),
      SD = sd(Arrest.Rate),
      Min = min(Arrest.Rate),
      Max = max(Arrest.Rate),
      SampleSize = n()  # Include sample size in summary
    )
  
  # Hypothesis testing
  adverse_rates <- combined_data %>% filter(Weather != "None") %>% pull(Arrest.Rate)
  non_adv_rates <- combined_data %>% filter(Weather == "None") %>% pull(Arrest.Rate)
  
  # Check if there are sufficient observations in both groups
  if (length(adverse_rates) > 0 && length(non_adv_rates) > 0) {
    t_test_result <- t.test(adverse_rates, non_adv_rates, alternative = "greater")
    
    # Print results with improved formatting
    cat("Outdoor Area:", area, "\n")
    cat("Summary Statistics:\n")
    print(summary_stats, row.names = FALSE)
    cat("\nSample Sizes by Weather:\n")
    print(sample_sizes, row.names = FALSE)  # Print sample sizes in the desired format
    cat("\nHypothesis Test (Adverse Weather > None):\n")
    cat("t-statistic:", round(t_test_result$statistic, 3), "\n")
    cat("p-value:", round(t_test_result$p.value, 3), "\n")
    cat("Confidence Interval (95%):", round(t_test_result$conf.int[1], 3), "-", round(t_test_result$conf.int[2], 3), "\n")
  } else {
    cat("Outdoor Area:", area, "\n")
    cat("Insufficient data for hypothesis testing.\n")
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
  
  # Create box plot
  plot <- ggplot(combined_data, aes(x = Weather, y = Arrest.Rate, fill = Weather)) +
    geom_boxplot() +
    labs(x = "Weather Condition", y = "Arrest Rate",
         title = paste("Arrest Rate Distribution at", area)) +
    plot_theme +
    scale_fill_discrete(name = "Weather Condition") +
    scale_fill_brewer(name = "Weather Event", palette = "Set1")
  
  print(plot)
  cat("\n\n")
}
